#define NT_PROCESSOR_GROUPS
#include <ntddk.h>
#include <wdm.h>
#include<ntstrsafe.h>
#include "winnvme.h"
#include "nvme.h"
#include "FastMutex.h"
#include "SpinLock.h"


#define	EVENTNAMEMAXLEN	100

#define arraysize( p ) ( sizeof( p ) / sizeof( ( p )[0] ) )

#define DEVICE_NAME					( L"\\Device\\WINNVME" )
#define DEVICE_SYMLINKNAME	( L"\\DosDevices\\WINNVME" )

UINT8 gucDeviceCounter;

LONGLONG io_qid = 1;
//KSPIN_LOCK interrupt_lock;


typedef struct tagWINMEM
{
	PVOID phyAddr;			// physical Address for map
	PVOID pvu;					// user space virtual address for unmap
	ULONG dwSize;				// memory size to map or unmap
	ULONG dwRegOff;		// register offset: 0-255
	ULONG dwBytes;			// bytes to read or write
} WINMEM, * PWINMEM;

//Mapped memory information list
typedef struct tagMAPINFO
{
	LIST_ENTRY	ListEntry;
	PMDL				pMdl;			//allocated mdl
	PVOID				pvk;				//kernel mode virtual address
	PVOID				pvu;				//user mode virtual address
	ULONG			memSize;	//memory size in bytes
} MAPINFO, * PMAPINFO;

LIST_ENTRY winnvme_mmap_linkListHead;
FastMutex winnvme_mmap_locker;

typedef struct _MEMORY {
	LIST_ENTRY					ListEntry;
	ULONG							Length;
	PHYSICAL_ADDRESS	   dmaAddr;
	PVOID								pvk;
	PVOID								pvu;
	PMDL								pMdl;
}MEMORY, *PMEMORY;


typedef struct _DEVICE_EXTENSION
{
	PDEVICE_OBJECT		fdo;
	PDEVICE_OBJECT		PhyDevice;
	PDEVICE_OBJECT		NextStackDevice;
	UNICODE_STRING	ustrDeviceName;    
	UNICODE_STRING	ustrSymLinkName;  
	struct _KINTERRUPT*		InterruptObject;

	BOOLEAN					bInterruptEnable;

	PVOID							bar0;
	SIZE_T							bar_size;

	//PVOID							admin_sq_pvk;
	//PVOID							admin_cq_pvk;
	//PVOID							data_buffer;

	ULONG						vector;

	u32*							admin_sq_doorbell;
	u32*							admin_cq_doorbell;
	int								admin_sq_tail ;
	int								admin_cq_head ;
	int								admin_cq_phase;
	int								admin_sq_size;       ///< queue size
	int								admin_cq_size;       ///< queue size
	nvme_sq_entry_t*	admin_sq_entry;
	nvme_cq_entry_t*	admin_cq_entry;
	MEMORY					admin_sq;
	MEMORY					admin_cq;


	u32*							io_sq_doorbell;
	u32*							io_cq_doorbell;
	int								io_sq_tail;
	int								io_cq_head;
	int								io_cq_phase;
	int								io_sq_size;       ///< queue size
	int								io_cq_size;       ///< queue size
	nvme_sq_entry_t*	io_sq_entry;
	nvme_cq_entry_t*	io_cq_entry;
	MEMORY					io_sq;
	MEMORY					io_cq;

	LIST_ENTRY				winnvme_dma_linkListHead;
	FastMutex					winnvme_dma_locker;

	_DMA_ADAPTER*   dmaAdapter;
	ULONG						NumOfMappedRegister;

	MEMORY					data_buffer[1024];

	HANDLE						adminHandle;
	PKEVENT					adminEvent;

//	HANDLE						ioHandle;
	//PKEVENT					ioEvent;

	UINT8							DeviceCounter;

	LONG							InterruptCount;

	ULONG						IsrType;

	ULONG						MessageId;

	SpinLock						adminq_locker;
	SpinLock						ioq_locker;

} DEVICE_EXTENSION, * PDEVICE_EXTENSION;

void WinNVMeDelay(long long millsecond);

NTSTATUS WinNVMeAddDevice(IN PDRIVER_OBJECT DriverObject, IN PDEVICE_OBJECT PhysicalDeviceObject);
NTSTATUS WinNVMePnp(IN PDEVICE_OBJECT fdo, IN PIRP Irp);
NTSTATUS WinNVMeDeviceControl(IN PDEVICE_OBJECT fdo, IN PIRP Irp);
NTSTATUS WinNVMeDispatchRoutine(IN PDEVICE_OBJECT fdo, IN PIRP Irp);
void WinNVMeUnload(IN PDRIVER_OBJECT DriverObject);

NTSTATUS ReadWriteConfigSpace(IN PDEVICE_OBJECT DeviceObject, IN ULONG ReadOrWrite, // 0 for read 1 for write
													IN PVOID Buffer, IN ULONG Offset, IN ULONG Length );

extern "C"
NTSTATUS DriverEntry(IN PDRIVER_OBJECT pDriverObject, IN PUNICODE_STRING pRegistryPath)
{

	UNREFERENCED_PARAMETER(pRegistryPath);

	pDriverObject->DriverExtension->AddDevice = WinNVMeAddDevice;
	pDriverObject->MajorFunction[IRP_MJ_PNP] = WinNVMePnp;
	pDriverObject->MajorFunction[IRP_MJ_DEVICE_CONTROL] = WinNVMeDeviceControl;
	pDriverObject->MajorFunction[IRP_MJ_CREATE] = WinNVMeDispatchRoutine;
	pDriverObject->MajorFunction[IRP_MJ_CLOSE] = WinNVMeDispatchRoutine;
	pDriverObject->MajorFunction[IRP_MJ_READ] = WinNVMeDispatchRoutine;
	pDriverObject->MajorFunction[IRP_MJ_WRITE] = WinNVMeDispatchRoutine;
	pDriverObject->DriverUnload = WinNVMeUnload;

	InitializeListHead(&winnvme_mmap_linkListHead);
	winnvme_mmap_locker.Init();
	//KeInitializeSpinLock(&interrupt_lock);

	return STATUS_SUCCESS;
}

BOOLEAN
MSI_ISR(
	IN  PKINTERRUPT  Interrupt,
	PVOID  ServiceContext,
	ULONG  MessageId
)
{
	UNREFERENCED_PARAMETER(Interrupt);

	//DbgPrint("Interrupt Occured: %d\n", MessageId);

	PDEVICE_EXTENSION p = (PDEVICE_EXTENSION)ServiceContext;

	p->MessageId = MessageId;
	IoRequestDpc(p->fdo, NULL, p);

	return TRUE;
}

BOOLEAN
FdoInterruptCallback(
	IN  PKINTERRUPT             InterruptObject,
	IN  PVOID                   Context
)
{
	UNREFERENCED_PARAMETER(InterruptObject);
	//UNREFERENCED_PARAMETER(Context);
	PDEVICE_EXTENSION p = (PDEVICE_EXTENSION)Context;
	

	DbgPrint("interrupt\n");
	return TRUE;
}


VOID DPC(
	IN PKDPC Dpc,
	IN PDEVICE_OBJECT DeviceObject,
	IN PIRP irp,
	IN PVOID context
)
{
	UNREFERENCED_PARAMETER(Dpc);
	UNREFERENCED_PARAMETER(DeviceObject);
	UNREFERENCED_PARAMETER(irp);
	//UNREFERENCED_PARAMETER(context);
	PDEVICE_EXTENSION p = (PDEVICE_EXTENSION)context;

	KeSetEvent(p->adminEvent, IO_NO_INCREMENT, FALSE);
	KeClearEvent(p->adminEvent);

	KeStallExecutionProcessor(50);

	KeSetEvent(p->adminEvent, IO_NO_INCREMENT, FALSE);
	KeClearEvent(p->adminEvent);


	if (p->MessageId == 0) {
		//DbgPrint("interrupt\n");
		p->adminq_locker.LockAtDPCLevel();

		nvme_cq_entry_t* admin_cq = (nvme_cq_entry_t*)p->admin_cq.pvk;

		if (admin_cq[p->admin_cq_head].u.a.p == p->admin_cq_phase) {
			while (admin_cq[p->admin_cq_head].u.a.p == p->admin_cq_phase) {
				InterlockedIncrement(&p->InterruptCount);

				//int head = p->admin_cq_head;
				if (++p->admin_cq_head == p->admin_cq_size) {
					p->admin_cq_head = 0;
					p->admin_cq_phase = !p->admin_cq_phase;
				}

				*(volatile u32*)(p->admin_cq_doorbell) = p->admin_cq_head;
			}
		}
		p->adminq_locker.UnLockFromDPCLevel();

	}
	else {

		p->ioq_locker.LockAtDPCLevel();

		nvme_cq_entry_t* io_cq = (nvme_cq_entry_t*)p->io_cq.pvk;

		if (io_cq[p->io_cq_head].u.a.p == p->io_cq_phase) {
			while (io_cq[p->io_cq_head].u.a.p == p->io_cq_phase) {
				InterlockedIncrement(&p->InterruptCount);

				//int head = p->admin_cq_head;
				if (++p->io_cq_head == p->io_cq_size) {
					p->io_cq_head = 0;
					p->io_cq_phase = !p->io_cq_phase;
				}

				*(volatile u32*)(p->io_cq_doorbell) = p->io_cq_head;
			}
		}
		p->ioq_locker.UnLockFromDPCLevel();

	}
	DbgPrint("interrupt occured: %d\n", p->InterruptCount);


	return;
}


NTSTATUS WinNVMeAddDevice(IN PDRIVER_OBJECT DriverObject, IN PDEVICE_OBJECT PhysicalDeviceObject)
{
	NTSTATUS			status;
	PDEVICE_OBJECT		fdo;
	PDEVICE_EXTENSION	pdx;
	UNICODE_STRING		devName;
	UNICODE_STRING		symLinkName;

	//DECLARE_UNICODE_STRING_SIZE(devName, 64);
	//DECLARE_UNICODE_STRING_SIZE(symLinkName, 64);

	wchar_t  devNameReal[64] = { 0 };
	wchar_t  symLinkNameReal[64] = { 0 };
	swprintf(devNameReal, L"%s%d", DEVICE_NAME, gucDeviceCounter);
	DbgPrint("%ls\n",devNameReal);
	RtlInitUnicodeString(&devName, devNameReal);
	//RtlUnicodeStringPrintf(&devName, L"\\Device\\WINMEM%d", gucDeviceCounter);

	DbgPrint("Add Device\n");
	status = IoCreateDevice(DriverObject, sizeof(DEVICE_EXTENSION), &devName, FILE_DEVICE_UNKNOWN, 0, FALSE, &fdo);
	if (!NT_SUCCESS(status))
	{
		DbgPrint("Failure IoCreateDevice\n");
		return status;
	}
	pdx = (PDEVICE_EXTENSION)fdo->DeviceExtension;
	pdx->fdo = fdo;
	pdx->PhyDevice = PhysicalDeviceObject;
	pdx->DeviceCounter = gucDeviceCounter;

	pdx->adminHandle = nullptr;
	//pdx->ioHandle = nullptr;

	InitializeListHead(&pdx->winnvme_dma_linkListHead);
	pdx->winnvme_dma_locker.Init();

	pdx->adminq_locker.Init();
	pdx->ioq_locker.Init();

	IoInitializeDpcRequest(fdo, DPC);
	//pdx->NextStackDevice = IoAttachDeviceToDeviceStack(fdo, PhysicalDeviceObject);
	status = IoAttachDeviceToDeviceStackSafe(fdo, PhysicalDeviceObject, &pdx->NextStackDevice);
	if (!NT_SUCCESS(status))
	{
		DbgPrint("Failure IoAttachDeviceToDeviceStackSafe");
		return status;
	}

	DEVICE_DESCRIPTION DeviceDescription;
	RtlZeroMemory(&DeviceDescription, sizeof(DEVICE_DESCRIPTION));

#if 0
	DeviceDescription.Version = DEVICE_DESCRIPTION_VERSION;
	DeviceDescription.Master = TRUE;
	DeviceDescription.ScatterGather = TRUE;
	DeviceDescription.Dma32BitAddresses = TRUE;
	DeviceDescription.Dma64BitAddresses = TRUE;
	DeviceDescription.InterfaceType = PCIBus;
	DeviceDescription.MaximumLength = 0x10000000;
#else
	DeviceDescription.Version = DEVICE_DESCRIPTION_VERSION3;
	DeviceDescription.Master = TRUE;
	DeviceDescription.ScatterGather = TRUE;
	DeviceDescription.IgnoreCount = TRUE;
	DeviceDescription.DmaChannel = 0;
	DeviceDescription.Dma32BitAddresses = TRUE;
	DeviceDescription.Dma64BitAddresses = TRUE;
	DeviceDescription.InterfaceType = PCIBus;
	DeviceDescription.MaximumLength = 0x10000000;   /*  256M */
	DeviceDescription.DmaAddressWidth = 32;
#endif

	pdx->dmaAdapter = IoGetDmaAdapter(pdx->PhyDevice, &DeviceDescription, &pdx->NumOfMappedRegister);

	if (!pdx->dmaAdapter) {
		DbgPrint("Failure IoGetDmaAdapter\n");

		if (pdx->NextStackDevice)
		{
			IoDetachDevice(pdx->NextStackDevice);
		}

		IoDeleteDevice(pdx->fdo);

		return STATUS_INSUFFICIENT_RESOURCES;
	}

	//RtlUnicodeStringPrintf(&symLinkName, L"\\DosDevices\\WINMEM_%d", gucDeviceCounter);

	swprintf(symLinkNameReal, L"%s_%d", DEVICE_SYMLINKNAME, gucDeviceCounter);
	DbgPrint("%ls\n", symLinkNameReal);
	RtlInitUnicodeString(&symLinkName, symLinkNameReal);
	//RtlInitUnicodeString(&symLinkName, DEVICE_SYMLINKNAME);

	pdx->admin_sq_tail = 0;
	pdx->admin_cq_head = 0;
	pdx->admin_cq_phase = 1;
	pdx->admin_sq_doorbell = nullptr;
	pdx->admin_cq_doorbell = nullptr;

	pdx->io_sq_tail = 0;
	pdx->io_cq_head = 0;
	pdx->io_cq_phase = 1;
	pdx->io_sq_doorbell = nullptr;
	pdx->io_cq_doorbell = nullptr;

	//pdx->sq = nullptr;
	//pdx->cq = nullptr;

	//pdx->data_buffer = nullptr;

	//pdx->admin_cq_pvk = nullptr;
	//pdx->admin_sq_pvk = nullptr;

	pdx->bar0 = nullptr;

	pdx->bInterruptEnable = FALSE;
	pdx->ustrDeviceName = devName;
	pdx->ustrSymLinkName = symLinkName;
	status = IoCreateSymbolicLink(&symLinkName, &devName); 

	if (!NT_SUCCESS(status))
	{
		DbgPrint("Failure IoCreateSymbolicLink\n");

		IoDeleteSymbolicLink(&pdx->ustrSymLinkName);
		status = IoCreateSymbolicLink(&symLinkName, &devName);
		if (!NT_SUCCESS(status))
		{
			DbgPrint("Failure IoCreateSymbolicLink\n");
			return status;
		}
	}

	fdo->Flags |= DO_BUFFERED_IO | DO_POWER_PAGABLE;
	fdo->Flags &= ~DO_DEVICE_INITIALIZING;

	DbgPrint("Success DriverEntry\n");

	gucDeviceCounter++;
	return STATUS_SUCCESS;
}

NTSTATUS DefaultPnpHandler(PDEVICE_EXTENSION pdx, PIRP Irp)
{
	IoSkipCurrentIrpStackLocation(Irp);
	return IoCallDriver(pdx->NextStackDevice, Irp);
}

NTSTATUS OnRequestComplete(PDEVICE_OBJECT junk, PIRP Irp, PKEVENT pev)
{
	UNREFERENCED_PARAMETER(junk);
	UNREFERENCED_PARAMETER(Irp);

	KeSetEvent(pev, 0, FALSE);
	return STATUS_MORE_PROCESSING_REQUIRED;
}


NTSTATUS ForwardAndWait(PDEVICE_EXTENSION pdx, PIRP Irp)
{       
	KEVENT event;

	KeInitializeEvent(&event, NotificationEvent, FALSE);
	IoCopyCurrentIrpStackLocationToNext(Irp);
	IoSetCompletionRoutine(Irp, (PIO_COMPLETION_ROUTINE)OnRequestComplete, (PVOID) &event, TRUE, TRUE, TRUE);

	IoCallDriver(pdx->NextStackDevice, Irp);
	KeWaitForSingleObject(&event, Executive, KernelMode, FALSE, NULL);
	return Irp->IoStatus.Status;
}                                                       


VOID ShowResources(IN PCM_PARTIAL_RESOURCE_LIST list, IN PDEVICE_EXTENSION pdx)
{                                                       
	ULONG							i;
	ULONG							nres = list->Count; 
	//NTSTATUS			status;

	PCM_PARTIAL_RESOURCE_DESCRIPTOR resource = list->PartialDescriptors;

	IO_CONNECT_INTERRUPT_PARAMETERS     Connect;
	IO_DISCONNECT_INTERRUPT_PARAMETERS  Disconnect;

	//UNREFERENCED_PARAMETER(pdx);
	PIO_INTERRUPT_MESSAGE_INFO  p;
	PIO_INTERRUPT_MESSAGE_INFO_ENTRY pp;
	NTSTATUS status;

	for (i = 0; i < nres; ++i, ++resource)
	{                                                   
		ULONG		type = resource->Type;

		static char* name[] = {
			"CmResourceTypeNull",
			"CmResourceTypePort",
			"CmResourceTypeInterrupt",
			"CmResourceTypeMemory",
			"CmResourceTypeDma",
			"CmResourceTypeDeviceSpecific",
			"CmResourceTypeBusNumber",
			"CmResourceTypeDevicePrivate",
			"CmResourceTypeAssignedResource",
			"CmResourceTypeSubAllocateFrom",
		};

		DbgPrint("type=%d, typeName=%s \n", type, type < arraysize(name) ? name[type] : "unknown");

		switch (type)
		{   // select on resource type
		case CmResourceTypePort:
		case CmResourceTypeMemory:
			pdx->bar_size = resource->u.Port.Length;
			pdx->bar0 = MmMapIoSpace(resource->u.Port.Start, resource->u.Port.Length, MmNonCached);
			//DbgPrint("bar0  kernel virtual address:  %p", pdx->bar0);

			//DbgPrint("CmResourceTypeMemory ===> start 0x%lX 0x%lX length:%d\n",
			//	resource->u.Port.Start.HighPart,
			//	resource->u.Port.Start.LowPart,
			//	resource->u.Port.Length);
			break;
		case CmResourceTypeBusNumber:
			//DbgPrint("CmResourceTypeBusNumber:::");
			break;
		case CmResourceTypeInterrupt:

#if  0
			RtlZeroMemory(&Connect, sizeof(IO_CONNECT_INTERRUPT_PARAMETERS));
			RtlZeroMemory(&Disconnect, sizeof(IO_DISCONNECT_INTERRUPT_PARAMETERS));

			//Connect.Version = CONNECT_FULLY_SPECIFIED_GROUP;
			Connect.Version = CONNECT_FULLY_SPECIFIED;
			Connect.FullySpecified.PhysicalDeviceObject = pdx->PhyDevice;
		
			Connect.FullySpecified.InterruptObject = &pdx->InterruptObject;
			Connect.FullySpecified.ServiceRoutine = FdoInterruptCallback;
			Connect.FullySpecified.ServiceContext = pdx;

			Connect.FullySpecified.FloatingSave = FALSE;
			Connect.FullySpecified.SpinLock = NULL;


			if (resource->Flags & CM_RESOURCE_INTERRUPT_MESSAGE) {
				Connect.FullySpecified.Vector = resource->u.MessageInterrupt.Translated.Vector;
				Connect.FullySpecified.Irql = (KIRQL)resource->u.MessageInterrupt.Translated.Level;
				Connect.FullySpecified.SynchronizeIrql = (KIRQL)resource->u.MessageInterrupt.Translated.Level;
				//Connect.FullySpecified.Group = resource->u.MessageInterrupt.Translated.Group;
				Connect.FullySpecified.ProcessorEnableMask = resource->u.MessageInterrupt.Translated.Affinity;
			}
			else {
				Connect.FullySpecified.Vector = resource->u.Interrupt.Vector;
				Connect.FullySpecified.Irql = (KIRQL)resource->u.Interrupt.Level;
				Connect.FullySpecified.SynchronizeIrql = (KIRQL)resource->u.Interrupt.Level;
		     	//Connect.FullySpecified.Group = resource->u.Interrupt.Group;
				Connect.FullySpecified.ProcessorEnableMask = resource->u.Interrupt.Affinity;
			}

			//Connect.Version = (Connect.FullySpecified.Group != 0) ? CONNECT_FULLY_SPECIFIED_GROUP : CONNECT_FULLY_SPECIFIED;
			Connect.FullySpecified.InterruptMode == (resource->Flags & CM_RESOURCE_INTERRUPT_LATCHED) ? Latched : LevelSensitive;
			Connect.FullySpecified.ShareVector = (BOOLEAN)(resource->ShareDisposition == CmResourceShareShared);
			
			status = IoConnectInterruptEx(&Connect);

			if (NT_SUCCESS(status)) {
				DbgPrint("Success IoConnectInterruptEx");
				pdx->IsrType = Connect.Version;
				pdx->bInterruptEnable = TRUE;
				p = (PIO_INTERRUPT_MESSAGE_INFO)pdx->InterruptObject;
				pp = p->MessageInfo;
				DbgPrint("interrupt version: %d", Connect.Version);

				for (i = 0; i < p->MessageCount; ++i) {
					DbgPrint("IoConnectInterruptEx params ===> Irql:%X, Vector:%X, Proc:%llX, MessageData:%lX, MessageAddress:%lX\n",
						(pp + i)->Irql,
						(pp + i)->Vector,
						(pp + i)->TargetProcessorSet,
						(pp + i)->MessageData,
						(pp + i)->MessageAddress.LowPart
					);
				}

				//Disconnect.Version = Connect.Version;
				//Disconnect.ConnectionContext.InterruptObject = pdx->InterruptObject;
				//IoDisconnectInterruptEx(&Disconnect);
			}
			else {
				DbgPrint("Failure  IoConnectInterruptEx:   %x", status);

			}

			//}
			//else {
			//	Connect.FullySpecified.Vector = resource->u.Interrupt.Vector;
			//	Connect.FullySpecified.Irql = (KIRQL)resource->u.Interrupt.Level;
			//	Connect.FullySpecified.SynchronizeIrql = (KIRQL)resource->u.Interrupt.Level;
			//	Connect.FullySpecified.Group = resource->u.Interrupt.Group;
			//	Connect.FullySpecified.ProcessorEnableMask = resource->u.Interrupt.Affinity;
			
			//}

			//if (resource->Flags & CM_RESOURCE_INTERRUPT_MESSAGE) {
			//	DbgPrint("CM_RESOURCE_INTERRUPT_MESSAGE\n");
			//}

			//DbgPrint("resource flag: %x\n", resource->Flags);
		//	DbgPrint("CmResourceTypeInterrupt   Translated ===> level:%X, vector:%X, affinity:%llX\n",
			//	resource->u.MessageInterrupt.Translated.Level,
			//	resource->u.MessageInterrupt.Translated.Vector,
			//	resource->u.MessageInterrupt.Translated.Affinity);

			//DbgPrint("CmResourceTypeInterrupt   Raw ===> level:%X, vector:%X, affinity:%llX\n",
			//	resource->u.MessageInterrupt.Translated.Level,
			//	resource->u.MessageInterrupt.Translated.Vector,
			//	resource->u.MessageInterrupt.Translated.Affinity);

#endif

			//if (resource->u.MessageInterrupt.Translated.Vector <= 128) {
			//	pdx->vector = resource->u.MessageInterrupt.Translated.Vector;
			//}

			break;

		case CmResourceTypeDma:
			DbgPrint("CmResourceTypeDma ===> channel %d, port %X\n", resource->u.Dma.Channel, resource->u.Dma.Port);
		} // select on resource type

	}    // for each resource
}       // ShowResources

BOOLEAN OnInterrupt(PKINTERRUPT InterruptObject, PDEVICE_EXTENSION pdx)
{   
	UNREFERENCED_PARAMETER(InterruptObject);
	UNREFERENCED_PARAMETER(pdx);
	
	return TRUE;
}

NTSTATUS HandleStartDevice(PDEVICE_EXTENSION pdx, PIRP Irp)
{
	NTSTATUS						status;
	PIO_STACK_LOCATION				stack;
	PCM_PARTIAL_RESOURCE_LIST		translated;
	PCM_FULL_RESOURCE_DESCRIPTOR	pfrd;
	IO_CONNECT_INTERRUPT_PARAMETERS     Connect;
	PIO_INTERRUPT_MESSAGE_INFO  p;
	PIO_INTERRUPT_MESSAGE_INFO_ENTRY pp;
	UINT16     command_reg;
	int i;
	status = ForwardAndWait(pdx, Irp);        

	if (!NT_SUCCESS(status))                
	{
		Irp->IoStatus.Status = status;
		IoCompleteRequest(Irp, IO_NO_INCREMENT);
		return status;
	}

	stack = IoGetCurrentIrpStackLocation(Irp);

	// Check bus Master
	status = ReadWriteConfigSpace(pdx->fdo, 0, &command_reg, 4, 2);
	DbgPrint("Command Register: 0x%X\n", command_reg);


	if (stack->Parameters.StartDevice.AllocatedResourcesTranslated)
	{
		translated = &stack->Parameters.StartDevice.AllocatedResourcesTranslated->List[0].PartialResourceList;
		pfrd = &stack->Parameters.StartDevice.AllocatedResourcesTranslated->List[0];
	}
	else
	{
		translated = NULL;
	}
	
#if 0
	status = ReadWriteConfigSpace(pdx->fdo, 0, &buffer, 0, 256);
	UINT8 next = buffer[0x34];
	DbgPrint("Next Pointer: 0x%X", next);

#if 1
	while (1) {
		DbgPrint("Cap ID: 0x%X", buffer[next]);
		next = buffer[next + 1];

		if (++i > 10 || next == 0) break;
	}
#endif


	//for ( i = 0; i <100; i=i+16) {
	//	DbgPrint("%x,%x,%x,%x,%x,%x,%x,%x,%x,%x,%x,%x,%x,%x,%x,%x", 
	//		pci_config.DeviceSpecific[i + 0], pci_config.DeviceSpecific[i+1], pci_config.DeviceSpecific[i + 2], pci_config.DeviceSpecific[i + 3],
	//		pci_config.DeviceSpecific[i + 4], pci_config.DeviceSpecific[i + 5],pci_config.DeviceSpecific[i + 6],pci_config.DeviceSpecific[i + 7],
	//		pci_config.DeviceSpecific[i + 8], pci_config.DeviceSpecific[i + 9], pci_config.DeviceSpecific[i + 10], pci_config.DeviceSpecific[i + 11],
	//		pci_config.DeviceSpecific[i + 12], pci_config.DeviceSpecific[i + 13], pci_config.DeviceSpecific[i + 14], pci_config.DeviceSpecific[i + 15]
	//	);
	//}
#endif

	// Show resource from PNP Manager
	ShowResources( translated, pdx);





#if 1
	{
		UNICODE_STRING name;
		UNICODE_STRING eventbase;
		UNICODE_STRING eventname;
		STRING eventnameString;

		char cEventName[EVENTNAMEMAXLEN] = { 0 };

		sprintf(cEventName, "%s%d", "admin", pdx->DeviceCounter);

		RtlInitUnicodeString(&eventbase, L"\\BaseNamedObjects\\");

		name.MaximumLength = EVENTNAMEMAXLEN + eventbase.Length;
		name.Length = 0;
		name.Buffer = (PWCH)ExAllocatePool(NonPagedPool, name.MaximumLength);
		RtlZeroMemory(name.Buffer, name.MaximumLength);

		RtlInitString(&eventnameString, cEventName);
		RtlAnsiStringToUnicodeString(&eventname, &eventnameString, TRUE);

		RtlCopyUnicodeString(&name, &eventbase);
		RtlAppendUnicodeStringToString(&name, &eventname);
		RtlFreeUnicodeString(&eventname);

		pdx->adminEvent = IoCreateNotificationEvent(&name, &pdx->adminHandle);

		ExFreePool(name.Buffer);

		if (!pdx->adminEvent) {
			status = STATUS_UNSUCCESSFUL;
			Irp->IoStatus.Status = status;
			IoCompleteRequest(Irp, IO_NO_INCREMENT);
			return status;
		}

		KeClearEvent(pdx->adminEvent);
	}
#endif

#if 0

	{
		UNICODE_STRING name;
		UNICODE_STRING eventbase;
		UNICODE_STRING eventname;
		STRING eventnameString;

		char cEventName[EVENTNAMEMAXLEN] = { 0 };

		sprintf(cEventName, "%s%d", "io", pdx->DeviceCounter);

		RtlInitUnicodeString(&eventbase, L"\\BaseNamedObjects\\");

		name.MaximumLength = EVENTNAMEMAXLEN + eventbase.Length;
		name.Length = 0;
		name.Buffer = (PWCH)ExAllocatePool(NonPagedPool, name.MaximumLength);
		RtlZeroMemory(name.Buffer, name.MaximumLength);

		RtlInitString(&eventnameString, cEventName);
		RtlAnsiStringToUnicodeString(&eventname, &eventnameString, TRUE);

		RtlCopyUnicodeString(&name, &eventbase);
		RtlAppendUnicodeStringToString(&name, &eventname);
		RtlFreeUnicodeString(&eventname);

		pdx->ioEvent = IoCreateNotificationEvent(&name, &pdx->ioHandle);

		ExFreePool(name.Buffer);

		if (!pdx->ioEvent) {
			status = STATUS_UNSUCCESSFUL;
			Irp->IoStatus.Status = status;
			IoCompleteRequest(Irp, IO_NO_INCREMENT);
			return status;
		}

		KeClearEvent(pdx->ioEvent);
	}

#endif



#if 1

	// Check bus Master
	status = ReadWriteConfigSpace(pdx->fdo, 0, &command_reg, 4, 2);

	DbgPrint("Command Register: 0x%X\n", command_reg);

	nvme_controller_reg_t* ctrl_reg = (nvme_controller_reg_t*)(pdx ->bar0)  ;

	nvme_controller_cap_t cap	= { 0 };
	nvme_adminq_attr_t	aqa		= { 0 };
	nvme_controller_config_t cc = { 0 };

	cap.val = ctrl_reg->cap.val;

	// wait controller disable
	ctrl_reg->cc.a.en = 0;

	while (ctrl_reg->csts.rdy == 1) {
		DbgPrint("Waiting  controller disable\n");
		WinNVMeDelay(1);
	}


	pdx->admin_cq_size = 1024;
	pdx->admin_sq_size = 1024;

	pdx->io_cq_size = 64;
	pdx->io_sq_size = 64;

	/* Allocate DMA */
	if (pdx->dmaAdapter) {
#if 0
		pdx->admin_cq.pvk = pdx->dmaAdapter->DmaOperations->AllocateCommonBuffer(
			pdx->dmaAdapter,
			65536,
			&pdx->admin_cq.dmaAddr,
			FALSE
		);
#else
		pdx->admin_cq.pvk = pdx->dmaAdapter->DmaOperations->AllocateCommonBufferEx(
			pdx->dmaAdapter,
			nullptr,
			65536,
			&pdx->admin_cq.dmaAddr,
			FALSE,
			KeGetCurrentNodeNumber()
		);

#endif

	}


	if (pdx->dmaAdapter) {
#if 0
		pdx->admin_sq.pvk = pdx->dmaAdapter->DmaOperations->AllocateCommonBuffer(
			pdx->dmaAdapter,
			65536,
			&pdx->admin_sq.dmaAddr,
			FALSE
		);

#else 
		pdx->admin_sq.pvk = pdx->dmaAdapter->DmaOperations->AllocateCommonBufferEx(
			pdx->dmaAdapter,
			nullptr,
			65536,
			&pdx->admin_sq.dmaAddr,
			FALSE,
			KeGetCurrentNodeNumber()
		);
#endif
	}



	if (pdx->dmaAdapter) {
#if 0
		pdx->io_cq.pvk = pdx->dmaAdapter->DmaOperations->AllocateCommonBuffer(
			pdx->dmaAdapter,
			4096,
			&pdx->io_cq.dmaAddr,
			FALSE
		);
#else
		pdx->io_cq.pvk = pdx->dmaAdapter->DmaOperations->AllocateCommonBufferEx(
			pdx->dmaAdapter,
			nullptr,
			4096,
			&pdx->io_cq.dmaAddr,
			FALSE,
			KeGetCurrentNodeNumber()
		);

#endif

	}


	if (pdx->dmaAdapter) {
#if 0
		pdx->io_sq.pvk = pdx->dmaAdapter->DmaOperations->AllocateCommonBuffer(
			pdx->dmaAdapter,
			4096,
			&pdx->io_sq.dmaAddr,
			FALSE
		);
#else
		pdx->io_sq.pvk = pdx->dmaAdapter->DmaOperations->AllocateCommonBufferEx(
			pdx->dmaAdapter,
			nullptr,
			4096,
			&pdx->io_sq.dmaAddr,
			FALSE,
			KeGetCurrentNodeNumber()
		);
#endif
	}



	if (pdx->dmaAdapter) {
#if 0
		pdx->data_buffer.pvk = pdx->dmaAdapter->DmaOperations->AllocateCommonBuffer(
			pdx->dmaAdapter,
			4096,
			&pdx->data_buffer.dmaAddr,
			FALSE
		);
#else
		for (int i = 0; i < 1024; ++i) {
			pdx->data_buffer[i].pvk = pdx->dmaAdapter->DmaOperations->AllocateCommonBufferEx(
				pdx->dmaAdapter,
				nullptr,
				4096,
				&pdx->data_buffer[i].dmaAddr,
				FALSE,
				KeGetCurrentNodeNumber()
			);
		}

#endif
	}



	
	pdx->admin_cq_entry = (nvme_cq_entry_t*)pdx->admin_cq.pvk;
	RtlZeroMemory(pdx->admin_cq_entry, sizeof(nvme_cq_entry_t) * pdx->admin_cq_size);

	pdx->admin_sq_entry = (nvme_sq_entry_t*)pdx->admin_sq.pvk;
	RtlZeroMemory(pdx->admin_sq_entry, sizeof(nvme_sq_entry_t) * pdx->admin_sq_size);

	aqa.a.acqs = pdx->admin_cq_size - 1;
	 aqa.a.asqs = pdx->admin_sq_size - 1;
	 ctrl_reg->aqa.val = aqa.val;

	 //ctrl_reg->asq = sq_phyaddr.QuadPart;
	 //ctrl_reg->acq = cq_phyaddr.QuadPart;

	 ctrl_reg->acq = pdx->admin_cq.dmaAddr.QuadPart;;
	 ctrl_reg->asq = pdx->admin_sq.dmaAddr.QuadPart;


	 ctrl_reg->aqa.val = aqa.val;

	pdx ->admin_sq_doorbell = ctrl_reg->sq0tdbl;                                                                         // 0
	pdx ->admin_cq_doorbell = ctrl_reg->sq0tdbl + ((LONGLONG)1 << cap.a.dstrd);                // 1

	pdx->io_sq_doorbell = ctrl_reg->sq0tdbl +((LONGLONG)1 << cap.a.dstrd) * (io_qid + 1);
	pdx->io_cq_doorbell = ctrl_reg->sq0tdbl + ((LONGLONG)1 << cap.a.dstrd) * (io_qid + 2);
	//DbgPrint("sq dbl   :%p\n", pdx->admin_sq_doorbell);
	//DbgPrint("cq dbl   :%p\n", pdx->admin_cq_doorbell);

	cc.val = NVME_CC_CSS_NVM;
	cc.val |= 0 << NVME_CC_MPS_SHIFT;
	cc.val |= NVME_CC_AMS_RR | NVME_CC_SHN_NONE;
	cc.val |= NVME_CC_IOSQES | NVME_CC_IOCQES;
	cc.a.en = 1;

	ctrl_reg->cc.val = cc.val;

	while (ctrl_reg->csts.rdy == 0) {
		DbgPrint("Waiting  controller ready\n");
		WinNVMeDelay(1);
	}



#endif



	/* Setting  Interrupt */
#if 1
	RtlZeroMemory(&Connect, sizeof(IO_CONNECT_INTERRUPT_PARAMETERS));
	Connect.Version = CONNECT_MESSAGE_BASED;
	Connect.MessageBased.ConnectionContext.InterruptObject = &pdx->InterruptObject;
	Connect.MessageBased.PhysicalDeviceObject = pdx->PhyDevice;
	Connect.MessageBased.FloatingSave = FALSE;
	//Connect.MessageBased.SpinLock = &interrupt_lock;
	Connect.MessageBased.SpinLock = nullptr;
	Connect.MessageBased.MessageServiceRoutine = MSI_ISR;
	Connect.MessageBased.SynchronizeIrql = 0;
	Connect.MessageBased.ServiceContext = pdx;
	Connect.MessageBased.FallBackServiceRoutine = FdoInterruptCallback;

	status = IoConnectInterruptEx(&Connect);

	if (NT_SUCCESS(status)) {
		DbgPrint("Success IoConnectInterruptEx\n");
		pdx->IsrType = Connect.Version;
		pdx->bInterruptEnable = TRUE;
		p = (PIO_INTERRUPT_MESSAGE_INFO)pdx->InterruptObject;
		pp = p->MessageInfo;
		DbgPrint("interrupt version: %d", Connect.Version);

		for (int i = 0; i < (int)p->MessageCount; ++i) {
			DbgPrint("IoConnectInterruptEx params ===> Irql:%X, Vector:%X, Proc:%llX, MessageData:%lX, MessageAddress:%lX\n",
				(pp + i)->Irql,
				(pp + i)->Vector,
				(pp + i)->TargetProcessorSet,
				(pp + i)->MessageData,
				(pp + i)->MessageAddress.LowPart
			);
		}
	}
	else {
		ZwClose(pdx->adminHandle);
		pdx->adminHandle = nullptr;
		//	ZwClose(pdx->ioHandle);
		//	pdx->ioHandle = nullptr;
		Irp->IoStatus.Status = status;
		IoCompleteRequest(Irp, IO_NO_INCREMENT);
		return status;

	}

#endif









#if 0
	for (int i = 0; i < 10; ++i) {

		int cid = pdx->admin_sq_tail;

		if (pdx->admin_sq_entry != nullptr) {
			pdx->admin_sq_entry[cid].get_log_page.opcode = nvme_admin_get_log_page;
			pdx->admin_sq_entry[cid].get_log_page.command_id = (u16)cid;
			pdx->admin_sq_entry[cid].get_log_page.nsid = 0xffffffff;
			pdx->admin_sq_entry[cid].get_log_page.dptr.prp1 = pdx->data_buffer.dmaAddr.QuadPart;
			pdx->admin_sq_entry[cid].get_log_page.lid = 2;
			pdx->admin_sq_entry[cid].get_log_page.numdl = (4096 / sizeof(u32) - 1) & 0xff;
			pdx->admin_sq_entry[cid].get_log_page.numdu = ((4096 / sizeof(u32) - 1) >> 16) & 0xff;
		}

		if (++pdx->admin_sq_tail == pdx->admin_sq_size) pdx->admin_sq_tail = 0;
		*(volatile u32*)pdx->admin_sq_doorbell = pdx->admin_sq_tail;

	}
#endif

	Irp->IoStatus.Status = STATUS_SUCCESS;
	IoCompleteRequest(Irp, IO_NO_INCREMENT);

	return status;
}





NTSTATUS HandleRemoveDevice(PDEVICE_EXTENSION pdx, PIRP Irp)
{
	NTSTATUS status;
	IO_DISCONNECT_INTERRUPT_PARAMETERS  Disconnect;

	Irp->IoStatus.Status = STATUS_SUCCESS;
	status = DefaultPnpHandler(pdx, Irp);

	if (pdx->bInterruptEnable) {
		DbgPrint("IoDisconnectInterruptEx\n");

		RtlZeroMemory(&Disconnect, sizeof(IO_DISCONNECT_INTERRUPT_PARAMETERS));

		Disconnect.Version = pdx->IsrType;
		Disconnect.ConnectionContext.InterruptObject = (PKINTERRUPT)pdx->InterruptObject;
		IoDisconnectInterruptEx(&Disconnect);
	}

	while (!IsListEmpty(&pdx->winnvme_dma_linkListHead))
	{
		DbgPrint("unalloc dma \n");

		PLIST_ENTRY pEntry = RemoveTailList(&pdx->winnvme_dma_linkListHead);
		PMEMORY pDmaMem = CONTAINING_RECORD(pEntry, MEMORY, ListEntry);
		//DbgPrint("Map physical 0x%p to virtual 0x%p, size %u\n", pMapInfo->pvk, pMapInfo->pvu , pMapInfo->memSize );

		MmUnmapLockedPages(pDmaMem->pvu, pDmaMem->pMdl);
		IoFreeMdl(pDmaMem->pMdl);
		pdx->dmaAdapter->DmaOperations->FreeCommonBuffer(
			pdx->dmaAdapter,
			pDmaMem->Length,
			pDmaMem->dmaAddr,
			pDmaMem->pvk,
			FALSE
		);

		ExFreePool(pDmaMem);

	}

#if 0
	{
		PSINGLE_LIST_ENTRY pLink = PopEntryList(&pdx->lstMapInfo);

		while (pLink)
		{
			PMAPINFO pMapInfo = CONTAINING_RECORD(pLink, MAPINFO, link);

			MmUnmapLockedPages(pMapInfo->pvu, pMapInfo->pMdl);
			IoFreeMdl(pMapInfo->pMdl);
			MmUnmapIoSpace(pMapInfo->pvk, pMapInfo->memSize);

			ExFreePool(pMapInfo);

			pLink = PopEntryList(&pdx->lstMapInfo);
		}
	}

	{
		PSINGLE_LIST_ENTRY pLink = PopEntryList(&pdx->lstDMAMapInfo);

		while (pLink)
		{
			PMEMORY pDmaMapInfo = CONTAINING_RECORD(pLink, MEMORY, link);

			MmUnmapLockedPages(pDmaMapInfo->pvu, pDmaMapInfo->pMdl);
			IoFreeMdl(pDmaMapInfo->pMdl);

			pdx->dmaAdapter->DmaOperations->FreeCommonBuffer(
				pdx->dmaAdapter,
				pDmaMapInfo->Length,
				pDmaMapInfo->dmaAddr,
				pDmaMapInfo->pvk,
				TRUE
			);

			ExFreePool(pDmaMapInfo);

			pLink = PopEntryList(&pdx->lstDMAMapInfo);
		}
	}

#endif

#if 1

	if (pdx->io_sq.pvk) {
		pdx->dmaAdapter->DmaOperations->FreeCommonBuffer(
			pdx->dmaAdapter,
			4096,
			pdx->io_sq.dmaAddr,
			pdx->io_sq.pvk,
			FALSE
		);
	}

	if (pdx->io_cq.pvk) {
		pdx->dmaAdapter->DmaOperations->FreeCommonBuffer(
			pdx->dmaAdapter,
			4096,
			pdx->io_cq.dmaAddr,
			pdx->io_cq.pvk,
			FALSE
		);
	}

	if (pdx->admin_sq.pvk) {
		pdx->dmaAdapter->DmaOperations->FreeCommonBuffer(
			pdx->dmaAdapter,
			65536,
			pdx->admin_sq.dmaAddr,
			pdx->admin_sq.pvk,
			FALSE
		);
	}

	if (pdx->admin_cq.pvk) {
		pdx->dmaAdapter->DmaOperations->FreeCommonBuffer(
			pdx->dmaAdapter,
			65536,
			pdx->admin_cq.dmaAddr,
			pdx->admin_cq.pvk,
			FALSE
		);
	}

	for (int i = 0; i < 1024; ++i) {
		if (pdx->data_buffer[i].pvk) {
			pdx->dmaAdapter->DmaOperations->FreeCommonBuffer(
				pdx->dmaAdapter,
				4096,
				pdx->data_buffer[i].dmaAddr,
				pdx->data_buffer[i].pvk,
				FALSE
			);
		}
	}


#endif

	if (pdx->dmaAdapter != nullptr) {
		pdx->dmaAdapter->DmaOperations->PutDmaAdapter(pdx->dmaAdapter);

	}


#if 1
	if (pdx->adminHandle) {
		DbgPrint("ZwClose\n");

		ZwClose(pdx->adminHandle);
		pdx->adminHandle = nullptr;
	}

	//if (pdx->ioHandle) {
	//	DbgPrint("ZwClose\n");

	//	ZwClose(pdx->ioHandle);
	//	pdx->ioHandle = nullptr;
	//}

#endif

	if (pdx->bar0) {
		MmUnmapIoSpace(pdx ->bar0 , pdx->bar_size);
		DbgPrint("MmUnmapIoSpace\n");
	}

	DbgPrint("Remove Device\n");
	IoDeleteSymbolicLink(&pdx->ustrSymLinkName);

	if (pdx->NextStackDevice)
	{
		IoDetachDevice(pdx->NextStackDevice);
	}

	IoDeleteDevice(pdx->fdo);

	return status;
}


NTSTATUS WinNVMePnp(IN PDEVICE_OBJECT fdo, IN PIRP Irp)
{
	NTSTATUS			status = STATUS_SUCCESS;
	PDEVICE_EXTENSION	pdx = (PDEVICE_EXTENSION)fdo->DeviceExtension;
	PIO_STACK_LOCATION	stack = IoGetCurrentIrpStackLocation(Irp);
	static NTSTATUS(*fcntab[])(PDEVICE_EXTENSION pdx, PIRP Irp) =
	{
		HandleStartDevice,  // IRP_MN_START_DEVICE
		DefaultPnpHandler,  // IRP_MN_QUERY_REMOVE_DEVICE
		HandleRemoveDevice, // IRP_MN_REMOVE_DEVICE
		DefaultPnpHandler,  // IRP_MN_CANCEL_REMOVE_DEVICE
		DefaultPnpHandler,  // IRP_MN_STOP_DEVICE
		DefaultPnpHandler,  // IRP_MN_QUERY_STOP_DEVICE
		DefaultPnpHandler,  // IRP_MN_CANCEL_STOP_DEVICE
		DefaultPnpHandler,  // IRP_MN_QUERY_DEVICE_RELATIONS
		DefaultPnpHandler,  // IRP_MN_QUERY_INTERFACE
		DefaultPnpHandler,  // IRP_MN_QUERY_CAPABILITIES
		DefaultPnpHandler,  // IRP_MN_QUERY_RESOURCES
		DefaultPnpHandler,  // IRP_MN_QUERY_RESOURCE_REQUIREMENTS
		DefaultPnpHandler,  // IRP_MN_QUERY_DEVICE_TEXT
		DefaultPnpHandler,  // IRP_MN_FILTER_RESOURCE_REQUIREMENTS
		DefaultPnpHandler,  //
		DefaultPnpHandler,  // IRP_MN_READ_CONFIG
		DefaultPnpHandler,  // IRP_MN_WRITE_CONFIG
		DefaultPnpHandler,  // IRP_MN_EJECT
		DefaultPnpHandler,  // IRP_MN_SET_LOCK
		DefaultPnpHandler,  // IRP_MN_QUERY_ID
		DefaultPnpHandler,  // IRP_MN_QUERY_PNP_DEVICE_STATE
		DefaultPnpHandler,  // IRP_MN_QUERY_BUS_INFORMATION
		DefaultPnpHandler,  // IRP_MN_DEVICE_USAGE_NOTIFICATION
		DefaultPnpHandler,  // IRP_MN_SURPRISE_REMOVAL
	};
	static char* fcnname[] =
	{
		"IRP_MN_START_DEVICE",
		"IRP_MN_QUERY_REMOVE_DEVICE",
		"IRP_MN_REMOVE_DEVICE",
		"IRP_MN_CANCEL_REMOVE_DEVICE",
		"IRP_MN_STOP_DEVICE",
		"IRP_MN_QUERY_STOP_DEVICE",
		"IRP_MN_CANCEL_STOP_DEVICE",
		"IRP_MN_QUERY_DEVICE_RELATIONS",
		"IRP_MN_QUERY_INTERFACE",
		"IRP_MN_QUERY_CAPABILITIES",
		"IRP_MN_QUERY_RESOURCES",
		"IRP_MN_QUERY_RESOURCE_REQUIREMENTS",
		"IRP_MN_QUERY_DEVICE_TEXT",
		"IRP_MN_FILTER_RESOURCE_REQUIREMENTS",
		"",
		"IRP_MN_READ_CONFIG",
		"IRP_MN_WRITE_CONFIG",
		"IRP_MN_EJECT",
		"IRP_MN_SET_LOCK",
		"IRP_MN_QUERY_ID",
		"IRP_MN_QUERY_PNP_DEVICE_STATE",
		"IRP_MN_QUERY_BUS_INFORMATION",
		"IRP_MN_DEVICE_USAGE_NOTIFICATION",
		"IRP_MN_SURPRISE_REMOVAL",
	};
	ULONG				fcn = stack->MinorFunction;

	if (fcn >= arraysize(fcntab))
	{   
		status = DefaultPnpHandler(pdx, Irp);
		return status;
	}

	status = (*fcntab[fcn])(pdx, Irp);

	return status;
}

NTSTATUS WinNVMeDispatchRoutine(IN PDEVICE_OBJECT fdo, IN PIRP Irp)
{
	UNREFERENCED_PARAMETER(fdo);
	Irp->IoStatus.Status = STATUS_SUCCESS;
	Irp->IoStatus.Information = 0; 
	IoCompleteRequest(Irp, IO_NO_INCREMENT);

	return STATUS_SUCCESS;
}

NTSTATUS ReadWriteConfigSpace(
	IN PDEVICE_OBJECT DeviceObject,
	IN ULONG ReadOrWrite, // 0 for read 1 for write
	IN PVOID Buffer,
	IN ULONG Offset,
	IN ULONG Length
)
{
	KEVENT				event;
	NTSTATUS			status;
	PIRP				irp;
	IO_STATUS_BLOCK		ioStatusBlock;
	PIO_STACK_LOCATION	irpStack;
	PDEVICE_OBJECT		targetObject;

	//PAGED_CODE();

	KeInitializeEvent(&event, NotificationEvent, FALSE);

	targetObject = IoGetAttachedDeviceReference(DeviceObject);

	irp = IoBuildSynchronousFsdRequest(IRP_MJ_PNP, targetObject, NULL, 0, NULL, &event, &ioStatusBlock);

	if (irp == NULL)
	{
		status = STATUS_INSUFFICIENT_RESOURCES;
		goto End;
	}

	irpStack = IoGetNextIrpStackLocation(irp);

	if (ReadOrWrite == 0)
	{
		irpStack->MinorFunction = IRP_MN_READ_CONFIG;
	}
	else
	{
		irpStack->MinorFunction = IRP_MN_WRITE_CONFIG;
	}

	irpStack->Parameters.ReadWriteConfig.WhichSpace = PCI_WHICHSPACE_CONFIG;
	irpStack->Parameters.ReadWriteConfig.Buffer = Buffer;
	irpStack->Parameters.ReadWriteConfig.Offset = Offset;
	irpStack->Parameters.ReadWriteConfig.Length = Length;

	//
	// Initialize the status to error in case the bus driver does not
	// set it correctly.
	//

	irp->IoStatus.Status = STATUS_NOT_SUPPORTED;

	status = IoCallDriver(targetObject, irp);

	if (status == STATUS_PENDING)
	{
		KeWaitForSingleObject(&event, Executive, KernelMode, FALSE, NULL);
		status = ioStatusBlock.Status;
	}

End:
	// Done with reference
	ObDereferenceObject(targetObject);

	return status;
}


NTSTATUS WinNVMeDeviceControl(IN PDEVICE_OBJECT fdo, IN PIRP irp)
{
	//DbgPrint("Enter WinNVMeDeviceControl\n");

	NTSTATUS			status;
	PIO_STACK_LOCATION irpStack;
	PDEVICE_EXTENSION	pdx;
	ULONG dwIoCtlCode;

	int cid;

	pdx = (PDEVICE_EXTENSION)fdo->DeviceExtension;

	irp->IoStatus.Status = STATUS_SUCCESS;
	irp->IoStatus.Information = 0;
	irpStack = IoGetCurrentIrpStackLocation(irp);

	PVOID pSysBuf = (PVOID)irp->AssociatedIrp.SystemBuffer;
	PWINMEM pMem = (PWINMEM)pSysBuf;

	ULONG dwInBufLen = irpStack->Parameters.DeviceIoControl.InputBufferLength;
	ULONG dwOutBufLen = irpStack->Parameters.DeviceIoControl.OutputBufferLength;

	switch (irpStack->MajorFunction)
	{
	
	case IRP_MJ_DEVICE_CONTROL:

		dwIoCtlCode = irpStack->Parameters.DeviceIoControl.IoControlCode;

		switch (dwIoCtlCode) {
		case IOCTL_WINNVME_MAP:

			if (dwInBufLen == sizeof(WINMEM) && dwOutBufLen == sizeof(PVOID))
			{
				PHYSICAL_ADDRESS phyAddr;
				PVOID pvk, pvu;

				phyAddr.QuadPart = (ULONGLONG)pMem->phyAddr;

				//get mapped kernel address
				pvk = MmMapIoSpace(phyAddr, pMem->dwSize, MmNonCached);

				if (pvk)
				{
					//allocate mdl for the mapped kernel address
					PMDL pMdl = IoAllocateMdl(pvk, pMem->dwSize, FALSE, FALSE, NULL);
					if (pMdl)
					{
						PMAPINFO pMapInfo;

						//build mdl and map to user space
						MmBuildMdlForNonPagedPool(pMdl);

						//pvu = MmMapLockedPages(pMdl, UserMode);
						pvu = MmMapLockedPagesSpecifyCache(pMdl, UserMode, MmNonCached, NULL, FALSE, NormalPagePriority);

						if (pvu) {
							//insert mapped infomation to list
							pMapInfo = (PMAPINFO)ExAllocatePool(NonPagedPool, sizeof(MAPINFO));
							pMapInfo->pMdl = pMdl;
							pMapInfo->pvk = pvk;
							pMapInfo->pvu = pvu;
							pMapInfo->memSize = pMem->dwSize;

							winnvme_mmap_locker.Lock();
							InsertHeadList(&winnvme_mmap_linkListHead, &pMapInfo->ListEntry);
							winnvme_mmap_locker.UnLock();

							//PushEntryList(&pdx->lstMapInfo, &pMapInfo->link);
							//ExInterlockedPushEntryList(&lstMapInfo, &pMapInfo->link, &singlelist_spinLock);
							//DbgPrint("Map kernel virtual addr: 0x%p , user virtual addr: 0x%p, size %u\n", pvk, pvu, pMem->dwSize);

							RtlCopyMemory(pSysBuf, &pvu, sizeof(PVOID));
							irp->IoStatus.Information = sizeof(PVOID);

						}
						else {
							IoFreeMdl(pMdl);
							MmUnmapIoSpace(pvk, pMem->dwSize);
							irp->IoStatus.Status = STATUS_INSUFFICIENT_RESOURCES;
						}
					}
					else
					{
						//allocate mdl error, unmap the mapped physical memory
						MmUnmapIoSpace(pvk, pMem->dwSize);
						irp->IoStatus.Status = STATUS_INSUFFICIENT_RESOURCES;
					}
				}
				else
					irp->IoStatus.Status = STATUS_INSUFFICIENT_RESOURCES;

			}
			else
				irp->IoStatus.Status = STATUS_INVALID_PARAMETER;

			break;

		case IOCTL_WINNVME_UNMAP:

			//DbgPrint("IOCTL_WINMEM_UNMAP\n");

			if (dwInBufLen == sizeof(WINMEM))
			{
				PMAPINFO pMapInfo;
				PLIST_ENTRY pLink;

				//initialize to head
				pLink = winnvme_mmap_linkListHead.Flink;

				while (pLink)
				{
					pMapInfo = CONTAINING_RECORD(pLink, MAPINFO, ListEntry);

					if (pMapInfo->pvu == pMem->pvu)
					{
						if (pMapInfo->memSize == pMem->dwSize)
						{
							//free mdl, unmap mapped memory
							MmUnmapLockedPages(pMapInfo->pvu, pMapInfo->pMdl);
							IoFreeMdl(pMapInfo->pMdl);
							MmUnmapIoSpace(pMapInfo->pvk, pMapInfo->memSize);

							//DbgPrint("Unmap user virtual address 0x%p, size %u\n", pMapInfo->pvu, pMapInfo->memSize);
							//delete matched element from the list
							//if (pLink == pdx->lstMapInfo.Next)
							//	pdx->lstMapInfo.Next = pLink->Next;	//delete head elememt
							//else
							//	pPrevLink->Next = pLink->Next;
							
							winnvme_mmap_locker.Lock();
							RemoveEntryList(&pMapInfo->ListEntry);
							winnvme_mmap_locker.UnLock();

							ExFreePool(pMapInfo);
						}
						else
							irp->IoStatus.Status = STATUS_INVALID_PARAMETER;

						break;
					}

					//pPrevLink = pLink;
					pLink = pLink->Flink;
				}

			}
			else
				irp->IoStatus.Status = STATUS_INVALID_PARAMETER;

			break;

		case IOCTL_WINNVME_ALLOCATE_DMA_MEMORY :
			DbgPrint("Enter  IOCTL_WINNVME_ALLOCATE_DMA_MEMORY\n");

			if (dwInBufLen == sizeof(WINMEM) && dwOutBufLen == sizeof(WINMEM))
			{
				PVOID pvu = nullptr;
				PVOID pvk = nullptr;
				PHYSICAL_ADDRESS phyAddr;

				if (pdx->dmaAdapter) {
#if 0
					pvk = pdx->dmaAdapter->DmaOperations->AllocateCommonBuffer(
						pdx->dmaAdapter,
						pMem->dwSize,
						&phyAddr,
						FALSE
					);
#else
					pvk = pdx->dmaAdapter->DmaOperations->AllocateCommonBufferEx(
						pdx->dmaAdapter,
						nullptr,
						pMem->dwSize,
						&phyAddr,
						FALSE,
						KeGetCurrentNodeNumber()
					);
#endif
				}

				if (pvk)
				{
					//allocate mdl for the mapped kernel address
					PMDL pMdl = IoAllocateMdl(pvk, pMem->dwSize, FALSE, FALSE, NULL);
					if (pMdl)
					{
						PMEMORY pDmaMapInfo;

						//build mdl and map to user space
						MmBuildMdlForNonPagedPool(pMdl);

						pvu = MmMapLockedPagesSpecifyCache(pMdl, UserMode, MmNonCached, NULL, FALSE, NormalPagePriority);

						if (pvu) {
							//insert mapped infomation to list
							pDmaMapInfo = (PMEMORY)ExAllocatePool(NonPagedPool, sizeof(MEMORY));
							pDmaMapInfo->pMdl = pMdl;
							pDmaMapInfo->pvk = pvk;
							pDmaMapInfo->pvu = pvu;
							
							pDmaMapInfo->dmaAddr.QuadPart = phyAddr.QuadPart;
							pDmaMapInfo->Length = pMem->dwSize;

							//PushEntryList(&pdx->lstDMAMapInfo, &pDmaMapInfo->link);
							pdx->winnvme_dma_locker.Lock();
							InsertHeadList(&pdx->winnvme_dma_linkListHead, &pDmaMapInfo->ListEntry);
							pdx->winnvme_dma_locker.UnLock();


							//DbgPrint("phy addr: 0x%llx\n", phyAddr.QuadPart);

							WINMEM   mem;
							mem.phyAddr = (PVOID)phyAddr.QuadPart;
							mem.pvu = pvu;
							mem.dwSize = pMem->dwSize;

							RtlCopyMemory(pSysBuf, &mem, sizeof(WINMEM));

							irp->IoStatus.Information = sizeof(WINMEM);
							DbgPrint("Leave IOCTL_WINNVME_ALLOCATE_DMA_MEMORY\n");

						}
						else {
							IoFreeMdl(pMdl);
							pdx->dmaAdapter->DmaOperations->FreeCommonBuffer(
								pdx->dmaAdapter,
								pMem->dwSize,
								phyAddr,
								pvk,
								FALSE
							);
							irp->IoStatus.Status = STATUS_INSUFFICIENT_RESOURCES;
						}
					}
					else
					{
						//allocate mdl error, unmap the mapped physical memory
						pdx->dmaAdapter->DmaOperations->FreeCommonBuffer(
							pdx->dmaAdapter,
							pMem->dwSize,
							phyAddr,
							pvk,
							FALSE
						);
						irp->IoStatus.Status = STATUS_INSUFFICIENT_RESOURCES;
					}
				}
				else
					irp->IoStatus.Status = STATUS_INSUFFICIENT_RESOURCES;
			}
			else
				irp->IoStatus.Status = STATUS_INVALID_PARAMETER;

			break;

		case IOCTL_WINNVME_UNALLOCATE_DMA_MEMORY:
			DbgPrint("Enter  IOCTL_WINNVME_UNALLOCATE_DMA_MEMORY\n");

			if (dwInBufLen == sizeof(WINMEM))
			{
				PMEMORY pDmaMapInfo;
				//PSINGLE_LIST_ENTRY pLink, pPrevLink;
				PLIST_ENTRY pLink;

				//initialize to head
				//pPrevLink = pLink = pdx->lstDMAMapInfo.Next;
				pLink = pdx->winnvme_dma_linkListHead.Flink;

				while (pLink)
				{
					pDmaMapInfo = CONTAINING_RECORD(pLink, MEMORY, ListEntry);

					if (pDmaMapInfo->pvu == pMem->pvu )
					{
						if (pDmaMapInfo->Length == pMem->dwSize)
						{
							//free mdl, unmap mapped memory
							MmUnmapLockedPages(pDmaMapInfo->pvu, pDmaMapInfo->pMdl);
							IoFreeMdl(pDmaMapInfo->pMdl);

							pdx->dmaAdapter->DmaOperations->FreeCommonBuffer(
								pdx->dmaAdapter,
								pDmaMapInfo->Length,
								pDmaMapInfo->dmaAddr,
								pDmaMapInfo->pvk,
								FALSE
							);

							//DbgPrint("Unmap user virtual address 0x%p, size %u\n", pMapInfo->pvu, pMapInfo->memSize);
							//delete matched element from the list
							//if (pLink == pdx->lstDMAMapInfo.Next)
							//	pdx->lstDMAMapInfo.Next = pLink->Next;	//delete head elememt
							//else
							//	pPrevLink->Next = pLink->Next;
							pdx->winnvme_dma_locker.Lock();
							RemoveEntryList(&pDmaMapInfo->ListEntry);
							pdx->winnvme_dma_locker.UnLock();

							ExFreePool(pDmaMapInfo);
							DbgPrint("Leave IOCTL_WINNVME_UNALLOCATE_DMA_MEMORY\n");
						}
						else
							irp->IoStatus.Status = STATUS_INVALID_PARAMETER;

						break;
					}

					//pPrevLink = pLink;
					pLink = pLink->Flink;
				}

			}
			else
				irp->IoStatus.Status = STATUS_INVALID_PARAMETER;

			break;

		case IOCTL_WINNVME_TEST:
			
			cid = pdx->admin_sq_tail;

			if (pdx->admin_sq_entry != nullptr) {
				pdx->admin_sq_entry[cid].get_log_page.opcode = nvme_admin_get_log_page;
				pdx->admin_sq_entry[cid].get_log_page.command_id = (u16)cid;
				pdx->admin_sq_entry[cid].get_log_page.nsid = 0xffffffff;
				pdx->admin_sq_entry[cid].get_log_page.dptr.prp1 = pdx->data_buffer[cid].dmaAddr.QuadPart;
				pdx->admin_sq_entry[cid].get_log_page.lid = 2;
				pdx->admin_sq_entry[cid].get_log_page.numdl = (4096 / sizeof(u32) - 1) & 0xff;
				pdx->admin_sq_entry[cid].get_log_page.numdu = ((4096 / sizeof(u32) - 1) >> 16) & 0xff;
			}

			if (++pdx->admin_sq_tail == pdx->admin_sq_size) pdx->admin_sq_tail = 0;
			*(volatile u32*)pdx->admin_sq_doorbell = pdx->admin_sq_tail;
			
			break;

		default:
			break;
		}	
	}

	status = irp->IoStatus.Status;

	IoCompleteRequest(irp, IO_NO_INCREMENT);
	return status;
}


void WinNVMeUnload(IN PDRIVER_OBJECT DriverObject)
{
	UNREFERENCED_PARAMETER(DriverObject);

	while (!IsListEmpty(&winnvme_mmap_linkListHead))
	{
		PLIST_ENTRY pEntry = RemoveTailList(&winnvme_mmap_linkListHead);
		PMAPINFO pMapInfo = CONTAINING_RECORD(pEntry, MAPINFO, ListEntry);

		//DbgPrint("Map physical 0x%p to virtual 0x%p, size %u\n", pMapInfo->pvk, pMapInfo->pvu , pMapInfo->memSize );

		MmUnmapLockedPages(pMapInfo->pvu, pMapInfo->pMdl);
		IoFreeMdl(pMapInfo->pMdl);
		MmUnmapIoSpace(pMapInfo->pvk, pMapInfo->memSize);

		ExFreePool(pMapInfo);

	}

}

void WinNVMeDelay(long long millsecond)
{
	LARGE_INTEGER	delayValue, delayTrue;
	NTSTATUS		ntRet;

	// 10*1000*1000 is 1 second, so 10*1000 is 1 millsecond
	delayValue.QuadPart = 10 * 1000 * millsecond; // 320 millisecond
	delayTrue.QuadPart = -(delayValue.QuadPart);
	ntRet = KeDelayExecutionThread(KernelMode, FALSE, &delayTrue);
}
