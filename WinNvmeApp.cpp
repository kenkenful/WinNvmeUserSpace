// WinNvmeApp.cpp : このファイルには 'main' 関数が含まれています。プログラム実行の開始と終了がそこで行われます。
//

#define _CRT_SECURE_NO_WARNINGS
#include <windows.h>
#include <iostream>
#include <process.h>

#define	FILE_DEVICE_WINNVME		0x8000

#define	IOCTL_WINNVME_MAP								CTL_CODE(FILE_DEVICE_WINNVME, 0x800, METHOD_BUFFERED, FILE_ANY_ACCESS)
#define	IOCTL_WINNVME_UNMAP						CTL_CODE(FILE_DEVICE_WINNVME, 0x801, METHOD_BUFFERED, FILE_ANY_ACCESS)
#define	IOCTL_WINNVME_ALLOCATE_DMA_MEMORY			CTL_CODE(FILE_DEVICE_WINNVME, 0x802, METHOD_BUFFERED, FILE_ANY_ACCESS)
#define	IOCTL_WINNVME_UNALLOCATE_DMA_MEMORY	CTL_CODE(FILE_DEVICE_WINNVME, 0x803, METHOD_BUFFERED, FILE_ANY_ACCESS)

#define	IOCTL_WINNVME_TEST						CTL_CODE(FILE_DEVICE_WINNVME, 0x804,	METHOD_OUT_DIRECT, FILE_ANY_ACCESS)
#define IOCTL_WINNVME_CLEAR_EVENT		CTL_CODE(FILE_DEVICE_WINNVME, 0x805,	METHOD_OUT_DIRECT, FILE_ANY_ACCESS)


#define ACTION_EVENT "Global\\admin0"

typedef struct tagWINMEM
{
    PVOID phyAddr;			// physical Address for map
    PVOID pvu;					// user space virtual address for unmap
    ULONG dwSize;				// memory size to map or unmap
    ULONG dwRegOff;		// register offset: 0-255
    ULONG dwBytes;			// bytes to read or write
} WINMEM, * PWINMEM;


unsigned __stdcall isr_thread(LPVOID param)
{
    HANDLE handle = (HANDLE)param;
    HANDLE event;
   // event[0] = CreateEvent(NULL, FALSE, FALSE, UI_ACTION_EVENT);
  // if (event[0] == NULL) std::cerr << "failure create" << std::endl;

    event = OpenEvent(SYNCHRONIZE, FALSE , ACTION_EVENT);
    if (event == NULL) std::cerr << "failure open" << std::endl;

    printf("Thread Start\n");


    while (1) {
        printf("waiting\n");;
        DWORD ret = WaitForSingleObject( event, INFINITE);
      // DWORD ret = WaitForMultipleObjects(2, event, FALSE, INFINITE);
        if (ret == WAIT_FAILED) break;
        if (ret == WAIT_OBJECT_0) {
            DeviceIoControl(handle, IOCTL_WINNVME_CLEAR_EVENT, nullptr, 0, nullptr, 0, nullptr, nullptr);
            printf("interrupt occured\n");

        }
        
    }
    printf("Thread Finish\n");

    CloseHandle(event);

    return 0;
}



int main()
{
    UINT8 bus = 3;
    UINT8 dev = 0;
    UINT8 func = 0;

    HANDLE handle = CreateFile(
        "\\\\.\\WINNVME_0",
        GENERIC_READ | GENERIC_WRITE,
        FILE_SHARE_READ | FILE_SHARE_WRITE,
        NULL, // No security attributes
        OPEN_EXISTING,
        FILE_ATTRIBUTE_NORMAL,
        NULL
    );

    if (handle == INVALID_HANDLE_VALUE)
        std::cerr << "failure " << std::endl;

    UINT ThreadId = 0;

    HANDLE isrthread = (HANDLE)_beginthreadex(NULL, 0, isr_thread, (void*)handle, 0, &ThreadId);

 
   PVOID pVirAddr = nullptr;	//mapped virtual addr
   WINMEM pm;
   DWORD dwBytes = 0;
   BOOL bRet = FALSE;
   
   pm.dwSize = 4096;	//memory size
   
   
   bRet = DeviceIoControl(handle, IOCTL_WINNVME_ALLOCATE_DMA_MEMORY, &pm,
           sizeof(WINMEM), &pm, sizeof(WINMEM), &dwBytes, nullptr);
   
   if (bRet)
       printf("Success IOCTL_WINNVME_DMA_MAP: %p, %p, %d, %d\n", pm.pvu, pm.phyAddr, pm.dwSize,dwBytes);
   else
       printf("Failure  IOCTL_WINNVME_DMA_MAP : %d\n", GetLastError());
   
 //  pm.pvu = pVirAddr;	//virtual address
  // pm.dwSize = 4096;	//memory size
   
   bRet =  DeviceIoControl(handle, IOCTL_WINNVME_UNALLOCATE_DMA_MEMORY, &pm,
           sizeof(WINMEM), nullptr, 0, &dwBytes, nullptr);
   
   if (bRet)
       printf("Success IOCTL_WINNVME_DMA_UNMAP\n");
   else
       printf("Failure  IOCTL_WINNVME_DMA_UNMAP : %d\n", GetLastError());
   

    while (1) {

        int d;
        printf("Enter\n");
        scanf("%d", &d);

        if (d == 1) {
            BOOL ret = DeviceIoControl(handle, IOCTL_WINNVME_TEST, nullptr,  0, nullptr, 0, nullptr, nullptr);
            if (ret)
                printf("Success DeviceIoControl\n");
            else
                printf("Failure  DeviceIoControl : %d\n", GetLastError());

        }
        else {
            break;
        }


    };
    


   CloseHandle(isrthread);
    CloseHandle(handle);

    system("pause");

}

