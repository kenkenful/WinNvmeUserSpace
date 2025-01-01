// WinNvmeApp.cpp : このファイルには 'main' 関数が含まれています。プログラム実行の開始と終了がそこで行われます。
//

#define _CRT_SECURE_NO_WARNINGS
#include <windows.h>
#include <iostream>
#include <process.h>
#include <timeapi.h>
#include "winnvme.h"

#pragma comment(lib, "winmm.lib")

#define ACTION_EVENT "Global\\admin0"
//#define ACTION_EVENT "admin0"


int counter = 0;
//std::atomic<int> counter(0);

void usleep(DWORD waitTime) {
    LARGE_INTEGER cpuFreq;
    LARGE_INTEGER start, now;

    QueryPerformanceFrequency(&cpuFreq);
    QueryPerformanceCounter(&start);

    do {
        QueryPerformanceCounter(&now);
        for (int i = 0; i < 10; ++i) _mm_pause();
    } while ((now.QuadPart - start.QuadPart) / float(cpuFreq.QuadPart) * 1000 * 1000 < waitTime);

}

unsigned __stdcall isr_thread(LPVOID param)
{
   // HANDLE handle = (HANDLE)param;
   // HANDLE event;
   // event[0] = CreateEvent(NULL, FALSE, FALSE, UI_ACTION_EVENT);
  // if (event[0] == NULL) std::cerr << "failure create" << std::endl;
    
    //HANDLE    event[2];
    //for(int i=0; i< 2; ++i)  event[i] = OpenEvent(SYNCHRONIZE, FALSE, ACTION_EVENT);
    HANDLE    event;
    event = OpenEvent(SYNCHRONIZE, FALSE , ACTION_EVENT);
    if (event == NULL) {
        std::cerr << "failure open" << std::endl;
        return 1;
    }

    printf("Thread Start\n");

    while (1) {

        //DWORD ret = WaitForSingleObject( event, INFINITE);

      /* admin コマンドとIOコマンドでeventを分ける必要があるので、 WaitForMultiを使い、戻り値でどのeventがシグナル状態になったかを判別する。  
      IO Queue 2個の場合、
      event[0]  ---- Admin Command
      event[1]  ---- IO Command
      event[2]  ---- IO Command
      */
      DWORD ret = WaitForMultipleObjects(1, &event, FALSE, INFINITE);
      if (ret == WAIT_FAILED) {
          printf("wait failed\n");
          break;
      }else if (ret == WAIT_OBJECT_0) {
            //DeviceIoControl(handle, IOCTL_WINNVME_CLEAR_EVENT, nullptr, 0, nullptr, 0, nullptr, nullptr);
           // printf("interrupt occured\n");
          ++counter;
          //usleep(20);
      }
      else {
      
      }
    }

    // for(int i=0; i<2; ++i)     CloseHandle(event[i]);
    CloseHandle(event);
    printf("Thread Finish\n");

    return 0;
}

unsigned __stdcall print_polling(LPVOID param) {

    while (1) {
        printf("interrupt occured: %d\n", counter);
        Sleep(100);
   
    }
    return 0;
}



int main()
{
    timeBeginPeriod(1);
/*
    UINT8 bus = 3;
    UINT8 dev = 0;
    UINT8 func = 0;
    */

    SetPriorityClass(GetCurrentProcess(), HIGH_PRIORITY_CLASS);

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

#if 1
    UINT ThreadId = 0;
    HANDLE isrthread = (HANDLE)_beginthreadex(NULL, 0, isr_thread, nullptr, 0, &ThreadId);
    HANDLE printThread = (HANDLE)_beginthreadex(NULL, 0, print_polling, nullptr, 0, nullptr);

    if (!SetThreadPriority(GetCurrentThread(), THREAD_PRIORITY_TIME_CRITICAL)) {
        printf("Error %d\n", GetLastError());
    }
#endif

   //PVOID pVirAddr = nullptr;	//mapped virtual addr
  // WINMEM pm;
  // DWORD dwBytes = 0;
  // BOOL bRet = FALSE;
   
   //pm.dwSize = 4096;	//memory size
   
   
  // bRet = DeviceIoControl(handle, IOCTL_WINNVME_ALLOCATE_DMA_MEMORY, &pm,
   //        sizeof(WINMEM), &pm, sizeof(WINMEM), &dwBytes, nullptr);
   
   //if (bRet)
   //    printf("Success IOCTL_WINNVME_DMA_MAP: %p, %p, %d, %d\n", pm.pvu, pm.phyAddr, pm.dwSize,dwBytes);
   //else
   //    printf("Failure  IOCTL_WINNVME_DMA_MAP : %d\n", GetLastError());
   
 //  pm.pvu = pVirAddr;	//virtual address
  // pm.dwSize = 4096;	//memory size
   
   //bRet =  DeviceIoControl(handle, IOCTL_WINNVME_UNALLOCATE_DMA_MEMORY, &pm,
      //     sizeof(WINMEM), nullptr, 0, &dwBytes, nullptr);
   
   //if (bRet)
      // printf("Success IOCTL_WINNVME_DMA_UNMAP\n");
   //else
      // printf("Failure  IOCTL_WINNVME_DMA_UNMAP : %d\n", GetLastError());
   

   Sleep(1000);

   for (int i = 0; i < 16; ++i) {
       BOOL ret = DeviceIoControl(handle, IOCTL_WINNVME_TEST, nullptr, 0, nullptr, 0, nullptr, nullptr);
       //  if (ret)
        //     printf("Success DeviceIoControl\n");
        // else
         //    printf("Failure  DeviceIoControl : %d\n", GetLastError());

         //Sleep(0);
   };

#if 0

    LARGE_INTEGER cpuFreq;
    LARGE_INTEGER count1, count2;

    QueryPerformanceFrequency(&cpuFreq);
    QueryPerformanceCounter(&count1);

    // to do anything

    QueryPerformanceCounter(&count2);

    printf("%f [msec]\n",   1000.0 *   ((double)count2.QuadPart - count1.QuadPart) /cpuFreq.QuadPart  );


#endif


    timeEndPeriod(1);
    system("pause");
   CloseHandle(isrthread);
   CloseHandle(printThread);
   CloseHandle(handle);



}

