#include "FastMutex.h"

void FastMutex::Init() {
	ExInitializeFastMutex(&_mutex);
}

void FastMutex::Lock() {
	ExAcquireFastMutex(&_mutex);
}

void FastMutex::UnLock() {
	ExReleaseFastMutex(&_mutex);
}