#include "SpinLock.h"

void SpinLock::Init() {
	KeInitializeSpinLock(&m_lock);
}

void SpinLock::LockAtDPCLevel() {
	KeAcquireSpinLockAtDpcLevel(&m_lock);
}

void SpinLock::UnLockFromDPCLevel() {
	KeReleaseSpinLockFromDpcLevel(&m_lock);
}