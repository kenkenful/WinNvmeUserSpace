#pragma once

#include <wdm.h>

class SpinLock {

public:
	void Init();
	void LockAtDPCLevel();
	void UnLockFromDPCLevel();

private:
	KSPIN_LOCK m_lock;

};