#include <stdint.h>
#include <string.h>

#include "ftd2xx.h"

static UCHAR Mask = 0x00;
static UCHAR Mode = 0x00;

FT_STATUS FT_CreateDeviceInfoList(LPDWORD lpdwNumDevs) {
  *lpdwNumDevs = 2;
  return FT_OK;
}

FT_STATUS FT_GetDeviceInfoList(FT_DEVICE_LIST_INFO_NODE *pDest,
                               LPDWORD lpdwNumDevs) {
  pDest[0].Flags = 0x0001;
  memcpy(pDest[0].Description, "test1", sizeof("test1") + 1);
  memcpy(pDest[0].SerialNumber, "000001", sizeof("000001") + 1);
  pDest[0].ftHandle = (void *)0x1234;

  pDest[1].Flags = 0x0002;
  memcpy(pDest[1].Description, "test2", sizeof("test2") + 1);
  memcpy(pDest[1].SerialNumber, "000002", sizeof("000002") + 1);
  pDest[1].ftHandle = (void *)0x1235;

  *lpdwNumDevs = 2;
  return FT_OK;
}

FT_STATUS FT_Open(int deviceNumber, FT_HANDLE *pHandle) {
  *pHandle = (void *)0x1234;
  return FT_OK;
}

FT_STATUS FT_OpenEx(PVOID pvArg1, DWORD dwFlags, FT_HANDLE *ftHandle) {
  switch (dwFlags) {
  case FT_OPEN_BY_SERIAL_NUMBER:
    if ((strncmp(pvArg1, "000001", sizeof("000001") + 1) == 0) ||
        (strncmp(pvArg1, "000002", sizeof("000002") + 1) == 0)) {
      return FT_OK;
    } else {
      return FT_DEVICE_NOT_FOUND;
    }
    break;
  case FT_OPEN_BY_DESCRIPTION:
    if (strncmp(pvArg1, "desc_ok", sizeof("desc_ok") + 1) == 0) {
      return FT_OK;
    } else {
      return FT_DEVICE_NOT_FOUND;
    }
    break;
  default:
    return FT_DEVICE_NOT_FOUND;
  }
}

FT_STATUS FT_GetBitmode(FT_HANDLE ftHandle, PUCHAR pucMode) {
  *pucMode = Mode;
  return FT_OK;
}

FT_STATUS FT_SetBitmode(FT_HANDLE ftHandle, UCHAR ucMask, UCHAR ucMode) {
  Mask = ucMask;
  Mode = ucMode;
  return FT_OK;
}

FT_STATUS FT_Close(FT_HANDLE Handle) { return FT_OK; }
