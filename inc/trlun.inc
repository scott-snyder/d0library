CDECK  ID>, TRFCMN. 
C-- Modified :    John Skeens   02/03/92  for D0
C-- Author :    David Adams   17/02/92  for SDC
C This deck contains pieces of TRFCMN which may be different for
C different applications.
C
C  Logical unit variables for the input and output devices
C  MXP variables are for number of events to print
      INTEGER        LUNINP, LUNCLU, LUNSCL, LUNTRK, LUNTRF, LUNERR,
     1                       MXPCLU, MXPSCL, MXPTRK, MXPTRF, MXPERR
C  ITRRUN and ITREVT are the run and event numbers.
      INTEGER ITRRUN, ITREVT
C Common declaration.
      COMMON /TRLUN/ LUNINP, LUNCLU, LUNSCL, LUNTRK, LUNTRF, LUNERR,
     1                       MXPCLU, MXPSCL, MXPTRK, MXPTRF, MXPERR,
     1               ITRRUN, ITREVT

