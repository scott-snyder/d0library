      SUBROUTINE DB_SERVER_LCKRES
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Locks and reserves the main update DBL3 server
C-                         cache.
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Modified   9-AUG-1991   SHAHRIAR ABACHI
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE       'D0$INC:DB_SRVR_NAMES.INC'
      INCLUDE       'D0$PARAMS:CPCERR.PARAMS'
      INTEGER*4     ICACHE(4)
      INTEGER       IERR,IDUM,IPID,ICPU
      CHARACTER     PRCNM*20, CPUNM*10
      LOGICAL       LKHELD
C
      CACHE_RESLCK = DB_CURRENT_SNAME(1:2)//'_DB_RESLK'
      CALL IC_BOOK (CACHE_RESLCK,ICACHE,'R',IERR)
      IF(IERR.NE.0) THEN
        PRINT *,'Server**ERROR** Resource cache not booked'
        GO TO 999
      ENDIF
      CALL IC_TAKE_LOCK(CACHE_RESLCK,ICACHE,IDUM,5.,' ',IERR)
      IF(IERR.EQ.IC_E_LKHELD) THEN
        CALL IC_CHECK_LOCK(CACHE_RESLCK,LKHELD,
     +    ICACHE,IPID,ICPU,PRCNM,CPUNM,' ',IERR)
        PRINT *,'Server**ERROR** Resource cache held'
        PRINT *,'      **ERROR** Maybe another copy is running'
        PRINT *,'      **ERROR** Holder "'//prcnm//'" on "'//cpunm//'"'
        PRINT '(2z)',ipid,icpu
        STOP 444
      ENDIF
  999 RETURN
      END
