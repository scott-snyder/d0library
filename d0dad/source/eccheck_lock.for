      SUBROUTINE ECCHECK_LOCK(IERR)
C-----------------------------------------------------------------------
C-
C-   Purpose and Methods: Check for a write-lock on an event catalog
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  31-Jul-1994   John D. Hobbs
C-
C-----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:d0dadcom.inc'
      INTEGER IERR,ICNTXT
      LOGICAL LIB$FIND_FILE
      CHARACTER*240 FNOUT
      REAL    DT_SLEEP,DT_TIMEOUT,DT
      PARAMETER(DT_SLEEP=10.0,DT_TIMEOUT=1200.0)
C-----------------------------------------------------------------------
      IERR=0
      DT=0
      CALL UHTOC(IQ(LFLOK+1),4,FNLOCK,LEN(FNLOCK))
      DO WHILE(LIB$FIND_FILE(FNLOCK,FNOUT,ICNTXT).AND.DT.LT.DT_TIMEOUT)
        IF(DT.EQ.0.0 ) WRITE(*,1001) DT_TIMEOUT
        CALL LIB$FIND_FILE_END(ICNTXT)
        ICNTXT=0
        CALL LIB$WAIT(DT_SLEEP)       ! Sleep and retry
        DT=DT+DT_SLEEP
        IERR=IERR+1
      ENDDO
C
      IF( DT.GE.DT_TIMEOUT ) IERR=-1
C
  999 RETURN
 1001 FORMAT(' ECGRUN: Catalog write-locked.  Waiting up to ',F6.0,
     >     ' seconds for access')
      END
