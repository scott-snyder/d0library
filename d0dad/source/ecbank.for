      SUBROUTINE ECBANK(IERR)
C-----------------------------------------------------------------------
C-
C-   Purpose and Methods: Book the control bank for an event catalog.
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  11-Jan-1994   John D. Hobbs
C-
C-----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:zebcom.inc'
      INCLUDE 'D0$INC:d0dadcom.inc'
      INTEGER IERR
C-----------------------------------------------------------------------
C
C  Zero out protection
C
      CALL VZERO(LRUNS,NLECHD)
C
C  Allocate space...
C
      CALL MZBOOK(IXDDAD,LECHD,LDADH,-JFEC,'ECHD',
     +   NLECHD,NLECHD,NDEC+KECHED,2,0)
      IF( LECHD.GT.0 ) THEN
         IERR=0
         IQ(LDADH+JFEC)=IQ(LDADH+JFEC)+1
      ELSE
         IERR = -1
      ENDIF
C
C  Allocate bank for lock file name.
C
      CALL MZBOOK(IXDDAD,LFLOK,LECHD,-LLFLOK,'FLOK',0,0,LEN(FNLOCK),2,0)
      IF( LFLOK.GT.0 ) THEN
         IERR=0
      ELSE
         IERR = -2
      ENDIF
C
  999 RETURN
      END
