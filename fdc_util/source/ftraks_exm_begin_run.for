      FUNCTION FTRAKS_EXM_BEGIN_RUN()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Call FTRPAR.  Need to know run number before
C-                         call.
C-
C-   Returned value  : 
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  16-FEB-1990   Susan Blessing
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INTEGER IOS
      INTEGER RUN,RUNNO
      INTEGER OLDRUN
C
      LOGICAL FTRAKS_EXM_BEGIN_RUN
C
      LOGICAL OK
      LOGICAL FTRPAR
C
      DATA OLDRUN/0/
C----------------------------------------------------------------------
C
      CALL GET_ZEBRA_IOS (IOS)
      IF (IOS.NE.0) GO TO 999
C
C Check to see if FTRAKS_EXM_BEGIN_RUN has already been called for this run
      RUN = RUNNO()
      IF (RUN.NE.OLDRUN) THEN
C
        OLDRUN = RUN
        OK = FTRPAR()
C
      ELSE
        OK = .TRUE.
      END IF
C
      FTRAKS_EXM_BEGIN_RUN = OK
C
  999 RETURN
      END
