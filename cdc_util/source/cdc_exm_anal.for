      FUNCTION CDC_EXM_ANAL()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-
C-   Returned value  : 
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  18-APR-1991   Susan K. Blessing
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      LOGICAL CDC_EXM_ANAL
      LOGICAL DTREVT,DTRPAR
      LOGICAL OK,DAQ
C
      INTEGER RUN,RUNNO,OLDRUN
      INTEGER IOS
C
      DATA RUN,OLDRUN/2*0/
C
C----------------------------------------------------------------------
C
C CHECK EVENT TYPE
      CALL GET_EVENT_TYPE('DAQ',DAQ)
      IF (.NOT.DAQ) GO TO 999
C
      CALL GET_ZEBRA_IOS (IOS)
      IF (IOS.NE.0) GO TO 999
C
      RUN = RUNNO()
      IF (RUN.NE.OLDRUN) THEN
C
        OLDRUN = RUN
C
        OK = DTRPAR()
        IF (.NOT.OK) THEN
          CDC_EXM_ANAL = OK
          GO TO 999
        END IF
C
      END IF
C
      OK = DTREVT()
      CDC_EXM_ANAL = OK
C
  999 RETURN
      END
