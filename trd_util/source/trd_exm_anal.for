      FUNCTION TRD_EXM_ANAL()
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
C-   Updated   7-DEC-1995   Susan K. Blessing  Kludge fix. 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
C
      LOGICAL TRD_EXM_ANAL
      LOGICAL TRD_FILLS_EVT,TRDPAR
      LOGICAL OK,DAQ
C
      INTEGER RUN,RUNNO,OLDRUN
      INTEGER IOS
      INTEGER LCDD4
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
        OK = TRDPAR()
        IF (.NOT.OK) THEN
          TRD_EXM_ANAL = OK
          GO TO 999
        END IF
C
      END IF
C
C Check if CDD4 bank exists
      LCDD4 = LQ(LHEAD-6)
      IF (LCDD4.GT.0) THEN
C
        OK = TRD_FILLS_EVT()
        CALL TRD_HISTOS
C
      END IF
      TRD_EXM_ANAL = OK
C
  999 RETURN
      END
