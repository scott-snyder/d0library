      FUNCTION VTX_EXM_ANAL()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  23-JAN-1992   Susan K. Blessing
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INTEGER IOS
      INTEGER RUN,RUNNO,OLDRUN
C
      LOGICAL VTX_EXM_ANAL
      LOGICAL VTRPAR,VTX_EXM_HST,VTRAKS_EXTERNAL
      LOGICAL OK
C
      DATA OLDRUN/-1/
C----------------------------------------------------------------------
C
      CALL GET_ZEBRA_IOS (IOS)
      IF (IOS.NE.0) GO TO 999
C
      RUN = RUNNO()
C
      IF (RUN.NE.OLDRUN) THEN
C
        OLDRUN = RUN
        OK = VTRPAR()
C
      END IF
C
      VTX_EXM_ANAL = VTRAKS_EXTERNAL()
      IF(VTX_EXM_ANAL) THEN
        OK = VTX_EXM_HST()
        CALL VTX_ACCUM_TRACKS
      ENDIF
C
  999 RETURN
      END
