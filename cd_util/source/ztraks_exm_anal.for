      FUNCTION ZTRAKS_EXM_ANAL()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Do PAR initialization and ZTRAKS processing
C-    for CD Examine2 package
C-
C-   Returned value  : TRUE
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  25-APR-1991   Susan K. Blessing
C-   Updated  20-MAR-2004   sss - compile with g77
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INTEGER IER
      INTEGER ILINKS(3)    
C
      LOGICAL ZTRAKS_EXM_ANAL
      LOGICAL OK,DAQ
      LOGICAL ZTRPAR, VTRPAR
      LOGICAL CDCON,FDCON,VTXON,TRDON
      LOGICAL VTRAKS,DTREVT,FTRAKS
      LOGICAL CALL_VERTEX, VERTEX
      LOGICAL CALL_TRD
      LOGICAL FIRST
C
      INTEGER IOS
      INTEGER OLDRUN,RUN,RUNNO
C
      DATA FIRST/.TRUE./
      DATA RUN,OLDRUN/2*0/
      DATA CDCON,FDCON,VTXON/3*.TRUE./
C
C----------------------------------------------------------------------
C
      IF (FIRST) THEN
        CALL EZPICK('ZTRAKS_RCP',IER)
        CALL EZGET('CALL_VERTEX',CALL_VERTEX,IER)
        CALL EZGET('CDCON',CDCON,IER)
        CALL EZGET('FDCON',FDCON,IER)
        CALL EZGET('VTXON',VTXON,IER)
        CALL EZGET('TRDON',TRDON,IER)
        CALL EZGET('CALL_TRD',CALL_TRD,IER)
        FIRST = .FALSE.
        CALL EZRSET
      END IF
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
        OK = ZTRPAR()
        IF (.NOT.OK) THEN
          ZTRAKS_EXM_ANAL = OK
          GO TO 999
        END IF
C
        IF (VTXON) THEN
          OK = VTRPAR()
          IF (.NOT.OK) THEN
            ZTRAKS_EXM_ANAL = OK
            GO TO 999
          END IF
          CALL VTX_GAIN_SAVE
        END IF
C
      END IF
C
      IF (CALL_VERTEX) OK = VERTEX() 
      IF (VTXON) OK = VTRAKS() 
      IF (CDCON) OK = DTREVT() 
      IF (FDCON) OK = FTRAKS() 
      IF (CALL_TRD.AND.TRDON) CALL TTRAKS(ILINKS)
      CALL ZTRHIS
C
      ZTRAKS_EXM_ANAL = .TRUE.
C
  999 RETURN
      END
