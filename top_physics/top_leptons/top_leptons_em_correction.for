      REAL FUNCTION TOP_LEPTONS_EM_CORRECTION(LCLUS)
C----------------------------------------------------------------------
C-   Purpose and Methods : compute EM scale correction factor
C-
C-   Inputs  : link to EM cluster
C-   Outputs : EM correction factor
C-   Controls: Top leptons RCP
C-
C-   Created   3-JUL-1993   Meenakshi Narain
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZVERT.LINK/LIST'
C
      EXTERNAL TOP_LEPTONS_UTIL_MONTECARLO
C
      LOGICAL FIRST,TOP_LEPTONS_UTIL_MONTECARLO
C
      INTEGER LCLUS,IER,N
      INTEGER LVERT,LVERH,GZVERH
      INTEGER VERSION,PASS
      REAL    FSCALE,HV_COR,EM_COR(3),HV_SCALE
      REAL    CAL_ETA,ETA_DET,THETA,ETAD,ZV
      SAVE FIRST
      DATA FIRST / .true. /
C----------------------------------------------------------------------
      IER=0
      IF(FIRST)THEN
        CALL EZPICK('TOP_LEPTONS_RCP')  
        CALL EZGET('HV_SCALE_FACTOR',HV_COR,IER)
        IF(IER.EQ.0)CALL EZGETA('EM_SCALE_FACTORS',0,0,0,N,IER)
        IF(IER.EQ.0)CALL EZGETA('EM_SCALE_FACTORS',1,N,1,EM_COR,IER)
        CALL EZRSET
        FIRST = .FALSE.
      ENDIF
C
      FSCALE=1.0
C
C *** Check for Monte Carlo - skip out if detected
C
      IF(TOP_LEPTONS_UTIL_MONTECARLO()) THEN
        GO TO 500
      ENDIF
C
      CALL RECO_VERSION(VERSION,PASS)
      IF (VERSION.GT.10) THEN
        HV_SCALE = 1.
      ELSE
        HV_SCALE = HV_COR
      ENDIF
C
C ****  Get Detector Eta
C
      CAL_ETA = Q(LCLUS+19)
      IF (CAL_ETA.EQ.0) THEN
C       FIRST get Z vertex
        LVERT=0
        LVERH=GZVERH()
        IF (LVERH.GT.0) LVERT=LQ(LVERH-IZVERT)
        ZV=Q(LVERT+5)
C       now compute detector eta
        THETA=Q(LCLUS+8)
        CALL DET_ETA(ZV,THETA,ETAD)
        ETA_DET = 10.* ETAD + SIGN(1.,ETAD)
        CAL_ETA = ETA_DET
      ENDIF
      IF(CAL_ETA.LT.-13)THEN
        FSCALE=HV_SCALE*EM_COR(1)       ! ECN
      ELSEIF(ABS(CAL_ETA).LE.13)THEN     
        FSCALE=HV_SCALE*EM_COR(2)       ! CC
      ELSE
        FSCALE=HV_SCALE*EM_COR(3)       ! ECS
      ENDIF
  500 TOP_LEPTONS_EM_CORRECTION=FSCALE
  999 RETURN
      END
