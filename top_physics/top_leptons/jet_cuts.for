      LOGICAL FUNCTION JET_CUTS
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-
C-   Returned value  :
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  18-NOV-1993   Balamurali V
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:VERT.INC'
      INCLUDE 'D0$INC:ELEC.INC'
      INCLUDE 'D0$INC:PHOT.INC'
      INCLUDE 'D0$INC:MET.INC'
      INCLUDE 'D0$INC:JET.INC'
      REAL     PI,TWOPI,HALFPI,RAD
      INCLUDE 'D0$INC:PI.INC'
C
      REAL    FICD_ACCEPT,JETA_MAX,ET,PHIJET,ETAJET,DETAJET
      REAL    THETAJET,DRMIN,DPHIEJET,DR,ETJET,FEMJET,FICD
      REAL    FICD_JET,COSJ,DRJ
      INTEGER IER,I,J,K,JSKIP
      LOGICAL FIRST,REQ_CLEANJ
      DATA    FIRST/.TRUE./
C----------------------------------------------------------------------
      IF(FIRST)THEN
        CALL EZPICK('TTEE_RCP')
        CALL EZGET('FICD_ACCEPT',FICD_ACCEPT,IER)
        CALL EZGET('JETA_MAX',JETA_MAX,IER)
        CALL EZGET('REQ_CLEANJ',REQ_CLEANJ,IER)
        CALL EZRSET
        FIRST = .FALSE.
      ENDIF
C
C ** Initialize Variables
C
      DO I = 1,MAX_JET
        IJET(I) = 0
      ENDDO
C
C ** Reject event if Serban's cleanup fails
C
      IF(REQ_CLEANJ)THEN
        DO I = 1,NJET
          ETJET  = JET(2,I)
          IF (ETJET.LE.0)GOTO 95
          FEMJET = JET(6,I)
          PHIJET = JET(3,I)
          DETAJET = JET(8,I)
C
C ** Hot cells
          IF((PHIJET.GT.4.75.AND.PHIJET.LT.4.8).AND.
     &      (DETAJET.GT.2.2.AND.DETAJET.LT.3.2).AND.
     &      FEMJET.GT.0.93) THEN
            JET_CUTS = .FALSE.
            GOTO 999
          ENDIF
          IF(ETJET.GT.900.)THEN
            JET_CUTS = .FALSE.
            GOTO 999
          ENDIF
C
C ** Main ring junk
          IF(PHIJET.GT.1.6.AND.PHIJET.LT.1.8.AND.DETAJET.GT.-1.0
     &        .AND.DETAJET.LT.1.0.AND.FEMJET.LT..3)THEN
            JET_CUTS = .FALSE.
            GOTO 999
          ENDIF
        END DO  
   95   CONTINUE
      ENDIF
C
C
C ** Now make cuts and store good jet numbers in array IJET
C
      NUM_JET = 0
      DO I = 1, NJET
        PHIJET = JET(3,I)
        ETAJET = JET(4,I)
        DETAJET = JET(8,I)
        THETAJET = 2.*ATAN(EXP(-ETAJET))
        DRMIN=999.
        DO J = 1,NELEC
          DPHIEJET = RELEC(2,abs(igelec(J)))-PHIJET
          IF(DPHIEJET.GT.PI)DPHIEJET=2*PI-DPHIEJET
          DR = (RELEC(3,abs(igelec(J)))-ETAJET)**2 + (DPHIEJET)**2
          IF (DR.GT.0) DR = SQRT(DR)
          IF (DR.LT.DRMIN) DRMIN=DR
        ENDDO
C        DO J = 1,NPHOT
C          DPHIEJET = RPHOT(2,J)-PHIJET
C          IF(DPHIEJET.GT.PI)DPHIEJET=2*PI-DPHIEJET
C          DR = (RPHOT(3,J)-ETAJET)**2 + (DPHIEJET)**2
C          IF (DR.GT.0) DR = SQRT(DR)  
C          IF (DR.LT.DRMIN) DRMIN=DR
C        ENDDO
        IF (DRMIN.GE.0.25) THEN
          JSKIP = 0
          DO K = 1, IBAD
            COSJ=COS(BJTHETA(K))*COS(THETAJET)+SIN(BJTHETA(K))
     &        *SIN(THETAJET)*COS(BJPHI(K)-PHIJET)
            DRJ = (BJETA(K)-ETAJET)**2 + (BJPHI(K)-PHIJET)**2
            IF (DRJ.GT.0) DRJ = SQRT(DRJ)
            IF(DRJ.LE.0.15)THEN
              JSKIP = 1
            ENDIF
          ENDDO
          IF (JSKIP.EQ.0) THEN
            FICD = JET(5,I)
            IF(FICD .LT. FICD_ACCEPT)THEN
              IF (ABS(ETAJET).GT.JETA_MAX)GOTO 100
              NUM_JET = NUM_JET + 1
              IJET(NUM_JET) = I
            ENDIF
          ENDIF
        ENDIF
  100   CONTINUE
      ENDDO
C
      JET_CUTS = .TRUE.
C
  999 RETURN
      END
