      SUBROUTINE CJET_NEWRMS(LJETS,THRESHOLD,RMS,OK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : calculate a jet rms only using the cells above
C-                         threshold
C-
C-   Inputs  : LJETS: jets bank address
C-             THRESHOLD: threshold for calculating rms
C-   Outputs : RMS: new rms
C-             OK: false if no enough cell information for calculating rms
C-
C-   Created  21-JUL-1995   Qizhong Li-Demarteau
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:PI.DEF'
      INCLUDE 'D0$LINKS:IZJPTS.LINK'
      INTEGER LJETS, LJPTS, LCAEH, GZCAEH
      INTEGER POINT, ICELL, II, NREP
      REAL    THRESHOLD, RMS
      REAL    E(4), PHITMP, THETA, ETATMP
      REAL    ETASUM, PHISUM, ETA2SUM, PHI2SUM, ETAWID2, PHIWID2
      REAL    PHIJET, ETAJET, RTMP, ETSUM
      REAL    PROXIM
      LOGICAL OK
C----------------------------------------------------------------------
      OK = .FALSE.
      IF (LJETS .LE. 0) RETURN
      LJPTS = LQ(LJETS-IZJPTS)
      LCAEH = GZCAEH()
      IF (LCAEH .LE. 0 .OR. LJPTS .LE. 0) then
          CALL ERRMSG('TAU_FIX','CJET_NEWRMS',
     &      'no CAEH or JPTS bank, can not get new_rms','W')
        RETURN
      ENDIF
      NREP = IQ(LCAEH+2)
      PHIJET = Q(LJETS+8)
      ETAJET = Q(LJETS+9)
      ETSUM = 0.0
      ETASUM = 0.0
      PHISUM = 0.0
      ETA2SUM = 0.0
      PHI2SUM = 0.0
      DO 710 ICELL = 1 , IQ(LJPTS+2)
        POINT = LCAEH + NREP*(IQ(LJPTS+ICELL+2)-1)
        DO 700 II = 1 ,4
          E(II) = Q(POINT+3+II)
  700   CONTINUE
        CALL ETOETA(E,PHITMP,THETA,ETATMP)
        PHITMP = PROXIM(PHITMP,PHIJET)
C
C ****  only use cells above threshold
C
        IF (ABS(E(4)) .LT. THRESHOLD) GOTO 710 
        RTMP = SQRT((ETATMP-ETAJET)**2 + (PHITMP-PHIJET)**2)
        IF (RTMP .GT. 0.7) GOTO 710   ! don't use cell out of 0.7 cone
        ETASUM = ETATMP*Q(POINT+8) + ETASUM
        PHISUM = PHITMP*Q(POINT+8) + PHISUM
        ETSUM = Q(POINT+8) + ETSUM
        ETA2SUM = ETATMP*ETATMP*Q(POINT+8) + ETA2SUM
        PHI2SUM = PHITMP*PHITMP*Q(POINT+8) + PHI2SUM
  710 CONTINUE
      IF (ETSUM .EQ. 0) GOTO 999
      ETAWID2 = ETA2SUM/ETSUM
      PHIWID2 = PHI2SUM/ETSUM
      ETAWID2 = ETAWID2-(ETASUM/ETSUM)**2
      PHIWID2 = PHIWID2-(PHISUM/ETSUM)**2
      IF (ETAWID2 .LT. 0 .OR. PHIWID2 .LT. 0) THEN
          CALL ERRMSG('TAU_FIX','CJET_NEWRMS',
     &      'negative RMS','W')
      ENDIF
      RMS = SQRT(ABS(ETAWID2)+ABS(PHIWID2))
      OK = .TRUE.
C      
  999 RETURN
      END
