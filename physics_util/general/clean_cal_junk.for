      FUNCTION CLEAN_CAL_JUNK()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-     remove junk events due to bad calorimeter data
C-
C-   Returned value  : false if junky data
C-
C-   Created  7-Jan-1993   Serban D. Protopopescu
C-   Updated  23-SEP-1993   Rajendran Raja  changed set_caph/reset_caph 
C-   logic 
C-   Updated   7-OCT-1993   Dhiman Chakraborty   
C-                          Commented out the part that looks for MR junk.
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL CLEAN_CAL_JUNK
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER LJETS,LPNUT,LPELC,I
      INTEGER GZPMUO,GZJETS,GZPNUT,GZPELC,GZPPHO,LPPHO
      INTEGER ICHOICE,IER,LVERT,GZVERT,RUN,ID
      REAL    ETA,PHI,TEMPLATE(5,4)
      REAL    ET,ETMIN,EM_FRAC,ZVTX
      REAL    THETA,DETA,MET1
      LOGICAL FIRST,OK
      DATA FIRST/.TRUE./
      DATA ICHOICE,ETMIN/2,10./
      DATA TEMPLATE/
     &  1.,6.,0.7,0.,0.,      ! CONE R=0.7
     &  1.,6.,0.5,0.,0.,      ! CONE R=0.5
     &  1.,6.,0.3,0.,0.,      ! CONE R=0.3
     &  2.,7.,2.,8.,2./       ! NN 2x2
      LOGICAL ISET
C----------------------------------------------------------------------
C
      CLEAN_CAL_JUNK=.FALSE.      ! set it to false to skip any additional
                                  ! processing of current event
      ISET = .FALSE.              !No calls to set_caph yet
C
C       book histograms in directory DST
C
      IF(FIRST) THEN
        FIRST=.FALSE.
      ENDIF
C
      LVERT=GZVERT(1)
      IF(LVERT.GT.0) ZVTX=Q(LVERT+5)
C
C        find junk electrons
      LPELC=GZPELC()
      DO WHILE (LPELC.GT.0)
        IF(Q(LPELC+7).GT.ETMIN) THEN
          THETA=Q(LPELC+8)
          PHI=Q(LPELC+10)
          CALL DET_ETA(ZVTX,THETA,DETA)
          IF(PHI.LT.5.346.AND.PHI.GT.5.342.AND.DETA.LT.-0.2
     &        .AND.DETA.GT.-0.8) GOTO 999
        ENDIF
        LPELC=LQ(LPELC)
      ENDDO
C
C        find junk photon
      LPPHO=GZPPHO()
      DO WHILE (LPPHO.GT.0)
        IF(Q(LPPHO+7).GT.ETMIN) THEN
          THETA=Q(LPPHO+8)
          PHI=Q(LPPHO+10)
          CALL DET_ETA(ZVTX,THETA,DETA)
          IF(PHI.LT.5.346.AND.PHI.GT.5.342.AND.DETA.LT.-0.2
     &        .AND.DETA.GT.-0.8) GOTO 999
        ENDIF
        LPPHO=LQ(LPPHO)
      ENDDO
C
C       remove junk jets
C
      IF(ICHOICE.EQ.1)THEN
        CALL SET_CAPH('CONE_JET',TEMPLATE(1,1),IER)
        ISET = .TRUE.
      ELSEIF(ICHOICE.EQ.2)THEN
        CALL SET_CAPH('CONE_JET',TEMPLATE(1,2),IER)
        ISET = .TRUE.
      ELSEIF(ICHOICE.EQ.3)THEN
        CALL SET_CAPH('CONE_JET',TEMPLATE(1,3),IER)
        ISET = .TRUE.
      ELSEIF(ICHOICE.EQ.4) THEN
        CALL SET_CAPH('NN_JET',TEMPLATE(1,4),IER)
        ISET = .TRUE.
      ENDIF
C
      LJETS=GZJETS()
      DO WHILE (LJETS.GT.0)
        EM_FRAC=Q(LJETS+14)
        IF(EM_FRAC.LT..05) GOTO 999   ! likely hot cell in FH or CC
        PHI=Q(LJETS+8)
        THETA=Q(LJETS+7)
        CALL DET_ETA(ZVTX,THETA,DETA)
C             known hot cell in ECEM
        IF(PHI.GT.4.75.AND.PHI.LT.4.8.AND.DETA.GT.2.2
     &        .AND.DETA.LT.2.7.AND.EM_FRAC.GT..93) GOTO 999
C          main ring junk
C        IF(PHI.GT.1.6.AND.PHI.LT.1.8.AND.DETA.GT.-1.0
C     &        .AND.DETA.LT.1.0.AND.EM_FRAC.LT..3) GOTO 999
        LJETS=LQ(LJETS)  ! pointer to next jet
      ENDDO
C
      CLEAN_CAL_JUNK=.TRUE.
  999 CONTINUE
      IF ( ISET ) THEN
        CALL RESET_CAPH
      ENDIF
      RETURN
      END
