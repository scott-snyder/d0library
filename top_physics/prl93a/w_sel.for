      FUNCTION W_SEL()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-
C-     select e + jets events
C-
C-   Returned value  : true
C-
C-   ENTRY W_DIAL : example of dialog for option 'User Dialog'
C-
C-   Created  26-JUL-1990   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL W_SEL,W_DIAL,W_DUMP,W_FIN
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKA.INC'
      INCLUDE 'D0$INC:PI.DEF'
      INTEGER DMPUNI,DUNIT,N1
      INTEGER LPNUT,LPELC,LJETS
      INTEGER GZPNUT,GZPELC,GZJETS
      INTEGER NEL,I,RUN,ID,J,K,ISYS,IER,NJETS,NJSEL
      REAL    ETA,PHI,ETIN,ET
      REAL    EIN,THETA
      REAL    MET1,MET2,ZVTX,PNUT(4)
      REAL    ETEL,ETAE,ETE_MIN,METC,MET_MIN,ETJ_MIN
      REAL    TEMPLATE(5)
      LOGICAL FIRST,OK,MC
      LOGICAL SELECT
      LOGICAL GOOD_ELECTRON,GOOD_EL
      DATA FIRST/.TRUE./
      DATA ETE_MIN,MET_MIN,ETJ_MIN/18.,15.,10./
      DATA TEMPLATE/
     &  1.,6.,0.5,0.,0./      ! CONE R=0.5
      DATA NJSEL/0/
C----------------------------------------------------------------------
C
      W_SEL=.FALSE.      ! set it to false to skip any additional
                         ! processing of current event
C
C       book histograms in directory DST
C
      IF(FIRST) THEN
        FIRST=.FALSE.
C
C           book histograms
      ENDIF
C
      CALL EVNTID(RUN,ID)
C
      MC=.FALSE.
      IF(IQ(LHEAD+1).GT.1000) MC=.TRUE.     ! MC data
C
C        find electron
      THETA=0.
      ETEL=ETE_MIN-5.
      LPELC=GZPELC()
      DO WHILE (LPELC.GT.0)
        IF(Q(LPELC+7).GT.ETEL.AND.GOOD_EL(LPELC)) THEN
          ETEL=Q(LPELC+7)
        ENDIF
        LPELC=LQ(LPELC)
      ENDDO
      IF(ETEL.LT.ETE_MIN) GOTO 999 !
C
C        missing ET
C
      MET2=0.
      LPNUT=GZPNUT(2)     ! pick missing ET bank with ICD correction
      IF(LPNUT.GT.0) THEN
        CALL UCOPY(Q(LPNUT+3),PNUT,4)
        MET2=SQRT(PNUT(1)**2+PNUT(2)**2)
      ENDIF
      IF(MET2.LT.MET_MIN) GOTO 999
      IF(NJSEL.GT.0) THEN
        CALL SET_CAPH('CONE_JET',TEMPLATE,IER)
        LJETS=GZJETS()
        IF(LJETS.NE.0) THEN
          NJETS=-1              ! set to -1 to not count electron
          DO WHILE (LJETS.GT.0)
            IF(Q(LJETS+6).GT.ETJ_MIN) NJETS=NJETS+1
            LJETS=LQ(LJETS)
          ENDDO
        ENDIF
        CALL RESET_CAPH
        IF(NJETS.LT.NJSEL) GOTO 999
      ENDIF
      W_SEL=.TRUE.      ! set it to false to skip any additional
      GOTO 999
C
C
      ENTRY W_DIAL
C
      W_DIAL=.TRUE.
      NJSEL=0
      CALL GETPAR(1,'Minimum no. of jets [0]>','I',NJSEL)
      GOTO 999
C
C
      ENTRY W_DUMP
      W_DUMP=.TRUE.
      GOTO 999
C
C
      ENTRY W_FIN
      W_FIN=.TRUE.
  999 RETURN
      END
      LOGICAL FUNCTION GOOD_EL(LPELC)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-     select good electron
C-   Inputs  : LPELC = pointer to electron
C-
C-   ENTRY GOOD_EL_SET(VALUE)
C-   Input: VALUE true passes all PELC
C-
C-   Created  24-SEP-1992   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL GOOD_EL_SET,PASS,VALUE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:PI.DEF'
      INTEGER LPELC,LHMTE,LZTRK,LZFIT
      INTEGER RUNNO
      REAL    THETA,PHIE,EISO,ETA,DPHI,DTH,CHSQ
      DATA PASS/.FALSE./
C----------------------------------------------------------------------
      GOOD_EL=.TRUE.
      IF(PASS) GOTO 999
      THETA=Q(LPELC+8)
      PHIE=Q(LPELC+10)
      LHMTE=LQ(LPELC-1)
      CHSQ=Q(LHMTE+7)
      EISO=(Q(LPELC+16)-Q(LPELC+17))/(Q(LPELC+17)+.001)
      LZTRK=LQ(LPELC-3)
      LZFIT=LQ(LZTRK-1)
      DPHI=ABS(PHIE-Q(LZFIT+10))
      DTH= ABS(THETA-Q(LZFIT+13))
      IF(DPHI.GT.PI) DPHI=TWOPI-DPHI
      GOOD_EL=CHSQ.LT.100.
      ETA=ABS(Q(LPELC+9))
      IF(ETA.GT.2.6) GOOD_EL=CHSQ.LT.60.
      IF(ETA.GT.3.2) GOOD_EL=CHSQ.LT.35.
      IF(EISO.GT.0.15) GOOD_EL=.FALSE.
      IF(ABS(DPHI).GT.0.15) GOOD_EL=.FALSE.
      IF(ABS(DTH).GT.0.15) GOOD_EL=.FALSE.
      GOTO 999
C
      ENTRY GOOD_EL_SET(VALUE)
      PASS=VALUE
  999 RETURN
      END
