      SUBROUTINE ADD_W_QUANS(MORE_NAMES,MORE_QUANS,NUM_MORE)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : ADD TO NTUPLE QUANTITIES CONNECTED TO THE W
C-
C-   Inputs  : CACL, PNUT ETC. LCLUS POINTS TO PELC?PPHO IN QUESTION
C-   Outputs : ADD NAMES AND QUANS TO MORE_NAMES, MORE_QUANS
C-             BUMP UP NUM_MORE
C-   Controls:
C-
C-   Created  21-SEP-1992   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKC.INC'
      INTEGER LCLUS
      EQUIVALENCE (CSTLNK(LNKMX-1),LCLUS)
      CHARACTER*8 MORE_NAMES(*)
      REAL    MORE_QUANS(*)
      INTEGER NUM_MORE,NJETS_NOEP
      REAL    METX,METY,MET,SET,SIG_ET,TR_MASS
      INTEGER GZPNUT
      REAL ETX,ETY,ET,PT_WX,PT_WY,PT_W
      INTEGER NJETS,IER
      CHARACTER*32 ALG_NAME
      REAL    TEMPLATE(50)
      SAVE ALG_NAME,TEMPLATE
      DATA ALG_NAME /'CONE_JET'/
      DATA TEMPLATE /50*0.0/
      INTEGER IVERS,I
      REAL    E(7),THETA,PHI,ETA
      INTEGER MAX_JETS
      PARAMETER( MAX_JETS = 6 )
      REAL    ET_JETS(2,MAX_JETS)  !ET and EM fraction
      CHARACTER*8 ETNAME,EMNAME
C----------------------------------------------------------------------
      LPNUT = GZPNUT(0) !LATEST BANK BOOKED
      METX = Q(LPNUT+3)
      METY = Q(LPNUT+4)
      MET = Q(LPNUT+7)
      SET = Q(LPNUT+14)
      IF (SET.EQ.0.0 ) THEN
        SET = 1.0
      ENDIF

      IF ( SET.GT.0.0 ) THEN
        SIG_ET = MET/SQRT(SET)
      ELSE
        SIG_ET = 0.0
      ENDIF
C
      ETX = Q(LCLUS+3)
      ETY = Q(LCLUS+4)
      ET = Q(LCLUS+7)
      TR_MASS = 2.0*(MET*ET-METX*ETX-METY*ETY)
      IF ( TR_MASS.GT.0.0 ) THEN
        TR_MASS = SQRT(TR_MASS)
      ENDIF
      PT_WX = METX + ETX
      PT_WY = METY + ETY
      PT_W = SQRT(PT_WX**2 + PT_WY**2)
C
      MORE_NAMES(1+NUM_MORE) = 'MET'
      MORE_NAMES(2+NUM_MORE) = 'SET'
      MORE_NAMES(3+NUM_MORE) = 'EMET'
      MORE_NAMES(4+NUM_MORE) = 'PT_W'
      MORE_NAMES(5+NUM_MORE) = 'TR_MASS'
      MORE_NAMES(6+NUM_MORE) = 'SIG_ET'
C
      MORE_QUANS(1+NUM_MORE) =  MET
      MORE_QUANS(2+NUM_MORE) =  SET
      MORE_QUANS(3+NUM_MORE) =  ET
      MORE_QUANS(4+NUM_MORE) =  PT_W
      MORE_QUANS(5+NUM_MORE) = TR_MASS
      MORE_QUANS(6+NUM_MORE) = SIG_ET
C
      NUM_MORE = NUM_MORE + 6
C
      CALL SET_CAPH(ALG_NAME,TEMPLATE,IER)
      IF ( IER .LT. 0 ) THEN
        CALL ERRMSG('ADD_W_QUANS','BAD_JETS',
     &    'Problem selecting jets','W')
        GOTO 999
      ENDIF
C
      CALL UZERO(ET_JETS,1,2*MAX_JETS)
C
      CALL GTJETS_NOEP  !SET JNEP MODE
      CALL GTJETS_TOTAL(NJETS,IER)
      NJETS_NOEP = 0   !JETS NOT EM
C
      DO I = 1 , NJETS
        CALL GTJETS(I,IVERS,E,THETA,PHI,ETA,IER)
        IF ( IER.EQ.0 ) THEN
          NJETS_NOEP = NJETS_NOEP + 1
          IF ( NJETS_NOEP.LE.MAX_JETS ) THEN
            ET_JETS(1,NJETS_NOEP) = E(5)   !ET
            ET_JETS(2,NJETS_NOEP) = E(7)   !EM FRACTION
          ENDIF
        ENDIF
      ENDDO
C
      MORE_NAMES(1+NUM_MORE) = 'NUM_JETS'
      MORE_QUANS(1+NUM_MORE) =  NJETS_NOEP
      NUM_MORE = NUM_MORE + 1
C
      DO I = 1 , MAX_JETS
        WRITE(ETNAME,11)I
   11   FORMAT('ET_JT',I2.2)
C
        WRITE(EMNAME,12)I
   12   FORMAT('EMRAT',I2.2)
        MORE_NAMES(1+NUM_MORE) =  ETNAME
        MORE_QUANS(1+NUM_MORE) =  ET_JETS(1,I)
        NUM_MORE = NUM_MORE + 1
C
        MORE_NAMES(1+NUM_MORE) =  EMNAME
        MORE_QUANS(1+NUM_MORE) =  ET_JETS(2,I)
        NUM_MORE = NUM_MORE + 1
      ENDDO
C
  999 RETURN
      END
