      LOGICAL FUNCTION VTZERO_JSUM()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : fits gaussian and gets tzeros for each cha-
C-                         nnel.
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created   1-JUN-1992   V. D. Elvira and Ed Oltman
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER NUM_CRA, NUM_CHA
      PARAMETER ( NUM_CRA=10 )
      PARAMETER ( NUM_CHA=127 )
      INTEGER NX
      INTEGER CHA,CRATE,CR
      INTEGER LRS(3),SCTS0(16),SCTS1(32),NLRS,NSCT(3),WRE(8),NWRE,LAYER,
     &  SECTOR,WIRE,END,I,J,IW,L,ID,DID,LOGI
      INTEGER START,GROUP,LCHAN
      INTEGER IER
      INTEGER CH(NUM_CRA,0:NUM_CHA),LGL(NUM_CRA,0:NUM_CHA)
C----------------------------------------------------------------------
      REAL C,AV,SD,AVT(10),SDT(10),AVS(10),SDS(10),CHI2,SIG(3),IMI,IMA
      REAL FRAC,TZERO(0:2000),T0VTMW(0:2,0:31,0:7,0:1),SIVTMW(0:2,0:31,
     &  0:7,0:1),THT,THS
      REAL BS,IMIP,HI,TOP,BOT,X,YMI,YMA,TRGOFF
C----------------------------------------------------------------------
      LOGICAL SINGLE,CRAST,ROWST,HEXIST,OK
C----------------------------------------------------------------------
      CHARACTER*5 CHANNEL
      CHARACTER*40 MSG,CHT
C----------------------------------------------------------------------
      DATA LRS/0,1,2/
      DATA SCTS0/0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15/
      DATA SCTS1/0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,
     &  21,22,23,24,25,26,27,28,29,30,31/
      DATA WRE/0,1,2,3,4,5,6,7/
C----------------------------------------------------------------------
      CALL EZPICK('VTRAKS_RCP')
      CALL EZGET('TRGOFF',TRGOFF,IER)
      CALL EZRSET
      CALL EZPICK('VTXCOFF_RCP')
      CALL EZGET('NX',NX,IER)
      CALL EZGET('IMI',IMI,IER)
      CALL EZGET('IMA',IMA,IER)
      CALL EZGET('FRAC',FRAC,IER)
      CALL EZGET('SINGLE',SINGLE,IER)
      CALL EZGET('CRAST',CRAST,IER)
      CALL EZGET('ROWST',ROWST,IER)
      CALL EZGET('THT',THT,IER)
      CALL EZGET('THS',THS,IER)
      CALL EZRSET
C----------------------------------------------------------------------
      VTZERO_JSUM = .TRUE.
C----------------------------------------------------------------------
      NLRS=3
      NSCT(1)=16
      NSCT(2)=32
      NSCT(3)=32
      NWRE=8
C----------------------------------------------------------------------
C- For each channel, finds the leading edge of the drift time distrib.
C- and calculates the upper time limit of the fitted function from a
C- weighted average of the absiza.
C----------------------------------------------------------------------
      GROUP = 1
      LCHAN = 1
      DO 4 END=0,1
        DO 1 I=1, NLRS
          LAYER=LRS(I)
          DO 2 J=1,NSCT(I)
            IF (LAYER.EQ.0) THEN
              SECTOR=SCTS0(J)
            ELSE
              SECTOR=SCTS1(J)
            ENDIF
            IF (LCHAN .GT. 128) THEN
              LCHAN = 1
              GROUP = GROUP + 1
            ENDIF
            DO 3 IW=1,NWRE
              WIRE=WRE(IW)
              ID=10000+512*LAYER+16*SECTOR+2*WIRE+END
              DID=20000+512*LAYER+16*SECTOR+2*WIRE+END
              BS=(IMA-IMI)/FLOAT(NX)
              IMIP=IMI-0.5*BS-TRGOFF
              DO L = 1,NX
                IF (HI(ID,L) .NE. 0.) THEN
                  START = L
                  GO TO 5
                ENDIF
              ENDDO
    5         TOP = 0.
              BOT = 0.
              DO L = START,NX
                X = HI(ID,L)
                TOP = TOP + (IMIP + FLOAT(L)*BS)*X
                BOT = BOT + X
              ENDDO
C----------------------------------------------------------------------
C- If there are enough entries in the channel, the histo is blowed up
C- and copied into memory (ID-->DID). A gaussian is fitted to them.
C----------------------------------------------------------------------
              LOGI=ID-10000
              WRITE(CHT,100) LOGI
              CHT='Drift time dist.'//CHT
              IF (BOT .GE. 10.) THEN
                AV = TOP/BOT
                CALL VHBLOW(ID,DID,CHT,.FALSE.,0.,AV,YMI,YMA,OK)
                CALL HFITGA(DID,C,AV,SD,CHI2,102,SIG)
              ELSE
                AV=0.
                SD=0.
                WRITE(CHANNEL,100) ID
                MSG='not enough entries histo='//CHANNEL
                CALL ERRMSG('ERROR','VTZERO_JSUM',MSG,'S')
              ENDIF
              IF (HEXIST(ID)) CALL HDELET(ID)
C----------------------------------------------------------------------
C- Crate and electronic address are gotten. Then TZERO and SIGMA are
C- copied into ARRAYS and different kinds of histograms are filled or
C- not depending on the values of logical switches.
C----------------------------------------------------------------------
              CALL VTX_TRANS_CHAN(LOGI,CRATE,CHA,1)
              CR=INT(FLOAT(CRATE+7)/10.)
              TZERO(CHA)=AV-SD*SQRT(-2*LOG(FRAC))
              T0VTMW(LAYER,SECTOR,WIRE,END)=TZERO(CHA)
              SIVTMW(LAYER,SECTOR,WIRE,END)=SD
              CALL HF1(100+10*(CR-1),FLOAT(CHA),TZERO(CHA))
              CALL HF1(100+10*(CR-1)+1,FLOAT(CHA),SD)
              CALL HF1(100+10*(CR-1)+2,TZERO(CHA),1.)
              CALL HF1(100+10*(CR-1)+3,SD,1.)
              IF (ROWST) THEN
                CALL HF1(200+10*(GROUP-1),FLOAT(LCHAN),TZERO(CHA))
                CALL HF1(200+10*(GROUP-1)+1,FLOAT(LCHAN),SD)
              ENDIF
              LCHAN=LCHAN+1
              IF (.NOT.SINGLE) THEN
                IF (HEXIST(DID)) CALL HDELET(DID)
              ENDIF
    3       CONTINUE
    2     CONTINUE
    1   CONTINUE
    4 CONTINUE
C------------------------------------------------------------------------
C-  Fix bad tzero values by replacing them with a crate average value;
C-  sigma is set to zero
C------------------------------------------------------------------------
      DO CR=1,10
        CALL HFITGA(100+10*(CR-1)+2,C,AVT(CR),SDT(CR),CHI2,102,SIG)
        CALL HFITGA(100+10*(CR-1)+3,C,AVS(CR),SDS(CR),CHI2,102,SIG)
      ENDDO
      DO END=0,1
        DO I=1, NLRS
          LAYER=LRS(I)
          DO J=1,NSCT(I)
            IF (LAYER.EQ.0) THEN
              SECTOR=SCTS0(J)
            ELSE
              SECTOR=SCTS1(J)
            ENDIF
            DO IW=1,NWRE
              WIRE=WRE(IW)
              LOGI=512*LAYER+16*SECTOR+2*WIRE+END
              CALL VTX_TRANS_CHAN(LOGI,CRATE,CHA,1)
              CR=INT(FLOAT(CRATE+7)/10.)
              IF ((ABS(T0VTMW(LAYER,SECTOR,WIRE,END)-AVT(CR)).GT.
     &          (THT*SDT(CR))) .OR.
     &          (ABS(SIVTMW(LAYER,SECTOR,WIRE,END)-AVS(CR)).GT.THS))
     &          THEN
                T0VTMW(LAYER,SECTOR,WIRE,END)=AVT(CR)
                SIVTMW(LAYER,SECTOR,WIRE,END)=0.
              ELSE IF (T0VTMW(LAYER,SECTOR,WIRE,END).EQ.0.) THEN
                T0VTMW(LAYER,SECTOR,WIRE,END)=AVT(CR)
                SIVTMW(LAYER,SECTOR,WIRE,END)=0.
              ENDIF
            ENDDO
          ENDDO
        ENDDO
      ENDDO
C------------------------------------------------------------------------
      IF (.NOT.CRAST) THEN
        DO CR=1,10
          IF (HEXIST(100+10*(CR-1))) CALL HDELET(100+10*(CR-1))
          IF (HEXIST(100+10*(CR-1)+1)) CALL HDELET(100+10*(CR-1)+1)
          IF (HEXIST(100+10*(CR-1)+2)) CALL HDELET(100+10*(CR-1)+2)
          IF (HEXIST(100+10*(CR-1)+3)) CALL HDELET(100+10*(CR-1)+3)
        ENDDO
      ENDIF
C
C ****  Write results to output file(s)
C
      CALL VTMW_WRITE(T0VTMW,SIVTMW)
C------------------------------------------------------------------------
  100 FORMAT(I5)
C------------------------------------------------------------------------
  999 RETURN
      END
