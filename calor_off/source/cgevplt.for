      FUNCTION CGEVPLT ()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-
C-   Returned value  : Create NTUPLE with addressing and energy 
C-                     scale parameters from the CGEV bank. 
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created   6-MAY-1992   Chip Stewart
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL CGEVPLT,CGEVPLT_END
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$PARAMS:CAL_ADC_NO.PARAMS'     ! CAD bank params
      INCLUDE 'D0$INC:CUNFLG.INC'
      LOGICAL FIRST,CEXIST,TB,BTEST
      INTEGER IETA,IPHI,ILYR,ICGEV,LCGEV,GZCGEV,NH,NV,SCALE,NR,ND,IER
      INTEGER LOC,ID,NCGEV,LCSFW,LCSFC,GZCSFW,GZCSFC,ICSFC,ICSFW,LENF
      INTEGER MIN_ETA,MAX_ETA,MIN_PHI,MAX_PHI,MIN_LYR,MAX_LYR,LUNIT
      INTEGER ICYCLE,N,GZCSFH,LCSFH,MAP(17),NX,NT,I,J
      INTEGER LCGEV1,LCSFC1,ICAB,ICRATE,ICRT,CRATE,DEPTH,CARD,SEQ,ADDR
      INTEGER IDATA,ADC,BLS,ROTOW,ISC,NG,NS,LZFIND,IMOD,CAL_MODULE
      REAL    K,S,P,ETA,LYR,CGEV(3),XTUPLE(17),AW,CC,PULSER,PED,SIGMA
      REAL    A(10),XT(17)
      CHARACTER CHTAGS(17)*3,NTUPLE_FILE*80,TITLE*80,CTAGS(17)*3
      CHARACTER CMODULE(10)*4,MODULE*4
      SAVE FIRST
      DATA FIRST / .TRUE. /
      DATA CHTAGS/'ETA','PHI','LYR','SCL','K','W','C','S','P',
     &  'PLS','PED','SIG','CRT','ADC','BLS','ROT','DEP'/
C----------------------------------------------------------------------
      CGEVPLT = .TRUE.
      IF(.NOT. FIRST ) GOTO 999
      CALL GTCGEV_HEAD(NH,NV,NS,NR,ND,IER)
      IF (IER.NE.0) GOTO 999
      FIRST = .FALSE.
      CALL EZLOC('CGEVPLT_RCP',LOC)
      IF (LOC.EQ.0) CALL INRCP('CGEVPLT_RCP',IER)
      CALL EZPICK('CGEVPLT_RCP')
      CALL EZGET('MIN_ETA',MIN_ETA,IER)
      CALL EZGET('MAX_ETA',MAX_ETA,IER)
      CALL EZGET('MIN_LYR',MIN_LYR,IER)
      CALL EZGET('MAX_LYR',MAX_LYR,IER)
      CALL EZGET('MIN_PHI',MIN_PHI,IER)
      CALL EZGET('MAX_PHI',MAX_PHI,IER)
      CALL EZGETS('NTUPLE_FILE',1,NTUPLE_FILE,LENF,IER)
      CALL EZ_GET_CHARS('TAGS',NT,CTAGS,IER)
      NX = 0
      DO I =1, NT
        IF(CTAGS(I).EQ.CHTAGS(I)) THEN
          NX = NX + 1
          MAP(I) = NX
          CTAGS(NX) = CTAGS(I)
        END IF
      END DO
      CALL EZRSET
      CALL GTUNIT(77,LUNIT,IER)
      CALL HROPEN(LUNIT,'CGEVPLT',NTUPLE_FILE,'N',1024,IER)
      CALL HCDIR('//PAWC',' ')
      CALL HCDIR('//CGEVPLT',' ')
      IMOD = CAL_MODULE(1,1,MODULE)
      CALL EZPICK('CAL_MODULE_RCP')
      CALL EZ_GET_CHARS('MODULE_NAME',N,CMODULE,IER)
      CALL EZRSET
      DO SCALE = 0,1
        DO IMOD = 1,10
          ID = 100*SCALE + IMOD
          WRITE(TITLE,10)SCALE,CMODULE(IMOD)
   10     FORMAT('CGEVPLT SCALE =',I2,2X,A6)
          CALL HBOOKN (ID, TITLE , NX, 'CGEVPLT', 10000, CTAGS)
        END DO
      END DO
      CALL HBOOKN (11,'CGEV BLANK',NX,'CGEVPLT',10000,CTAGS)
      CALL EZRSET
      TB = BTEST(D0VSN,6)         ! Check for NWA CAD bank
C
C ****  LOOP OVER CALIB BANKS
C
      CALL  STP_INZLNK
      LCGEV1 = GZCGEV()
      LCSFW = GZCSFW()
      LCSFC1 = GZCSFC()
      LCGEV = LCGEV1
      DO 50, SCALE = 0,1
        IF((LCGEV.GT.0).AND.(SCALE.NE.NS)) 
     &   LCGEV=LZFIND(IDVSTP,LCGEV1,SCALE,3)
        IF(LCGEV.LE.0) THEN
          CALL ERRMSG('ERROR_FROM_CGEVPLT','CGEVPLT',
     &    'NO_CGEV BANK','W')
        END IF
        DO ICAB = 7,8
          DO ICRT = 0,5
            ICRATE = ICRT*10 + ICAB
            IF (TB) THEN
              IF( ICRT.GT. 0) GOTO 50
              ICRATE = 87
            END IF
            DO CARD = 0, NADCC-1
              DO 100, SEQ = 0, NDEPTC*NEFC*NBLSC-1
                DEPTH = MOD(SEQ,NDEPTC)
                ADDR = ISHFT(CARD,9)+16*((SEQ-DEPTH)/NDEPTC)+DEPTH
                IDATA = 2**18*ADDR
                CALL CADUPK(ICRATE,IDATA,CRATE,
     &            ADC,BLS,ROTOW,DEPTH,ISC,NG)
                CALL CADT_ADCPHY(CRATE,ADC,BLS,ROTOW,DEPTH,
     &            IETA,IPHI,ILYR,IER)
                IF(IER.NE.0)                               GOTO 100
                IF((ILYR.LT.MIN_LYR).OR.(ILYR.GT.MAX_LYR)) GOTO 100
                IF((IETA.LT.MIN_ETA).OR.(IETA.GT.MAX_ETA)) GOTO 100
                IF((IPHI.LT.MIN_PHI).OR.(IPHI.GT.MAX_PHI)) GOTO 100
                IF(.NOT.CEXIST(IETA,IPHI,ILYR))            GOTO 100
                ICSFW =ILYR+(IABS(IETA)-1)*NLYRL  ! 1 to 629
                ICSFC = IPHI+(IETA+NETAL)*NPHIL  
                AW  = C(LCSFW+1+ICSFW) 
                LCSFC = LCSFC1
                IF((LCSFC.GT.0)) LCSFC = LZFIND(IDVSTP,LCSFC1,ILYR,2)
                IF((LCSFC.GT.0)) THEN
                  CC  = C(LCSFC+2+ICSFC) 
                ELSE
                  CC = 1
                END IF
                CALL GT_PED_GNS_ADDR(3,CRATE,CARD,SEQ,SCALE,
     &            PULSER,SIGMA,IER)
                CALL GT_PED_GNS_ADDR(1,CRATE,CARD,SEQ,SCALE,
     &            PED,SIGMA,IER)
                CALL GTCGEV(ILYR,IPHI,IETA,SCALE,NCGEV,CGEV,IER)
                K = CGEV(1)
                S = CGEV(2)
                P = CGEV(3)
                XTUPLE(1) = IETA
                XTUPLE(2) = IPHI
                XTUPLE(3) = ILYR
                XTUPLE(4) = SCALE
                XTUPLE(5) = K
                XTUPLE(6) = AW
                XTUPLE(7) = CC
                XTUPLE(8) = S
                XTUPLE(9) = P
                XTUPLE(10) = PULSER
                XTUPLE(11) = PED
                XTUPLE(12) = SIGMA
                XTUPLE(13) = CRATE
                XTUPLE(14) = ADC
                XTUPLE(15) = BLS
                XTUPLE(16) = ROTOW
                XTUPLE(17) = DEPTH
                DO I = 1, 17
                  IF(MAP(I).GT.0) XT(MAP(I)) = XTUPLE(I)
                END DO
                IMOD = CAL_MODULE(IETA,ILYR,MODULE)
                ID = 100*SCALE + IMOD
                CALL HFN(ID,XT)
  100         CONTINUE
            END DO
          END DO
        END DO
   50 CONTINUE
      CALL HCDIR('//PAWC',' ')
      CALL HCDIR('//CGEVPLT',' ')
      CALL HROUT(0,ICYCLE,' ')
      CALL HREND('CGEVPLT')
      CLOSE(LUNIT)
      CALL rlunit(77,LUNIT,IER)
      CALL INTMSG(' NTUPLE STORED IN '//NTUPLE_FILE)
C----------------------------------------------------------------------
  999 RETURN
      END
