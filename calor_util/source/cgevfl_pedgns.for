      SUBROUTINE CGEVFL_PEDGNS(RCP_FILE,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :  Fill CGEV with CALIB gains and pedestals.
C-                          channels with (pulser signals le 0) do not 
C-                          have calib gains applied. 
C-                          Bad channels are handled in CGEVFL_BAD.
C-
C-   Inputs  : RCP_FILE [C] RCP_FILE for control parameters
C-   Outputs : IER          0 = OK
C-   Controls: RCP_FILE
C-
C-   Created   7-APR-1992   Chip Stewart
C-   Updated   9-JUN-1992   Chip Stewart  Pulser signal <= 0  
C-   Updated   9-FEB-1994   Jan Guida  Add call to CMODGN for calib gain
C-                                      corrections 
C-   Updated  24-FEB-1994   Jan Guida  Set PLSCOR FALSE before calling CMODGN
C-   Updated  19-MAR-1994   J Guida    Fill IPED_CRT and IGNS_CRT for determing
C-                                      when a new ped/gns run was read from 
C-                                      the database, for each crate 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) RCP_FILE
      INTEGER IER
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$PARAMS:CAL_ADC_NO.PARAMS'
      INCLUDE 'D0$PARAMS:BYTE_ORDER.PARAMS'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:STP_ZLINKA.INC'
      INCLUDE 'D0$PARAMS:PRTPDG.DEF'
      INCLUDE 'D0$INC:CUNFLG.INC'
      LOGICAL DO_GNSCOR,DO_ADC_TO_GEV,DO_ZERO_SUPRESS,DO_PEDSUB
      LOGICAL MC,EZERR,TB,CEXIST,LPED,LGNS,DO_CMODGN
      INTEGER ADDR,MAX_ADDR,SEQ, ISC,GZCPDH,GZCGNH,ICAB,ICRT,CARD,ISHFT
      INTEGER I,J,K
      INTEGER LOC,ICRATE,SCALE,NTYPE,ITYPE,LCRATE,IZCARD(0:1)
      INTEGER NCRATE,ICAD,IZ,LCARD,ICARD,DEPTH,IDATA,ADC,BLS,ROTOW,NG
      INTEGER IETA,IPHI,ILYR,CRATE,NH,NR,NS,ND,NV,LZFIND,ICGEV
      INTEGER LCGEV, GZCGEV,MAX, CELL_FBKCAP,FBKCAP,IRUN,RUNNO
      INTEGER IPED_CRT,IGNS_CRT,ICABLE,ICR,IPCRT,IGCRT
      REAL    GAIN_NORM,SIGMA_CUTOF,EJUNK, VALUE,SIGMA,GAIN_NORM_NWA
      REAL    CALIB_NORM,PULSER,PED,EM_GAIN,CGAIN
      CHARACTER CTYPE*4,ERR_ID*80,MSG*80
      LOGICAL FIRST,BTEST,DROP_BANKS
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      IF ( FIRST ) THEN
        FIRST = .FALSE.
C
C ****  READ RCP_FILE
C
        CALL EZLOC(RCP_FILE,LOC)
        IF(LOC.EQ.0) CALL INRCP(RCP_FILE,IER)
        CALL EZPICK(RCP_FILE)
        IF(EZERR(IER)) THEN
          CALL ERRMSG('NO_RCP_FILE_CGEV','CGEVFL',RCP_FILE,'W')
        ELSE
          CALL EZGET_l ('DO_GNSCOR',         DO_GNSCOR,IER)
          CALL EZGET ('EM_GAIN',           EM_GAIN,IER)  
          CALL EZGET ('GAIN_NORMALIZATION',GAIN_NORM,IER)
          CALL EZGET ('GAIN_NORMALIZATION_NWA',GAIN_NORM_NWA,IER)
          CALL EZGET_l ('DO_PEDSUB',         DO_PEDSUB,IER)
          CALL EZGET_l ('DO_ADC_TO_GEV',     DO_ADC_TO_GEV,IER)
          CALL EZGET_l ('DO_ZERO_SUPRESS',   DO_ZERO_SUPRESS,IER)
          CALL EZGET ('SIGMA_CUTOFF',      SIGMA_CUTOF,IER)
          CALL EZGET_l ('DROP_CALIB_BANKS',  DROP_BANKS,IER)
          IF(IER.NE.0) DROP_BANKS = .TRUE. ! DEFAULT DROP
          CALL EZGET_l ('DO_CMODGN',         DO_CMODGN,IER)
        END IF
        CALL EZRSET
        CALL CADPAK(NADCC-1,NBLSC-1,NEFC-1,NDEPTC-1,0,0,MAX_ADDR)
        MAX_ADDR = MAX_ADDR / 4
        ICRATE = 17
        ADDR = 0
        SCALE = 0
        LCGEV = GZCGEV ()
        CALL GTCGEV_HEAD(NH,NV,NS,NR,ND,IER)
        TB = BTEST(D0VSN,6)         ! Check for NWA CAD bank
        IF (TB) THEN
          ICRATE = 87
          GAIN_NORM = GAIN_NORM_NWA
          IF(ABS(GAIN_NORM-1900.0).GT.1.0) THEN
            WRITE(MSG,10)GAIN_NORM
   10       FORMAT(' SET ',F9.1,
     &        '- USE 1900 TO HAVE NWA CALIBRATION')
           CALL ERRMSG('GAIN_NORMALIZATION_NWA','CGEVFL_PEDGNS',MSG,'W')
          END IF
        ELSE
          IF(ABS(GAIN_NORM-6000.0).GT.1.0) THEN
            WRITE(MSG,20)GAIN_NORM
   20       FORMAT(' SET ',F9.1,
     &      '- USE 6000 TO HAVE D0 CALIBRATION')
            CALL ERRMSG('GAIN_NORMALIZATION','CGEVFL_PEDGNS',MSG,'W')
          END IF
        END IF
      END IF
C----------------------------------------------------------------------
      IER = 0
      IRUN = RUNNO()
      IPED_CRT = 0
      IGNS_CRT = 0
C
      DO ICAB = ADCR00,ADCR01
        DO 40, ICRT = 0,NADCRC-1
          ICRATE = ICRT*10 + ICAB
          IF (TB) THEN
            IF( ICRT.GT. 0) GOTO 50
            ICRATE = 87
          END IF
          CALL CGEVFL_FETCH(RCP_FILE,ICRATE,IER)
          IF(IER.LE.0) THEN
            WRITE(MSG,'('' FOR CRATE '',I5)')ICRATE
            IF(IER.EQ.0) THEN
              CALL ERRMSG('PED/GNS in PLACE','CGEVFL_PEDGNS',MSG,'I')
            ELSE
              CALL ERRMSG('ERROR in CGEVFL_FETCH','CGEVFL_PEDGNS',
     &          MSG,'W')
            END IF
            GOTO 40
          END IF
C
          LPED = BTEST(IER,0)
          LGNS = BTEST(IER,1)
          ICABLE = 0
          IF(MOD(ICRATE,10).EQ.8) ICABLE = 1
          ICR=ICRATE/10 + 1 + ICABLE*6       ! Get crate number 1-12
          IF (LPED) IPED_CRT = IBSET(IPED_CRT,ICR-1)
          IF (LGNS) IGNS_CRT = IBSET(IGNS_CRT,ICR-1)
C
          DO SCALE = 0,1
            LCGEV = GZCGEV()
            IF((LCGEV.GT.0)) LCGEV=LZFIND(IDVSTP,LCGEV,SCALE,3)
            IF(LCGEV.LE.0) THEN
              WRITE(MSG,19)SCALE
   19         FORMAT(' NO GEV BANK FOR SCALE',I3)
              CALL ERRMSG('ERROR_FROM_CGEVFL','CGEVFL_PEDGNS',MSG,'W')
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
                IF(.NOT.CEXIST(IETA,IPHI,ILYR))            GOTO 100
                ICGEV=NH+
     &            NR*(ILYR-1+(IPHI-1)*NLYRL+(IETA+NETAL)*NPHIL*NLYRL)

                IF(DO_GNSCOR.AND.LGNS) THEN
                  CALL GT_PED_GNS_ADDR(3,CRATE,CARD,SEQ,SCALE,
     &              PULSER,SIGMA,IER)
                  IF (IER.NE.0) GOTO 100
                  FBKCAP = CELL_FBKCAP(IETA,IPHI,ILYR)
                  CGAIN = PULSER
                  IF(PULSER.GT.0) THEN
                    PLSCOR = .FALSE.
                    IF (DO_CMODGN .AND. IRUN.LT.74360)
     &                CALL CMODGN(IRUN,FBKCAP,PULSER, 
     &                ICRATE,ADC,BLS,ROTOW,DEPTH,CGAIN)
                    PULSER = CGAIN
                    C(LCGEV+ICGEV+1) = C(LCGEV+ICGEV+1)*GAIN_NORM/PULSER
                  ELSE 
                    ISC = SCALE
                    IF(SCALE.EQ.0) ISC = 8
                    WRITE(MSG,'(''PULSER RESPONSE ='',E12.3)')PULSER
                    CALL CAHITS_ERRMSG(3,IETA,IPHI,ILYR,SCALE,
     &                CRATE,IDATA,MSG,IER)
                    IF (FBKCAP.EQ.10) 
     &                C(LCGEV+ICGEV+1) = C(LCGEV+ICGEV+1)*EM_GAIN
                  END IF
                END IF
                IF(LPED.AND.(NR.GT.1)) THEN
                  CALL GT_PED_GNS_ADDR(1,CRATE,CARD,SEQ,SCALE,
     &              PED,SIGMA,IER)
                  IF (IER.NE.0) GOTO 100
                  C(LCGEV+ICGEV+2) = SIGMA
                  IF(NR.GE.3) C(LCGEV+ICGEV+3) = PED
                END IF
  100         CONTINUE
            END DO
C
C ****  CALIB BAD CHANNELS ... GAINS, PEDS
C
            K = MAX(NR-1,2)
            DO I = 1, NR, 2
              LCRATE = GZCPDH ()              !PEDS
              IF(I.EQ.1) LCRATE = GZCGNH ()   !GAINS 
              IF(LCRATE.GT.0) LCRATE = LZFIND(IDVSTP,LCRATE,CRATE,9)
              IF(LCRATE.GT.0) THEN
                LCARD  = LC(LCRATE-(SCALE+1))
                IF(LCARD.GT.0) CALL CGEVFL_BAD(RCP_FILE,CRATE,LCARD,IER)
              END IF
            END DO
          END DO
          IF (DROP_BANKS) THEN
            LCPDH = GZCPDH ()
            IF(LCPDH.GT.0)CALL MZDROP(IXSTP,LCPDH,'L')
            LCGNH = GZCGNH ()
            IF(LCGNH.GT.0)CALL MZDROP(IXSTP,LCGNH,'L')
          END IF
   40   CONTINUE
      END DO
   50 CONTINUE
      CALL CGEVFL_DBFINISH 
  999 CONTINUE
      RETURN
C**********************************************************************
C
      ENTRY CGEVFL_CRTVAL(IPCRT,IGCRT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-
C-   Inputs  : none
C-   Outputs : IPCRT - Sets a bit for each crate, when new ped run is read
C-             IGCRT - Sets a bit for each crate, when new gains run is read
C-   Controls: none
C-
C-   Created  19-MAR-1994   J. Guida
C-
C----------------------------------------------------------------------
      IPCRT = IPED_CRT
      IGCRT = IGNS_CRT
C
 1999 RETURN
C**********************************************************************
      ENTRY CGEVFL_PEDGNS_RESET
      FIRST = .TRUE.
      RETURN
      END
