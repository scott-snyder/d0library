      FUNCTION TB90_NTUPLE ()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-
C-   Returned value  : 
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  24-OCT-1990   DrMikie
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL FIRST_SETUP
      LOGICAL TB90_NTUPLE,TB90_NTUPLE_SETUP,TB90_NTUPLE_END
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:TB90_NTUPLE.INC'
C
      INTEGER NPRIME, ISTAT, IER
      CHARACTER*8 CHTAGS(NTUPLEN)
C
      LOGICAL OK,CAMAC
      INTEGER TRIGGER,MTMASK
      REAL      PROJ_Y1, PROJ_Y2, PROJ_DX, PROJ_UX, DELTAX
      INTEGER   NTDC,NADC,NSCALER,NLDTDC
      INTEGER   TAG_WORD,HALL
      INTEGER TDC_WORDS(50)
      INTEGER ADC_WORDS(50)
      INTEGER HVLT_WORDS(50)
      INTEGER STRING_EAT(50)
      INTEGER LDTDC_WORDS(50)
      INTEGER   SCALERS(50)
C
      CHARACTER MSG_STRING*80, HROUT_FILE*80, COMMAND*80, RUN*4
      CHARACTER MSG*40
      INTEGER IEVNTS,NCH,IEVT
      INTEGER NTRY,JER,NV,NR,I,J,K,L,II,JJ,KK,IRUN,UNIT
      INTEGER NLYR,IDETA,IDPHI
      INTEGER IETA,IPHI,ILYR,BITS,ICHAN
      REAL    EMLYR(4),IHLYR(11:15),EMTOTAL,IHTOTAL,MHTOTAL,ENERGY
      REAL    EMLYR_CUT(4),IHLYR_CUT(11:15),EMIH1_TIGHT_CUT,ETOTAL
      REAL    Gain_10pf(4),EMIH1,EMIH1_CUT,IHEM_CUT,SFRAC_CORR(17)
      REAL    UPSTR_CUT,RETA,RPHI,MAX_ENERGY,LOW_CRAZY,HIGH_CRAZY
      REAL    ETA_ARRAY(15,14:39),PHI_ARRAY(15,64),EM_CUT
      REAL    EM3_ETA_ARRAY(27:78),EM3_PHI_ARRAY(129),META,MPHI
      REAL    ETA_BEAM,ETA_RANGE,PHI_BEAM,PHI_RANGE
      REAL    TIGHT_ETA_RANGE,TIGHT_PHI_RANGE
      REAL    ETA_IH_RANGE,PHI_IH_RANGE
      REAL    ETA_LOW,ETA_HIGH,PHI_LOW,PHI_HIGH
      REAL    ETA_LOW1,ETA_HIGH1,PHI_LOW1,PHI_HIGH1
      REAL    ETA_LOW2,ETA_HIGH2,PHI_LOW2,PHI_HIGH2
      REAL    ETOTSUM
C
      LOGICAL FIRSTCALL,FIRST,EZERR,EM,IH,MH
C
      INTEGER ICRATE,HEADER_LEN,SYNCH,CONTROL_WORD,STATUS,VERSION
      INTEGER PULSER,IDATA,IWORD,RUNNUM,RUNNO,PRUNNUM,IBITS
      REAL    PEDESTAL,PULSE_HEIGHT
C
      DATA MTMASK/7/

      DATA CHTAGS/'     NUX','     NDX','     NY ','  XCP_UX'
     &,'  XCP_DX','  XCP_Y ','  SLP_UX','  SLP_DX','  SLP_Y '
     &,' CHIT_DX',' CHIT_Y ','UX_ZBEND','DX_ZBEND',' Y_NW9_1'
     &,' Y_NW9_2','P_HALLPR','P_NOMINL',' B_GAUSS','    MUON'
     &,'    MIP ','    HALO','   CKV_9','   CKV_A','   CKV_2'
     &,'LDTDC1  ','LDTDC2  ','LDTDC3  ','LDTDC4  ','LDTDC5  '
     &,'LDTDC6  ','LDTDC7  ',' SCALER1',' SCALER2','  ADC1  '
     &,'  ADC2  ','  ADC3  ','  ADC4  ','  RL_ADC','  ADCMIP'
     &,' TDCMIP ',' TDCMU  ',' TDCSC3 ',' NXCRYO ',' NYCRYO '
     &,' CHI2_Y ',' CHI2_DX',' TDCCKV2','ADC_CKV9','ADC_CKVA'
     &,'TRIGGER ','EM_LYR1 ','EM_LYR2 ','EM_LYR3 ','EM_LYR4 ' 
     &,'IH_LYR1 ','IH_LYR2 ','IH_LYR3 ','IH_LYR4 ','IH_LYR5 ' 
     &,'EM_CUT1 ','EM_CUT2 ','EM_CUT3 ','EM_CUT4 ','IH_CUT1 '
     &,'IH_CUT2 ','IH_CUT3 ','IH_CUT4 ','IH_CUT5 ','MAX_EM3 ' 
     &,'EMTOTAL ','IHTOTAL ','MHTOTAL ','ETOTAL  ','EMIH1   '
     &,'EM_CUT  ','EMIH1CUT','EMIHTITE','IHEM_CUT','META', 'MPHI'/
C
      DATA FIRST_SETUP/.TRUE./
C----------------------------------------------------------------------
      TB90_NTUPLE = .TRUE.
C----------------------------------------------------------------------
      EMIH1_CUT       = 0.0
      EMIH1_TIGHT_CUT = 0.0
      EM_CUT          = 0.0
      IHEM_CUT        = 0.0
      MAX_ENERGY      = -10000.0
      META            = 0.0
      MPHI            = 0.0
      EMTOTAL         = 0.0
      IHTOTAL         = 0.0
      MHTOTAL         = 0.0
      FIRSTCALL=.TRUE.
      CALL VZERO(EMLYR(1),4)
      CALL VZERO(IHLYR(11),5)
      CALL VZERO(EMLYR_CUT(1),4)
      CALL VZERO(IHLYR_CUT(11),5)
      CALL VZERO(ETA_ARRAY(1,14),390)
      CALL VZERO(PHI_ARRAY(1,1), 960)
      CALL VZERO(EM3_ETA_ARRAY(27),52)
      CALL VZERO(EM3_PHI_ARRAY(1), 129)
C............................................................
      IEVNTS=IEVNTS+1
      RUNNUM=RUNNO()
C............................................................
C SET THE DIRECTORY (MAYBE...)
       CALL DHDIR('TB90_NTUPLE_RCP','HBOOK_DIRECTORY',IER,' ')
       IF (IER.NE.0) THEN
         CALL ERRMSG('DHDIRERROR','TB90_NTUPLE',
     &    ' ERROR SETTING HBOOK DIRECTORY ','W')
       ENDIF
C
C............................................................
      TRIGGER = IBITS(IQ(LHEAD+11),0,3)         ! .AND.MTMASK
      NTRY = 0
  1   CALL GTTCMC(TAG_WORD,HALL,NADC,ADC_WORDS,NTDC,TDC_WORDS,
     &            NSCALER,SCALERS,NLDTDC,LDTDC_WORDS,IER)
      IF( IER.NE.0 .AND. NTRY.LT.1) THEN
        NTRY = NTRY + 1                 ! 
        OK =  CAMAC (IER)
        GOTO 1
      ELSE IF( IER.NE.0) THEN
        CALL ERRMSG('TB90_NTUPLE','TB90_NTUPLE',
     &          ' NO CAMAC INFO ','W')        
        GOTO 999
      END IF
C----------------------------------------------------------------------
C
C- 02-OCT-90 MT FILL MISCELLANEOUS NTUPLE WORDS WITH USEFUL CAMAC DATA
C
      CALL CAMAC_NTUPLE(HALL,TAG_WORD,LDTDC_WORDS,ADC_WORDS,SCALERS,
     &                  TDC_WORDS,TRIGGER)
C----------------------------------------------------------------------
      CALL GTCAEP_HEADER(NV,NR,NCH,IER)
      IF (IER.NE.0)THEN
        WRITE(MSG,1002)IER
        CALL ERRMSG('BAD CAEP HEAD','TB90_NTUPLE',MSG,'W')
        GO TO 999
      ENDIF
C
C ***  Loop over actual hit cells
C
      DO 500 I=1,NCH
        CALL GTCAEP(FIRSTCALL,IETA,IPHI,ILYR,BITS,ENERGY,ICHAN,IER)
        IF(FIRSTCALL)FIRSTCALL=.FALSE.
        EM = .FALSE.
        IH = .FALSE.
        MH = .FALSE.
C
C ****  Check for crazies...
C
        IF (ENERGY.LT.LOW_CRAZY.or.ENERGY.GT.HIGH_CRAZY)THEN
          WRITE(MSG,1102)ENERGY,IETA,IPHI,ILYR
          CALL ERRMSG('BAD CAEP CELL','TB90_NTUPLE',MSG,'W')
          GO TO 500
 1102     FORMAT('CRAZY ENERGY ', F10.1,3I4)
        ENDIF

        IF (ILYR.GE.8.AND.ILYR.LE.10
     &    .OR.ILYR.GE.16
     &    .OR.IETA.LT.11               ! used to be 13 but missed MH
     &    .OR.ILYR.LT.1 ) THEN         ! Check for crazy addresses
          WRITE(MSG,1103)IETA,IPHI,ILYR
          CALL ERRMSG('BAD CAEP ADDR','TB90_NTUPLE',MSG,'W')
          GO TO 800
 1103     FORMAT('CRAZY ADDRESS ',3I4)
        ENDIF

        RETA = IETA
        RPHI = IPHI

        IF (ILYR.LE.7) EM = .TRUE.
        IF(EM) THEN            ! EM layers
          IF (IETA.LE.13) GOTO 800     ! not TB Load 1 data
          NLYR = ILYR
          IF (ILYR.GE.3.AND.ILYR.LE.6) NLYR = 3
          IF (ILYR.EQ.7) NLYR = 4
          IF (IETA.LE.26 .AND. IPHI.GE.29 .AND. IPHI.LE.34) THEN
            IF (ILYR.EQ.3 .OR. ILYR.EQ.4) RETA = IETA - 0.5  ! EM3 pads
            IF (ILYR.EQ.4 .OR. ILYR.EQ.6) RPHI = IPHI + 0.5  ! EM3 pads
          ENDIF
          ILYR = NLYR
        ELSE      !  IH or MH layers
          IF (ILYR.GE.11.AND.IETA.GE.21 ) THEN
            IH = .TRUE.
          ELSE IF (ILYR.GE.11.AND.IETA.LE.16) THEN
            MH = .TRUE.
          ELSE IF (ILYR+6.GT.IETA) THEN
            MH = .TRUE.
          ELSE
            IH = .TRUE.
          ENDIF
        ENDIF

        IF (MH) THEN
          NLYR = ILYR
          IF (ILYR.GE.11.AND.ILYR.LE.14) NLYR = 16
          IF (ILYR.EQ.15) NLYR = 17
          ILYR = NLYR
        END IF

        ENERGY=ENERGY*SFRAC_CORR(ILYR)  ! Sampling fraction correction

        IF (ILYR.LE.4 .AND. IPHI.GE.29 .AND. IPHI.LE.34) THEN
          ENERGY = GAIN_10PF(ILYR)*ENERGY       ! Correct for 10pF in EM3 & EM4
        ENDIF

        IF (EM) THEN
          EMTOTAL = EMTOTAL + ENERGY
          EMLYR(ILYR) = EMLYR(ILYR) + ENERGY
          IF ( ILYR.EQ.3 .AND. ENERGY.GT.MAX_ENERGY ) THEN ! maximum EM3 energy
            MAX_ENERGY = ENERGY
            META       = RETA
            MPHI       = RPHI
          ENDIF
        ELSE IF (IH) THEN
          IHLYR(ILYR) = IHLYR(ILYR) + ENERGY
          IHTOTAL = IHTOTAL + ENERGY
        ELSE IF (MH) THEN
          MHTOTAL  = MHTOTAL + ENERGY
          GOTO 500
        END IF

        ETA_ARRAY(ILYR,IETA) = ETA_ARRAY(ILYR,IETA) + ENERGY
        PHI_ARRAY(ILYR,IPHI) = PHI_ARRAY(ILYR,IPHI) + ENERGY
        IF (ILYR.EQ.3) THEN
          EM3_ETA_ARRAY(NINT(2*RETA))=EM3_ETA_ARRAY(NINT(2*RETA))+ENERGY
          EM3_PHI_ARRAY(NINT(2*RPHI))=EM3_PHI_ARRAY(NINT(2*RPHI))+ENERGY
        END IF

C Eta & phi cuts about ETA_BEAM & PHI_BEAM (eta=1.95 & phi=61 for EM benchmark)

        IF (RETA.GE.ETA_LOW .AND. RETA.LE.ETA_HIGH .AND.
     &      RPHI.GE.PHI_LOW .AND. RPHI.LE.PHI_HIGH) THEN
          IF (ILYR.LE.11) EMIH1_CUT = EMIH1_CUT + ENERGY ! EM+IH(1) energy
          IF (ILYR.LE.4)  THEN
            EMLYR_CUT(ILYR) = EMLYR_CUT(ILYR)+ ENERGY    ! EMi energy
            EM_CUT          = EM_CUT         + ENERGY    ! EM total energy
          ENDIF
        ENDIF

C Tight Eta & phi cuts about ETA_BEAM & PHI_BEAM

        IF (RETA.GE.ETA_LOW1 .AND. RETA.LE.ETA_HIGH1 .AND.
     &      RPHI.GE.PHI_LOW1 .AND. RPHI.LE.PHI_HIGH1 .AND. ILYR.LE.11)
     &     EMIH1_TIGHT_CUT = EMIH1_TIGHT_CUT + ENERGY        ! EM+IH(1) Energy

C IH Eta & phi cuts for IH showers - If we're outside the "keyhole" then:

        IF (RETA.LE.ETA_HIGH2 .AND. RETA.GE. ETA_LOW2 .AND.
     &      RPHI.GE.PHI_LOW2 .AND. RPHI.LE.PHI_HIGH2) THEN
          IHEM_CUT = IHEM_CUT + ENERGY   ! IH + EM energy in cut area
          IF (ILYR.GE.11) IHLYR_CUT(ILYR)=IHLYR_CUT(ILYR)+ENERGY
        ENDIF

  500 CONTINUE

      ETOTAL = EMTOTAL + IHTOTAL + MHTOTAL
      EMIH1  = EMTOTAL + IHLYR(11)     ! EM + IH(1) to include leakage energy
C
C ****  PUT OUT MESSAGE EVERY 10TH ANALYZED EVENT
C
      IF(MOD(IEVNTS,10).EQ.0) THEN
        WRITE(MSG_STRING,1301)IEVNTS,RUNNUM,ETOTAL
 1301   FORMAT(' TB90_NTUPLE > ',I5,' EVENTS, ',
     &    ' RUN ',I7,', TOTAL ENERGY SUM =' ,F10.2)
        CALL INTMSG(MSG_STRING)
      END IF
C
C- MT PUT VARIABLES INTO NTUPLE EVENT VECTOR
      XTUPLE(51) = EMLYR(1)
      XTUPLE(52) = EMLYR(2)
      XTUPLE(53) = EMLYR(3)
      XTUPLE(54) = EMLYR(4)

      XTUPLE(55) = IHLYR(11)
      XTUPLE(56) = IHLYR(12)
      XTUPLE(57) = IHLYR(13)
      XTUPLE(58) = IHLYR(14)
      XTUPLE(59) = IHLYR(15)

      XTUPLE(60) = EMLYR_CUT(1)
      XTUPLE(61) = EMLYR_CUT(2)
      XTUPLE(62) = EMLYR_CUT(3)
      XTUPLE(63) = EMLYR_CUT(4)

      XTUPLE(64) = IHLYR_CUT(11)
      XTUPLE(65) = IHLYR_CUT(12)
      XTUPLE(66) = IHLYR_CUT(13)
      XTUPLE(67) = IHLYR_CUT(14)
      XTUPLE(68) = IHLYR_CUT(15)

      XTUPLE(69) = MAX_ENERGY

      XTUPLE(70) = EMTOTAL
      XTUPLE(71) = IHTOTAL
      XTUPLE(72) = MHTOTAL
      XTUPLE(73) = ETOTAL

      XTUPLE(74) = EMIH1
      XTUPLE(75) = EM_CUT
      XTUPLE(76) = EMIH1_CUT
      XTUPLE(77) = EMIH1_TIGHT_CUT
      XTUPLE(78) = IHEM_CUT

      XTUPLE(79) = META
      XTUPLE(80) = MPHI
C
C- NTUPLE IS FULL, HFN IT OUT
      CALL HFN(10,XTUPLE)
C
      GOTO 900

  800 CONTINUE
      IER = -1         ! not TB load 1 data
      WRITE(MSG,1004)
      CALL ERRMSG('BADCAEPADDR','TB90_CALOR_NTUPLE',MSG,'W')
C................................................................
  900 CONTINUE
 1001 FORMAT(' TB90_CALOR_NTUPLE, Run ',I8,' Pedstl run ',I8,
     &       ' # EVENTS= ',I6)
 1002 FORMAT(' IER in GTCAEP_HEADER=',I3)
 1003 FORMAT(' JER FROM GTTBES = ',I5)
 1004 FORMAT(' INCORRECT ETA OR LAYER FOR TB LOAD 1')
 1101 FORMAT(' IN TB90_CALOR_NTUPLE, # CHANS IN CAEP BANK = ',I16)
C
  999 RETURN
C
      ENTRY TB90_NTUPLE_SETUP ()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-
C----------------------------------------------------------------------
      IF (FIRST_SETUP) THEN
        FIRST_SETUP = .FALSE.
        CALL INRCP('TB90_NTUPLE_RCP',IER)
        IF (IER.EQ.0) THEN
          CALL EZPICK('TB90_NTUPLE_RCP')
          CALL DHDIR('TB90_NTUPLE_RCP','HBOOK_DIRECTORY',IER,' ')
          IF (IER.NE.0) THEN
            CALL ERRMSG('DHDIR-NODIR','TB90_NTUPLE_SETUP',
     &          ' ERROR SETTING HBOOK DIRECTORY ','W')
          ENDIF
C
          IF(EZERR(IER)) GOTO 999
          CALL EZGET('NPRIME',NPRIME,IER)
          CALL EZGET('UPSTREAM_ENERGY_CUT',UPSTR_CUT,IER)
          CALL EZGET('CAPACITANCE_GAIN_CORR',GAIN_10PF,IER)
          CALL EZGET('SAMPLING_FRACTION_CORR',SFRAC_CORR,IER)
          CALL EZGET('LOW_ENERGY_CRAZY_LIMIT',LOW_CRAZY,IER)
          CALL EZGET('HIGH_ENERGY_CRAZY_LIMIT',HIGH_CRAZY,IER)
          CALL EZGET('ETA_OF_BEAM',ETA_BEAM,IER)
          CALL EZGET('ETA_RANGE_EM_SHOWER',ETA_RANGE,IER)
          CALL EZGET('TIGHT_ETA_RANGE_EM_SHOWER',TIGHT_ETA_RANGE,IER)
          CALL EZGET('ETA_RANGE_HAD_SHOWER',ETA_IH_RANGE,IER)
          CALL EZGET('PHI_OF_BEAM',PHI_BEAM,IER)
          CALL EZGET('PHI_RANGE_EM_SHOWER',PHI_RANGE,IER)
          CALL EZGET('TIGHT_PHI_RANGE_EM_SHOWER',TIGHT_PHI_RANGE,IER)
          CALL EZGET('PHI_RANGE_HAD_SHOWER',PHI_IH_RANGE,IER)
C EM SHOWER REGION
          ETA_LOW  = ETA_BEAM - ETA_RANGE/2.0
          ETA_HIGH = ETA_BEAM + ETA_RANGE/2.0
          PHI_LOW  = PHI_BEAM - PHI_RANGE/2.0
          PHI_HIGH = PHI_BEAM + PHI_RANGE/2.0
C TIGHTER EM SHOWER REGION
          ETA_LOW1  = ETA_BEAM - TIGHT_ETA_RANGE/2.0
          ETA_HIGH1 = ETA_BEAM + TIGHT_ETA_RANGE/2.0
          PHI_LOW1  = PHI_BEAM - TIGHT_PHI_RANGE/2.0
          PHI_HIGH1 = PHI_BEAM + TIGHT_PHI_RANGE/2.0
C HAD SHOWER REGION
          ETA_LOW2  = ETA_BEAM - ETA_IH_RANGE/2.0
          ETA_HIGH2 = ETA_BEAM + ETA_IH_RANGE/2.0
          PHI_LOW2  = PHI_BEAM - PHI_IH_RANGE/2.0
          PHI_HIGH2 = PHI_BEAM + PHI_IH_RANGE/2.0
C
          CALL HBOOKN(10,'TB90_EVENT_NTUPLE, V08-NOV-90/MT  ',NTUPLEN,
     &    ' ',NPRIME,CHTAGS) 
          CALL EZRSET
        ELSE
          CALL ERRMSG('INRCP-NO RCP','TB90_NTUPLE_SETUP',
     &       'NO TB90_RCP','W')
        END IF
      END IF
C
      TB90_NTUPLE_SETUP = .TRUE.
C
 1999 RETURN
C
C----------------------------------------------------------------------
C
      ENTRY TB90_NTUPLE_END
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-
C----------------------------------------------------------------------
      TB90_NTUPLE_END = .TRUE.
C
      CALL EZPICK('TB90_NTUPLE_RCP')
      CALL EZ_GET_CHARS('HROUT_FILE',L,HROUT_FILE,IER)
      CALL EZRSET
      IF (IER.NE.0) GOTO 2999
      IF (L.EQ.0) GOTO 2999
      IF (INDEX(HROUT_FILE,'NO').GT.0) GOTO 2999
      IRUN = RUNNUM/10000
      IRUN = IRUN * 10000
      IRUN = RUNNUM - IRUN
      WRITE(RUN,'(I4.4)') IRUN
      CALL SWORDS (HROUT_FILE,I,J,K)
      L = LEN(HROUT_FILE)
C
C ****  PICK YOUR FAVORITE MARKER TO STICK RUN AT
C
      IRUN = INDEX(HROUT_FILE,'*')
C
      IF (IRUN.EQ.0 )THEN
        HROUT_FILE = HROUT_FILE(I:J)//RUN
      ELSE
        HROUT_FILE = HROUT_FILE(I:IRUN-1)
     &    //RUN//HROUT_FILE(IRUN+1:J)
      END IF
C
      CALL DHDIR('TB90_NTUPLE_RCP','HBOOK_DIRECTORY',IER,' ')
C
C **** If HROUT_FILE exists then create a new one to write over
C
C
      CALL D0H_BUMP_VERSION(HROUT_FILE)
C
          CALL ERRMSG('TB90_NTUPLE_END','TB90_NTUPLE_END',
     &       'WRITING HROUT_FILE','W')
      CALL HRPUT(0,HROUT_FILE,' ')     ! Store all HISTOGRAMS

 2999 RETURN
      END
