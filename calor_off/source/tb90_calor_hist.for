      FUNCTION TB90_CALOR_HIST()
C----------------------------------------------------------------------
C
C   Purpose and Methods : FILL STANDARD TB90 CALORIMETER HISTOGRAMS
C                         Called from EXAMINE hook: EXM_DO_ANALYSIS
C
C   Inputs  : None
C   Outputs : None
C   Controls: None
C
C-  Created   20-APR-1990   W.G.D.Dharmaratna, Chip Stewart
C-   Updated   2-JUN-1990   Ron Madaras, 10pf correction + more EM plots
C-   Updated   4-JUN-1990   Marcel Demarteau, added HBOOK directory structure
C-   Updated   5-JUN-1990   John Womersley & Ron Madaras, IH gain corr plus
C-                            EM & IH plots with eta,phi cuts.
C-   Updated   6-JUN-1990   Ron Madaras, Retrieve better EM3 spatial info
C-   Updated  25-JUL-1990   Chip Stewart  - MADE HOOKS INTO ENTRIES
C
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL TB90_CALOR_HIST
      LOGICAL TB90_CALOR_HIST_SETUP
      LOGICAL TB90_CALOR_HIST_END
      LOGICAL TB90_CALOR_HIST_BEGIN_RUN
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
C
      CHARACTER MSG_STRING*80, HROUT_FILE*80, COMMAND*80, RUN*4
      CHARACTER MSG*40
      LOGICAL OK
      INTEGER IEVNTS,NCH,IEVT
      INTEGER IER,JER,NV,NR,I,J,K,L,II,JJ,KK,IRUN,UNIT
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
      LOGICAL FIRSTCALL,FIRST,EZERR,EM,IH,MH,FIRST_SETUP,LEGO
      LOGICAL TRIGGER,TRIGGER_BIT_CHECK,BEAM
C
      INTEGER ICRATE,HEADER_LEN,SYNCH,CONTROL_WORD,STATUS,VERSION
      INTEGER PULSER,IDATA,IWORD,RUNNUM,RUNNO,PRUNNUM
      REAL    PEDESTAL,PULSE_HEIGHT
      REAL LEGO_ENERGY_CUT
      DATA    FIRST /.TRUE./,FIRST_SETUP/.TRUE./
C----------------------------------------------------------------------
      TB90_CALOR_HIST = .TRUE.
C............................................................
      IEVNTS=IEVNTS+1
      RUNNUM=RUNNO()
C.............................................................
C
      IF (FIRST) THEN
        FIRST = .FALSE.
        CALL EZPICK('TB90_CALOR_HIST_RCP')
        IF(EZERR(IER)) GOTO 999
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

        CALL EZGET('DO_LEGO',LEGO,IER)
        CALL EZGET('LEGO_ENERGY_CUT',LEGO_ENERGY_CUT,IER)
C
        CALL EZGET('SELECT_TRIGGERS',TRIGGER,IER)
        IF( IER.NE.0) THEN
          CALL ERRMSG('NOTRIGSWTCH','TB90_CALOR_HIST',
     &    'NO TRIGGER SWITCH IN TB90_CALOR_HIST_RCP','W')
          TRIGGER = .FALSE.
        END IF
C
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
        CALL EZRSET
        BEAM = .TRUE.
      ENDIF
C
      IF(TRIGGER) BEAM = TRIGGER_BIT_CHECK ('TB90_CALOR_HIST_RCP')
      IF(.NOT.BEAM) GOTO 999
      CALL DHDIR('TB90_CALOR_HIST_RCP','HBOOK_DIRECTORY',IER,' ')
      IF (IER.NE.0) THEN
        CALL ERRMSG('DHDIRERROR','TB90_CALOR_HIST',
     &    ' ERROR SETTING HBOOK DIRECTORY ','W')
      ENDIF
C
      CALL GTCAEP_HEADER(NV,NR,NCH,IER)
      IF (IER.NE.0)THEN
        WRITE(MSG,1002)IER
        CALL ERRMSG('BAD CAEP HEAD','TB90_CALOR_HIST',MSG,'W')
        GO TO 999
      ENDIF
C
C ***  Loop over actual hit cells
C
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
          CALL ERRMSG('BAD CAEP CELL','TB90_CALOR_HIST',MSG,'W')
          GO TO 500
 1102     FORMAT('CRAZY ENERGY ', F10.1,3I4)
        ENDIF

        IF (ILYR.GE.8.AND.ILYR.LE.10
     &    .OR.ILYR.GE.16
     &    .OR.IETA.LT.11               ! used to be 13 but missed MH
     &    .OR.ILYR.LT.1 ) THEN         ! Check for crazy addresses
          WRITE(MSG,1103)IETA,IPHI,ILYR
          CALL ERRMSG('BAD CAEP ADDR','TB90_CALOR_HIST',MSG,'W')
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
          IF (ILYR.EQ.3)  CALL HF2(5010,RETA+0.25,RPHI+0.25,ENERGY)
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

        IF (LEGO) THEN
          IF ( ABS(ENERGY).GT.LEGO_ENERGY_CUT ) THEN
            IF ( .NOT.MH ) THEN
              CALL HF2(30000+ILYR, RETA, RPHI, ENERGY)
            ELSE
              CALL HF2(30020, RETA, RPHI, ENERGY)
            ENDIF
          ENDIF
        END IF

  500 CONTINUE

      DO 10, ILYR = 1, 15
      IF(ILYR.LT.11 .AND. ILYR.GT.4) GOTO 10
      IDETA = 10000 + ILYR
      IDPHI = 10005 + ILYR
      IF(ILYR.NE.3) THEN
        DO IETA = 14, 39
          ENERGY = ETA_ARRAY(ILYR,IETA)
          CALL HF1(IDETA,IETA+0.25,ENERGY)
        END DO
        DO IPHI = 1, 64
          ENERGY = PHI_ARRAY(ILYR,IPHI)
          CALL HF1(IDPHI,IPHI+0.25,ENERGY)
        END DO
      ELSE
        DO IETA = 27, 78
          ENERGY = EM3_ETA_ARRAY(IETA)
          CALL HF1(IDETA,IETA/2.0+0.25,ENERGY)
        END DO
        DO IPHI = 1, 129
          ENERGY = EM3_PHI_ARRAY(IPHI)
          CALL HF1(IDPHI,IPHI/2.0+0.25,ENERGY)
        END DO
      END IF
   10 CONTINUE

      IEVT = IEVT + 1
      ETOTAL = EMTOTAL + IHTOTAL + MHTOTAL
      EMIH1  = EMTOTAL + IHLYR(11)     ! EM + IH(1) to include leakage energy
C
C ****  PUT OUT MESSAGE EVERY 10TH EVENT
C
      IF(MOD(IEVT,10).EQ.0) THEN
        WRITE(MSG_STRING,1301)IEVT,RUNNUM,ETOTAL
 1301   FORMAT(' TB90_CALOR_HIST > ',I5,' EVENTS, 
     &   RUN ',I7,', TOTAL ENERGY SUM =' ,F10.2)
        CALL INTMSG(MSG_STRING)
      END IF

      CALL HF1(1001,EMLYR(1),1.)       ! SEE NOTE BELOW ABOUT ADDING NEW HISTS
      CALL HF1(1002,EMLYR(2),1.)
      CALL HF1(1003,EMLYR(3),1.)
      CALL HF1(1004,EMLYR(4),1.)

      CALL HF1(1101,EMLYR_CUT(1),1.)
      CALL HF1(1102,EMLYR_CUT(2),1.)
      CALL HF1(1103,EMLYR_CUT(3),1.)
      CALL HF1(1104,EMLYR_CUT(4),1.)

      CALL HF1(1201,EMTOTAL,1.)
      CALL HF1(1202,EMIH1,1.0)
      CALL HF1(1203,EM_CUT,1.)
      CALL HF1(1204,EMIH1_CUT,1.0)

      CALL HF1(2001,IHLYR(11),1.)
      CALL HF1(2002,IHLYR(12),1.)
      CALL HF1(2003,IHLYR(13),1.)
      CALL HF1(2004,IHLYR(14),1.)

      CALL HF1(2005,IHLYR(15),1.)
      CALL HF1(2101,IHLYR_CUT(11),1.)
      CALL HF1(2102,IHLYR_CUT(12),1.)
      CALL HF1(2103,IHLYR_CUT(13),1.)

      CALL HF1(2104,IHLYR_CUT(14),1.)
      CALL HF1(2105,IHLYR_CUT(15),1.)
      CALL HF1(3000,MHTOTAL,1.)
      IF (EMTOTAL+IHTOTAL.LE.UPSTR_CUT) CALL HF1(3001,MHTOTAL,1.)

      CALL HF1(3002,IHTOTAL,1.)
      CALL HF1(3003,IHEM_CUT,1.0)
      IF (EM_CUT.LT.UPSTR_CUT)CALL HF1(3004,IHEM_CUT,1.0)
      CALL HF1(3005,ETOTAL,1.)

      CALL HF1(20000,EMIH1_TIGHT_CUT,1.0)
      CALL HF2(5005,META,MPHI,1.)
C
C ******  NOTE: If you add more histograms, especially those of a temporary
C ******        nature, please use ID numbers >20000, in order to keep the
C ******        current histograms arranged in a logical order.


      GOTO 900

  800 CONTINUE
      IER = -1         ! not TB load 1 data
      WRITE(MSG,1004)
      CALL ERRMSG('BADCAEPADDR','TB90_CALOR_HIST',MSG,'W')
C................................................................
  900 CONTINUE
 1001 FORMAT(' TB90_CALOR_HIST, Run ',I8,' Pedstl run ',I8,
     &       ' # EVENTS= ',I6)
 1002 FORMAT(' IER in GTCAEP_HEADER=',I3)
 1003 FORMAT(' JER FROM GTTBES = ',I5)
 1004 FORMAT(' INCORRECT ETA OR LAYER FOR TB LOAD 1')
 1101 FORMAT(' IN TB90_CALOR_HIST, # CHANS IN CAEP BANK = ',I16)
C
  999 CONTINUE
      RETURN
C
C
      ENTRY TB90_CALOR_HIST_SETUP
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : INITIALIZE TB90_CALOR_HIST PACKAGE
C-
C----------------------------------------------------------------------
      TB90_CALOR_HIST_SETUP = .TRUE.
C
C ****  BOOK HISTOS
C
      IF (FIRST_SETUP) THEN
        FIRST_SETUP = .FALSE.
        CALL INRCP('TB90_CALOR_HIST_RCP',IER)
        IF (IER.EQ.0) THEN
          CALL EZPICK('TB90_CALOR_HIST_RCP')
          CALL DHDIR('TB90_CALOR_HIST_RCP','HBOOK_DIRECTORY',IER,' ')
          IF (IER.NE.0) THEN
            CALL ERRMSG('DHDIR-NODIR','TB90_CALOR_HIST_SETUP',
     &          ' ERROR SETTING HBOOK DIRECTORY ','W')
          ENDIF
          CALL DO_HBOOK('HISTOGRAMS')
          CALL EZRSET
        ELSE
          CALL ERRMSG('INRCP-NO RCP','TB90_CALOR_HIST_SETUP',
     &       'NO TB90_CALOR_HIST_RCP','W')
        END IF
      END IF
C
 1999 RETURN
C
C
      ENTRY TB90_CALOR_HIST_END
C
      TB90_CALOR_HIST_END = .TRUE.
C
      CALL EZPICK('TB90_CALOR_HIST_RCP')
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
      CALL DHDIR('TB90_CALOR_HIST_RCP','HBOOK_DIRECTORY',IER,' ')
C
C **** If HROUT_FILE exists then create a new one to write over
C
C
      CALL D0H_BUMP_VERSION(HROUT_FILE)
C
      CALL HRPUT(0,HROUT_FILE,'NT')     ! Store all HISTOGRAMS

 2999 RETURN
C
      ENTRY TB90_CALOR_HIST_BEGIN_RUN ()
C
      TB90_CALOR_HIST_BEGIN_RUN = .TRUE.
C ****  copy TB90_CALOR_HIST_RCP to begin run division
C
      CALL BRCPFL('TB90_CALOR_HIST_RCP')
C
 3999 RETURN
      END
