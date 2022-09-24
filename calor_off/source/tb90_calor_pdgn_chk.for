      LOGICAL FUNCTION TB90_CALOR_PDGN_CHK()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Analyzes PED or GAIN file data to determine
C-                         problems.
C-
C-   Returned value  : True if OK
C-   Inputs  : none
C-   Outputs : none
C-   Controls: TB90_CALOR_PDGN_CHK_RCP file
C-
C-   Created  13-JUL-1990   Chip Stewart
C-   Updated 5-MAR-1994  Roy Thatcher Fixed to eliminate spurious D0entry
C-     Changed ENTRY to ENTRY1
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL TB90_CALOR_PDGN_CHK_SETUP,TB90_CALOR_PDGN_CHK_FINAL

      INTEGER IER,NCH,ICRATE,HEADER_LEN,SYNCH,CONTROL_WORD,STATUS
      INTEGER VERSION,PULSER,ICAD,IWORD,NCOUNT,NEGLIM,SCALE,DEPTH
      INTEGER CARD,SEQ,SEQ_PED,LCPD8,GZCPD8,LCPD1,GZCPD1
      INTEGER LCGN8,GZCGN8,LCGN1,GZCGN1,IPH,I
      INTEGER IDOFF,ID(10),ISCALE,GBIN,PBIN,PSEQ,GSEQ
      LOGICAL FIRST,EZERR,DO_PEDS,DO_GAINS,ZSUP
      LOGICAL FIRSTCALL,FIRST_PED,FIRST_GNS
      CHARACTER*80 TITLE,MSG
      REAL ENTRY1,SCUTOF,SIGMA,PLO,PHI,PSIG,GLO,GHI,GSIG
      REAL    PEDESTAL,PULSE_HEIGHT,GAIN,GAIN_NORM,WEIRD_GAIN
      INCLUDE 'D0$PARAMS:CAL_ADC_NO.PARAMS'     ! CAD bank params
      INCLUDE 'D0$PARAMS:BYTE_ORDER.PARAMS'
      INTEGER*2 HLFWRD(2)
      EQUIVALENCE (HLFWRD,IWORD)        ! unpacking pulse_height
      INTEGER IBITS

      DATA FIRST_PED,FIRST_GNS/2*.TRUE./
      DATA GAIN/1./
      DATA FIRST/.TRUE./,ENTRY1 /0.001/
C----------------------------------------------------------------------
      ENTRY1 = ENTRY1 + 1
      TB90_CALOR_PDGN_CHK = .TRUE.
      CALL GTCAD_TOTAL(1,NCH,IER)
C  Get CAD1 header info
      CALL GTCAD_HEADER (1,ICRATE,HEADER_LEN,SYNCH,
     &  CONTROL_WORD,STATUS,VERSION,PULSER,IER)
C
      FIRSTCALL = .TRUE.
C
C ***** loop over ADCs per crate
C
      DO 40, ICAD = 1, NCH
        CALL GTCAD(1,FIRSTCALL,ICRATE,IWORD,IER)
        FIRSTCALL = .FALSE.
        NCOUNT=NCOUNT+1
        NEGLIM = IBITS(IWORD,16,1)                    ! JBIT(ADR,1)
        SCALE =  IBITS(IWORD,17,1)                    ! JBIT(ADR,2)
        DEPTH =  IBITS(IWORD,18,4)                    ! JBYT(ADR,3,4)
        CARD  =  IBITS(IWORD,27,5)                    ! JBYT(ADR,12,5)
C
C--- Now get the SEQUENTIAL ADC address
C
        SEQ =  IBITS(IWORD,22,9)*NDEPTC + DEPTH   + 1
C
        SEQ_PED = IBITS(IWORD,22,5)*NDEPTC + DEPTH
C
C ****  Do pedestal subtraction if needed
C
        IF (FIRST_PED) THEN
          FIRST_PED = .FALSE.
          LCPD8 = GZCPD8 ()
          LCPD1 = GZCPD1 ()
          IF ( LCPD8.LE.0 .OR. LCPD1.LE.0 ) THEN
            DO_PEDS = .FALSE.
            PEDESTAL = 0
          END IF
        END IF
        IF ( DO_PEDS ) THEN
          CALL GT_PED_GNS_ADDR(1,ICRATE,CARD,SEQ_PED,SCALE,
     &    PEDESTAL,SIGMA,IER)
          IPH=HLFWRD(WORD1)
          PULSE_HEIGHT = IPH
          CALL HFILL(ID(1)+IDOFF,PEDESTAL,PULSE_HEIGHT,1.)
          PULSE_HEIGHT = IPH - PEDESTAL
          CALL HF1(ID(2)+IDOFF,PULSE_HEIGHT,1.)
          CALL HFILL(ID(3)+IDOFF,FLOAT(SEQ)+0.001,PULSE_HEIGHT,1.)
        END IF
C
C ****  QUICK ZERO SUPPRESSION
C
        IF ( (ABS(PULSE_HEIGHT).LT.(SCUTOF*SIGMA))) GOTO 40
        IF ( PULSE_HEIGHT.EQ. 0) GOTO 40
C
C ****  Do gains correction if needed
C
        IF (FIRST_GNS ) THEN
          FIRST_GNS = .FALSE.
          LCGN8 = GZCGN8 ()
          LCGN1 = GZCGN1 ()
          IF ( LCGN8.LE.0 .OR. LCGN1.LE.0 ) THEN
            DO_GAINS = .FALSE.
            GAIN = 1.0
          END IF
        END IF
        IF ( DO_GAINS ) THEN
          CALL GT_PED_GNS_ADDR(3,ICRATE,CARD,SEQ_PED,SCALE,
     &    GAIN,SIGMA,IER)
          CALL HFILL(ID(4)+IDOFF,GAIN,PULSE_HEIGHT,1.)
          IF ( GAIN.LT.WEIRD_GAIN) THEN
            WRITE(MSG,1956) CARD,SEQ_PED, SEQ,GAIN
 1956       FORMAT(' BAD GAIN ',3I4,' GAIN ',E8.2)
            GOTO 40
          ELSE
            PULSE_HEIGHT = GAIN_NORM * PULSE_HEIGHT / GAIN
            CALL HF1(ID(5)+IDOFF,PULSE_HEIGHT,1.)
            CALL HFILL(ID(6)+IDOFF,FLOAT(SEQ)+0.001,PULSE_HEIGHT,1.)
          END IF
        ENDIF
   40   CONTINUE
C
      
  999 RETURN
C
      ENTRY TB90_CALOR_PDGN_CHK_SETUP ()
      IF (FIRST) THEN
         FIRST = .FALSE.
         CALL INRCP ('TB90_CALOR_PDGN_CHK_RCP',IER)
         IF (IER.NE.0) THEN
           CALL STAMSG('NO TB90_CALOR_PDGN_CHK_WATCH_RCP',.TRUE.)
           GOTO 999
         END IF
         CALL EZPICK('TB90_CALOR_PDGN_CHK_RCP')
         IF(EZERR(IER)) GOTO 999
         CALL EZGET_i('HIST_ID_OFFSET',IDOFF,IER)
         CALL EZGET_iarr('HIST_ID',ID,IER)
         CALL EZGET('SIGMA_CUTOFF',SCUTOF,IER)
         CALL EZGET('GAIN_NORM',GAIN_NORM,IER)
         CALL EZGET('WEIRD_GAIN_CUT',WEIRD_GAIN,IER)
         CALL EZGET_l('DO_GAINS',DO_GAINS,IER)
         CALL EZGET_l('DO_PEDS',DO_PEDS,IER)
         CALL EZGET_i('CHOOSE_SCALE',ISCALE,IER)
         CALL EZGET('HIST_PED_LO',PLO,IER)
         CALL EZGET('HIST_PED_HI',PHI,IER)
         CALL EZGET('HIST_PED_SIG',PSIG,IER)
         CALL EZGET_i('HIST_PED_BIN',PBIN,IER)
         CALL EZGET_i('HIST_PED_SEQ',PSEQ,IER)
         CALL EZGET('HIST_GNS_LO',GLO,IER)
         CALL EZGET('HIST_GNS_HI',GHI,IER)
         CALL EZGET('HIST_GNS_SIG',GSIG,IER)
         CALL EZGET_i('HIST_GNS_BIN',GBIN,IER)
         CALL EZGET_i('HIST_GNS_SEQ',GSEQ,IER)
         CALL EZRSET
         CALL HBPROF(ID(1),'PEDS (X) <PH>(Y)',PBIN,PLO,PHI,PLO,PHI,' ') 
         CALL HBOOK1(ID(2),'PH-PED ',PBIN,-PSIG,PSIG,0)
         CALL HBPROF(ID(3),'SEQ (X) <PH-PED>(Y)',PSEQ,0.,FLOAT(PSEQ),
     &     -PSIG,PSIG,' ')
         CALL HBPROF(ID(4),'GNS (X) <PH>(Y)',GBIN,GLO,GHI,GLO,GHI,' ') 
         CALL HBOOK1(ID(5),'PH/GAIN ',GBIN,-GSIG,GSIG,0)
         CALL HBPROF(ID(6),'SEQ (X) <PH/GAIN>(Y)',GSEQ,0.,FLOAT(GSEQ),
     &     -GSIG,GSIG,' ')
      END IF
      TB90_CALOR_PDGN_CHK_SETUP = .TRUE.
 1999 RETURN
      ENTRY TB90_CALOR_PDGN_CHK_FINAL ()
      TB90_CALOR_PDGN_CHK_FINAL = .TRUE.
      DO I = 1, 6
        CALL HPRINT(ID(I))
      END DO
 2999 RETURN
      END
