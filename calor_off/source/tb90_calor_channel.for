      FUNCTION TB90_CALOR_CHANNEL ()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : MAKE HISTOGRAMS AND WATCH HISTOGRAMS OF SELECTED
C-                         CHANNELS
C-
C-   Returned value  : TRUE
C-   Inputs  : -
C-   Outputs : -
C-   Controls: -
C-
C-   Created  24-JUN-1990   Chip Stewart
C-   Updated   8-APR-1991   Scott Snyder - Add EZRSET call.
C-   Updated  5-MAR-1994    Roy Thatcher - fixed so D0entry works right
C-     Changed local variable ENTRY to ENTRY1

C-    
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL TB90_CALOR_CHANNEL
      LOGICAL TB90_CALOR_CHANNEL_SETUP
      REAL ENTRY1
      INTEGER NBOOK,MAX_WATCH,IER
      PARAMETER (MAX_WATCH=50)
      INTEGER ETA(MAX_WATCH),PHI(MAX_WATCH),LYR(MAX_WATCH)
      REAL ENERGY,LOW_CRAZY,HIGH_CRAZY
      LOGICAL FIRST,EZERR
      LOGICAL SEARCH_CRAZY,FIRSTCALL
      INTEGER PHYSICS(MAX_WATCH*10),ELECTRONICS(MAX_WATCH*10)
      INTEGER I,J,K,IWATCH,POINT,IOFF1,IOFF2,NUMG,BITS,ICHAN
      INTEGER SEQ,NSEQ,ADD,IDATA,REGION,ETAC,PHIC,LYRC,JETA,JPHI,JLYR
      REAL    RHYSICS(MAX_WATCH*10),RLECTRONICS(MAX_WATCH*10)
      EQUIVALENCE (PHYSICS(1),RHYSICS(1))
      EQUIVALENCE (ELECTRONICS(1),RLECTRONICS(1))
      CHARACTER*80 TITLE,MSG
      REAL HLO,HHI,F
      INTEGER HBINS,WBINS
      INTEGER IADC,IBLS,ITOW,IDEP,JTOW,JDEP,ETAV(20),PHIV(20),LYRV(20)
      INTEGER ADCP,BLSP,TOWP,DEPP,NV,NR,NCH
      DATA FIRST/.TRUE./,ENTRY1 /0.001/
C----------------------------------------------------------------------
      ENTRY1 = ENTRY1 + 1
      CALL CPTCAF
      IF (SEARCH_CRAZY .AND. NBOOK.LT. MAX_WATCH) THEN
        CALL GTCAEP_HEADER(NV,NR,NCH,IER)
        FIRSTCALL = .TRUE.
        DO 500 I=1,NCH
          CALL GTCAEP(FIRSTCALL,JETA,JPHI,JLYR,BITS,ENERGY,ICHAN,IER)
          IF(FIRSTCALL)FIRSTCALL=.FALSE.
C
C ****  Check for crazies...
C
          IF (ENERGY.LT.LOW_CRAZY.or.ENERGY.GT.HIGH_CRAZY)THEN
            HLO = MIN(1.2*ENERGY, -500.)
            HHI = MIN(1.2*ENERGY, 20000.)
            HBINS = 100
            WBINS = 100
            CALL TB90_CALOR_CHANNEL_BOOK(JETA,JPHI,JLYR,HBINS,
     &        HLO,HHI,ENTRY1-0.001,WBINS,IOFF1,IOFF2,NBOOK,IER)
            IF(IER.EQ.0) THEN
              WRITE(MSG,5001)JETA,JPHI,LYRC,ENERGY
 5001         FORMAT('WATCH BAD CHANNEL ETA ',I2,' PHI ',I2,' LYR ',I2,
     &          ' ENERGY ',E10.2)
              CALL INTMSG(MSG,.FALSE.)
              ETA(NBOOK) = JETA
              PHI(NBOOK) = JPHI
              LYR(NBOOK) = JLYR
            END IF
          ENDIF
  500   CONTINUE
      END IF
      DO I = 1, NBOOK
        CALL GTCAEP_ADDR(ETA(I),PHI(I),LYR(I),ENERGY,IER)
        IF (IER.EQ.0) THEN
          CALL HF1(IOFF1+I,ENERGY,1.0)
          CALL HF1(IOFF2+I,ENTRY1,ENERGY)
        END IF
      END DO
      TB90_CALOR_CHANNEL = .TRUE.
      RETURN
C

      ENTRY TB90_CALOR_CHANNEL_SETUP ()
      IF (FIRST) THEN
         FIRST = .FALSE.
         CALL INRCP ('TB90_CALOR_CHANNEL_RCP',IER)
         IF (IER.NE.0) THEN
           CALL STAMSG('NO TB90_CALOR_CHANNEL_WATCH_RCP',.TRUE.)
           GOTO 999
         END IF
      END IF
      CALL EZPICK('TB90_CALOR_CHANNEL_RCP')
      IF(EZERR(IER)) GOTO 999
      CALL EZGET('WATCH_CRAZIES',SEARCH_CRAZY,IER)
      CALL EZGET('LOW_CRAZY_LIMIT',LOW_CRAZY,IER)
      CALL EZGET('HIGH_CRAZY_LIMIT',HIGH_CRAZY,IER)
      CALL EZGET('HIST_ID_OFFSET',IOFF1,IER)
      CALL EZGET('HIST_ID_OFFSET',IOFF1,IER)
      CALL EZGET('WATCH_ID_OFFSET',IOFF2,IER)
      CALL EZGET('PHYSICS_WATCH',PHYSICS,IER)
      CALL EZGET('ELECTRONICS_WATCH',ELECTRONICS,IER)
      CALL EZRSET
C
C ****  PHYSICS ADDRESS CHANNEL WATCH
C
      POINT = 0
      NBOOK = 0
      DO IWATCH = 1, MAX_WATCH
        IF (NBOOK .GE. MAX_WATCH ) GOTO 999
        ETAC          = PHYSICS(POINT+1)
        IF (ETAC.LE.0) GOTO 60
        PHIC          = PHYSICS(POINT+2)
        LYRC          = PHYSICS(POINT+3)
        REGION        = PHYSICS(POINT+4)
        HBINS         = PHYSICS(POINT+5)
        HLO           = RHYSICS(POINT+6)
        HHI           = RHYSICS(POINT+7)
        F             = RHYSICS(POINT+8)
        WBINS         = PHYSICS(POINT+9)
        DO JETA = ETAC-REGION,ETAC+REGION
          DO JPHI = PHIC-REGION,PHIC+REGION
            CALL TB90_CALOR_CHANNEL_BOOK(JETA,JPHI,LYRC,
     &        HBINS,HLO,HHI,1.,WBINS,IOFF1,IOFF2,NBOOK,IER)
            IF(IER.EQ.0) THEN
              ETA(NBOOK) = JETA
              PHI(NBOOK) = JPHI
              LYR(NBOOK) = LYRC
            END IF
         END DO
        END DO
        POINT = POINT + 9
      END DO
   60 CONTINUE
C
C ****  ELECTRONICS ADDRESS CHANNEL WATCH
C
      POINT = 0
      J = NBOOK
      DO IWATCH = J, MAX_WATCH
        IADC  = ELECTRONICS(POINT+1)
        IF (IADC .LT.0) GOTO 70
        IBLS           = ELECTRONICS(POINT+2)
        ITOW           = ELECTRONICS(POINT+3)
        IDEP           = ELECTRONICS(POINT+4)
        IF ( IADC.EQ.ADCP .AND. IBLS.EQ.BLSP .AND. ITOW.EQ.TOWP
     &    .AND. IDEP.EQ.DEPP) GOTO 70
        ADCP = IADC
        BLSP = IBLS
        TOWP = ITOW
        DEPP = IDEP
        REGION         = ELECTRONICS(POINT+5)
        HBINS         = ELECTRONICS(POINT+6)
        HLO           = RLECTRONICS(POINT+7)
        HHI           = RLECTRONICS(POINT+8)
        F             = RLECTRONICS(POINT+9)
        WBINS         = ELECTRONICS(POINT+10)
        DO 64 JTOW = ITOW -REGION,ITOW +REGION
          IF(JTOW.LT.0 ) GOTO 64
          DO 62 JDEP = IDEP-REGION,IDEP+REGION
            IF(JDEP.LT.0) GOTO 62
            CALL CADPAK(IADC,IBLS,JTOW,JDEP,0,0,ADD)
            IDATA = ISHFT(ADD,16)
            CALL TB90_ADCPHY(IDATA,NUMG,ETAV,PHIV,LYRV,IER)
            IF (IER.NE.0) GOTO 62
            CALL TB90_CALOR_CHANNEL_BOOK(ETAV(1),PHIV(1),LYRV(1),
     &        HBINS,HLO,HHI,1.,WBINS,IOFF1,IOFF2,NBOOK,IER)
            IF(IER.EQ.0) THEN
              ETA(NBOOK) = ETAV(1)
              PHI(NBOOK) = PHIV(1)
              LYR(NBOOK) = LYRV(1)
            END IF
   62     CONTINUE
   64   CONTINUE
        POINT = POINT + 10
      END DO
   70 CONTINUE
      TB90_CALOR_CHANNEL_SETUP = .TRUE.
  999 RETURN
      END