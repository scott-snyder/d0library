      PROGRAM CHOTFLAG
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-
C-   Created  30-MAR-1993   sFahey
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$INC:PAWC.INC'
C----------------------------------------------------------------------
      CHARACTER*80 FILENAME,HISTFILE,TEMPSTR
      INTEGER CRATES(12)
      INTEGER IETA, IPHI, ILYR, IER, N, NHOT, RUNNUM, U, U1, IPART
      INTEGER NUM_EVTS,MIN_EVENTS,NEVTS,LENGTH,POINT
      INTEGER NHITS(-NETAL:NETAL,1:NPHIL,1:NLYRL)
      INTEGER NUM_HITS(-NETAL:NETAL,1:NPHIL,1:NLYRL)
C&IF VAXVMS
      INTEGER ISTAT,LIB$DELETE_FILE 
C&ENDIF
      REAL ENRG_SUM(-NETAL:NETAL,1:NPHIL,1:NLYRL)
      REAL NRG_SUM(-NETAL:NETAL,1:NPHIL,1:NLYRL)
      REAL OCCUPANCY(-NETAL:NETAL,1:NPHIL,1:NLYRL)
      REAL EVT(-NETAL:NETAL,1:NPHIL,1:NLYRL)
      REAL AVPHI(-NETAL:NETAL,1:NLYRL)
      REAL AVHI(1:4,1:2,1:NLYRL)
      REAL STDE(-NETAL:NETAL,1:NLYRL)
      REAL DIFE(-NETAL:NETAL,1:NPHIL,1:NLYRL)
      REAL NSIGE(-NETAL:NETAL,1:NPHIL,1:NLYRL)
      REAL STDO(-NETAL:NETAL,1:NLYRL)
      REAL DIFO(-NETAL:NETAL,1:NPHIL,1:NLYRL)
      REAL NSIGO(-NETAL:NETAL,1:NPHIL,1:NLYRL)
      INTEGER IBAD(-NETAL:NETAL,1:NPHIL,1:NLYRL)
      INTEGER ICHOT(-NETAL:NETAL,1:NPHIL,1:NLYRL)
C
      REAL OCC_CUT,E_EVT_HIGH_CUT,E_EVT_LOW_CUT
      REAL NSIG_E_CUT,NSIG_OCC_CUT
      LOGICAL LOK
C
      DATA CRATES/7,17,27,37,47,57,8,18,28,38,48,58/
C
C----------------------------------------------------------------------
C
      CALL MZEBRA(0)
      CALL INZSTP
      CALL INRCP('CHOTFLAG_RCP',IER)
      CALL EZPICK('CHOTFLAG_RCP')
      CALL EZGET('OCCUPANCY_CUT',OCC_CUT,IER)
      CALL EZGET('E_EVT_HIGH_CUT',E_EVT_HIGH_CUT,IER)
      CALL EZGET('E_EVT_LOW_CUT',E_EVT_LOW_CUT,IER)
      CALL EZGET('NSIG_E_CUT',NSIG_E_CUT,IER)
      CALL EZGET('NSIG_OCC_CUT',NSIG_OCC_CUT,IER)
      CALL EZGET_i('MIN_EVENTS',MIN_EVENTS,IER)
      CALL EZGETS('HSTCHOT',1,HISTFILE,LENGTH,IER)
      CALL EZRSET
C
C ALSO READ IN HISTOGRAM FILE NAME AND DIRECTORY FOR GM HISTOGRAMS
C
      CALL HLIMIT(-NPAWC)
      CALL HMDIR('//PAWC/CHOT','S')
      CALL HCDIR('//PAWC/CHOT',' ')
      CALL HBOOK2(100,'PHI vs ETA OF CAL NOISY CELLS',
     &           75,-37.5,37.5,64,0.5,64.5)
      NUM_EVTS=0
      CALL VZERO_i(NUM_HITS,(2*NETAL+1)*NPHIL*NLYRL)
      CALL VZERO(NRG_SUM,(2*NETAL+1)*NPHIL*NLYRL)
C
C   Read in files here.  Fill RUNNUM,NUM_EVTS,NUM_HITS,NRG_SUM
      CALL GTUNIT(57,U,IER)
      CALL D0OPEN(U,'CHOTLIST','I',LOK)
      IF(.NOT.LOK)THEN
        CALL ERRMSG('CALHOT','CHOTFLAG','Could not open list file','W')
        GOTO 999
      ENDIF
      CALL GTUNIT(57,U1,IER)
      DO WHILE(.TRUE.)
        READ(U,100,END=1000)FILENAME
  100   FORMAT(A)
        CALL D0OPEN(U1,FILENAME,'IU',LOK)
        IF(.NOT.LOK)THEN
          CALL ERRMSG('CALHOT','CHOTFLAG','Could not open file','W')
          GOTO 999
        ENDIF
        READ(U1) RUNNUM,IPART,NEVTS
        READ(U1) (((NHITS(IETA,IPHI,ILYR),ENRG_SUM(IETA,IPHI,ILYR),
     &    ILYR=1,NLYRL),IPHI=1,NPHIL),IETA=-NETAL,NETAL)
C
        NUM_EVTS=NUM_EVTS+NEVTS
        DO IETA=-NETAL,NETAL
          DO IPHI=1,NPHIL
            DO ILYR=1,NLYRL
              NUM_HITS(IETA,IPHI,ILYR)=NUM_HITS(IETA,IPHI,ILYR)+
     &          NHITS(IETA,IPHI,ILYR)
              NRG_SUM(IETA,IPHI,ILYR)=NRG_SUM(IETA,IPHI,ILYR)+
     &          ENRG_SUM(IETA,IPHI,ILYR)
            ENDDO
          ENDDO
        ENDDO
C
        CALL D0CLOSE(U1,' ',LOK)
      ENDDO
C
 1000 CALL D0CLOSE(U,' ',LOK)
      CALL RLUNIT(57,U,IER)
      CALL RLUNIT(57,U1,IER)
C
C     FILL OCCUPANCY, ENERGY/EVENT
      IF (NUM_EVTS.LT.MIN_EVENTS) THEN
         CALL HRESET(100,'CAL NOISY CELLS:  TOO FEW EVENTS')
         GOTO 900
      ENDIF 
      DO IETA = -NETAL, NETAL
        DO IPHI = 1, NPHIL
          DO ILYR = 1, NLYRL
            N = NUM_HITS(IETA, IPHI, ILYR)
            IF ( N .GT. 0 ) THEN
              OCCUPANCY(IETA, IPHI, ILYR) = FLOAT(N)/NUM_EVTS
              EVT(IETA,IPHI,ILYR) = NRG_SUM(IETA,IPHI,ILYR)/NUM_EVTS
            ENDIF
          ENDDO
        ENDDO
      ENDDO
C
      CALL VZERO_i(ICHOT,NPHIL*NLYRL*(NETAL*2+1))
C     NOW MAKE PHI-AVERAGE ANALYSIS. FIRST DO OCCUPANCY.
C     SET ALL CELLS OK.
      CALL VZERO_i(IBAD,NPHIL*NLYRL*(NETAL*2+1))
C     GET AVERAGES OVER PHI FOR EACH ETA, LAYER.
      CALL CAVEPHI(OCCUPANCY,IBAD,AVPHI,AVHI)
C     GET DEVS, SIGS, DEV/SIG, AND FLAG |DEV/SIG|>3.
      CALL CDEVGET(OCCUPANCY,IBAD,AVPHI,AVHI,DIFO,STDO,NSIGO)
C     NOW RECALCULATE AVERAGES WITH FLAGGED CELLS REMOVED
      CALL CAVEPHI(OCCUPANCY,IBAD,AVPHI,AVHI)
C     AND GET REVISED DEVS, SIGS,DEV/SIGS
      CALL CDEVGET(OCCUPANCY,IBAD,AVPHI,AVHI,DIFO,STDO,NSIGO)
C
C     REPEAT ANALYSIS FOR ENERGY/EVENT
C     SET ALL CELLS OK.
      CALL VZERO_i(IBAD,NPHIL*NLYRL*(NETAL*2+1))
C     GET AVERAGES OVER PHI FOR EACH ETA, LAYER.
      CALL CAVEPHI(EVT,IBAD,AVPHI,AVHI)
C     GET DEVS, SIGS, DEV/SIG, AND FLAG |DEV/SIG|>3.
      CALL CDEVGET(EVT,IBAD,AVPHI,AVHI,DIFE,STDE,NSIGE)
C     NOW RECALCULATE AVERAGES WITH FLAGGED CELLS REMOVED
      CALL CAVEPHI(EVT,IBAD,AVPHI,AVHI)
C     AND GET REVISED DEVS, SIGS,DEV/SIGS
      CALL CDEVGET(EVT,IBAD,AVPHI,AVHI,DIFE,STDE,NSIGE)
C
      NHOT = 0
      DO IETA = -NETAL, NETAL
        DO IPHI = 1, NPHIL
          DO ILYR = 1, NLYRL
            IF ((OCCUPANCY(IETA,IPHI,ILYR).GT.OCC_CUT)       .AND.
     &            ((EVT(IETA,IPHI,ILYR).GT.E_EVT_HIGH_CUT).OR.
     &            (EVT(IETA,IPHI,ILYR).LT.E_EVT_LOW_CUT))    .AND.
     &            (ABS(NSIGE(IETA,IPHI,ILYR)).GT.NSIG_E_CUT) .AND.
     &            (NSIGO(IETA,IPHI,ILYR).GT.NSIG_OCC_CUT))    THEN
              NHOT = NHOT + 1
              ICHOT(IETA,IPHI,ILYR) = 4
              CALL HFILL(100,FLOAT(IETA),FLOAT(IPHI),1.0)
            ENDIF
          ENDDO                         ! ilyr = 1, 17
        ENDDO                           ! iphi = 1, 64
      ENDDO                             ! ieta = -37, 37
C
C   Book and fill the calorimeter hot channel bank
C
      CALL DBCLB_INITIALIZE_FORCE_RUN(RUNNUM)
      CALL CDBINI(RUNNUM,12,CRATES,0,LOK)
      CALL CHOTFL(NHOT,ICHOT,RUNNUM,IER)
C
C   Delete temporary files if no error from CHOTFL
C&IF VAXVMS
      IF(IER.EQ.0)THEN
        CALL GTUNIT(57,U,IER)
        CALL D0OPEN(U,'CHOTLIST','I',LOK)
        CALL GTUNIT(57,U1,IER)
        DO WHILE(.TRUE.)
          READ(U,100,END=1100)FILENAME
          ISTAT=LIB$DELETE_FILE(FILENAME//';')
        ENDDO
 1100   CALL D0CLOSE(U,' ',LOK)
        CALL RLUNIT(57,U,IER)
      ENDIF
C&ENDIF
C
  900 CONTINUE
C   Add '_runno' to histfile
C
      CALL ADDSTR(HISTFILE(1:LENGTH),'CHOT_',TEMPSTR,LENGTH)
      CALL STRINT(TEMPSTR(1:LENGTH),RUNNUM,TEMPSTR,LENGTH)
      CALL ADDSTR(TEMPSTR(1:LENGTH),'.HST',HISTFILE,LENGTH)
      CALL HRPUT(0,HISTFILE,'NT')
  999 CONTINUE
      END
