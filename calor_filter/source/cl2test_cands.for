      FUNCTION CL2TEST_CANDS()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Compare results from CL2 fast conversion
C-                         routines to CAHITS on Level 2 EM and JET candidates
C-   Inputs  : CAEP and CAEH from CAHITS
C-      the full FILT path is assumed to exist
C-
C-   Outputs : histograms and dumps of differing channels; timing of unpacking
C-   Controls: CL2TEST_RCP
C-
C-   Created 13-MAY-1991   James T. Linnemann
C-
C----------------------------------------------------------------------
C#######################################################################
C     ENTRY CL2TEST_CANDS_END()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :end of run processing for cl2test_CANDS
C-              print timing numbers for event processing, and histos
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  13-MAY-1991   James T. Linnemann
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL TSEC,TEM,TJT    ! time interval and sums in CPU seconds
      INTEGER NEM,NJT    ! number of trials
      REAL    TICK                      ! VAX CLOCK TICK
      PARAMETER( TICK = .010  )         ! 10 msec
      REAL AVGEM,AVGJT,ERREM,ERRJT    ! average time
      INTEGER NSOFAR                    ! how many channels converted so far
      INTEGER LUN,USUNIT,IER            ! printing unit
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$CALOR_FILTER$SOURCE:PTCAEP2.DEF'      ! Fast routine pointer
      INCLUDE 'D0$INC:CL2_LINK.INC'     ! pointers to CL2xxx banks
      INCLUDE 'D0$INC:CL2TEST_LINK.INC' ! CAHITS links
      INCLUDE 'D0$PARAMS:L1PHP.PARAMS'
      INCLUDE 'D0$PARAMS:L2JETS.PARAMS'
      INCLUDE 'D0$INC:L2JETS_HOT.INC'            ! The Hot Towers from L1
      INCLUDE 'D0$INC:DBITT.INC'        ! hot tower to L1 eta,phi
      INTEGER I
      INTEGER GZCAEH
      CHARACTER*4 OLD_PATH
      LOGICAL CL2TEST_INIT,CL2TEST_CANDS,CL2TEST_CANDS_END
      LOGICAL EZERROR,OK
      LOGICAL L1SIM_INI,L1SIM_EVENT,OKL1
      LOGICAL L2JETS_HOTFL
      LOGICAL TIME_CANDS,COMPARE_CANDS
      INTEGER TIMES_PER_CAND
      REAL ETCAHITS,ETCL2,ETL1,DUM
      INTEGER ICAND
      INTEGER IETAL1,IPHIL1             ! L1 center of ring
      INTEGER ETALO,ETAHI,PHILO(2),PHIHI(2),NPHI        ! boundary of ring
      LOGICAL ICDJET                    ! do ICD's with jet?
      INTEGER SIZEM,SIZJT               ! Ring sizes
C----------------------------------------------------------------------
      SAVE LUN
      LOGICAL FIRST
      SAVE FIRST
      SAVE NEM,NJT,TEM,TJT
      DATA FIRST/.TRUE./
      DATA NEM,NJT/2*0/
      DATA TEM,TJT,AVGEM,AVGJT,ERREM,ERRJT/6*0.0/
C----------------------------------------------------------------------
C
      IF (FIRST) THEN
        LUN = USUNIT()                  ! user summary unit
        CALL HCDIR('//PAWC',' ')          ! go to top directory
        CALL HMDIR('CL2TEST_CANDS','S')       ! create CL2TEST_CANDS directory
        CALL EZPICK('CL2TEST_RCP')       ! Select bank
        OK = .NOT.EZERROR(IER)
        IF (IER .EQ. 0) CALL EZGET_l('TIME_CANDS',TIME_CANDS,IER)
        IF (IER .EQ. 0) CALL EZGET_i('TIMES_PER_CAND',TIMES_PER_CAND,
     &                                IER)
        IF (IER .EQ. 0) CALL EZGET_l('ICDJET',ICDJET,IER)
        IF (IER .EQ. 0) CALL EZGET_i('SIZEM',SIZEM,IER)
        IF (IER .EQ. 0) CALL EZGET_i('SIZJT',SIZJT,IER)
        IF (IER .EQ. 0) CALL EZGET_l('COMPARE_CANDS',COMPARE_CANDS,IER)
        IF (IER .NE. 0) THEN      ! Error reading RCP
          CALL ERRMSG('CL2TEST','CL2TEST_CANDS',
     &  ' Error while reading CL2TEST_RCP','F')
        ELSE
          CALL EZRSET
        ENDIF
        OK = CL2TEST_INIT()             ! be sure INIT is done
        IF (TIME_CANDS) THEN
          CALL HBOOK1(1,'EM CANDS',10,0.,10.,0.)
          CALL HIDOPT(1,'STAT')
          CALL HBOOK1(2,'JT CANDS',10,0.,10.,0.)
          CALL HIDOPT(2,'STAT')
          CALL HBOOK1(3,'CHAN/EM CAND',50,0.,500.,0.)
          CALL HIDOPT(3,'STAT')
          CALL HBOOK1(4,'CHAN/JET CAND',50,0.,1000.,0.)
          CALL HIDOPT(4,'STAT')
          CALL HBOOK1(5,'ET of EM CAND CAHITS',50,0.,50.,0.)
          CALL HBOOK1(6,'ET of EM CAND CL2/CAHITS',50,.9,1.1,0.)
          CALL HBOOK1(7,'ET of EM CAND L1/CAHITS',50,0.,1.0,0.)
          CALL HBOOK1(8,'ET of JT CAND CAHITS',100,0.,100.,0.)
          CALL HBOOK1(9,'ET of JT CAND CL2/CAHITS',50,.9,1.1,0.)
          CALL HBOOK1(10,'ET of JT CAND L1/CAHITS',50,0.,1.0,0.)
          OK = OK.AND.L1SIM_INI()                   ! Level 1 cal simulator init
        ENDIF
        FIRST = .FALSE.
      ENDIF
C-
C----------------------------------------------------------------------
      CL2TEST_CANDS = .FALSE.
      CALL PATHGT(OLD_PATH)
      IF(TIME_CANDS) THEN
        CALL MZLINT(IXCOM,'/CL2TEST/',CL2TEST_LNKFLG,
     &    CL2TEST_LINK(NTLNK), CL2TEST_LNKFLG)          ! init temp link area
C
C...now do timing on per-candidate basis
        CALL PATHST('RECO')
        LCAEH = GZCAEH()                ! set link for CL2TEST_ETSUM
        OKL1 = L1SIM_EVENT()            ! Get trigger candidates from Level 1
        CALL HCDIR('//PAWC/CL2TEST_CANDS',' ') ! Get correct PAW directory
        CALL PATHST('FILT')
        IF (OKL1) THEN
          OKL1 = L2JETS_HOTFL()
          IF (OKL1) THEN
            CALL HFILL(1,FLOAT(NEMHOT)+.0001,0.,1.)
            CALL HFILL(2,FLOAT(NJTHOT)+.0001,0.,1.)
C...em candidates
            NSOFAR  = 0
            CALL CL2TEST_FORGET_EVT          ! dump event and try again
            DO ICAND = 1,NEMHOT
              IETAL1 =  DBITT(IHOT_ADR_EM(ICAND),1)
              IPHIL1 =  DBITT(IHOT_ADR_EM(ICAND),2)
              DO I = 1,TIMES_PER_CAND
                CALL TIMED(TSEC)
C
C...do this by hand instead of CL2TEST_FORGET_EVT for speed to make timing right
C...intentionally don't reset PTCAEP2
                IQ(LHEAD+7) = -IABS(IQ(LHEAD+7)) - 1 ! forget this event
                CALL MZDROP(IXCOM,L2CAEP,'L')
                L2CAEP = 0              ! be sure the link area forgets
                L2PNUT = 0
                CALL CL2_RING12(IETAL1,IPHIL1,SIZEM,ETALO,ETAHI,PHILO,
     &                  PHIHI,NPHI)
                CALL CL2_ROTOW_ETNOM(ETALO,ETAHI,PHILO(1),PHIHI(1))
                IF (NPHI.EQ.2)
     &              CALL CL2_ROTOW_ETNOM(ETALO,ETAHI,PHILO(2),PHIHI(2))
                CALL TIMED(TSEC)
                TEM = TEM + TSEC
                NEM = NEM + 1
              ENDDO
              CALL HFILL(3,FLOAT(IQ(L2CAEP+3)-NSOFAR)+.0001,0.,1.)
              NSOFAR = IQ(L2CAEP+3)
              IF (COMPARE_CANDS) THEN
                CALL CL2TEST_ETSUM('RECO',ETALO,ETAHI,
     &                PHILO,PHIHI,NPHI,MXLYEM,ETCAHITS)
                CALL HFILL(5,ETCAHITS,0.,1.)
                CALL CL2TEST_ETSUM('FILT',ETALO,ETAHI,PHILO,PHIHI,
     &            NPHI,MXLYEM,ETCL2)
                IF (ETCAHITS.NE.0) THEN
                  CALL HFILL(6,ETCL2/ETCAHITS,0.,1.)
                  CALL CL1PHET(IPHIL1,IETAL1,DUM,ETL1)
                  CALL HFILL(7,ETL1/ETCAHITS,0.,1.)
                ENDIF
              ENDIF
            ENDDO
C...jet candidates
            NSOFAR = 0
            CALL CL2TEST_FORGET_EVT
            DO ICAND = 1,NJTHOT
              IETAL1 =  DBITT(IHOT_ADR_JT(ICAND),1)
              IPHIL1 =  DBITT(IHOT_ADR_JT(ICAND),2)
              DO I = 1,TIMES_PER_CAND
                CALL TIMED(TSEC)
C
C...do this by hand instead of CL2TEST_FORGET_EVT for speed to make timing right
C...intentionally don't reset PTCAEP2
                IQ(LHEAD+7) = -IABS(IQ(LHEAD+7)) - 1 ! forget this event
                CALL MZDROP(IXCOM,L2CAEP,'L')
                L2CAEP = 0              ! be sure the link area forgets
                L2PNUT = 0
                CALL CL2_RING12(IETAL1,IPHIL1,SIZJT,ETALO,ETAHI,PHILO,
     &                  PHIHI,NPHI)
                CALL CL2_ROTOW_ETNOM(ETALO,ETAHI,PHILO(1),PHIHI(1))
                IF (ICDJET) CALL CL2_ICDMG_ETNOM(ETALO,ETAHI,
     &                                  PHILO(1),PHIHI(1))
                IF (NPHI.EQ.2) THEN
                  CALL CL2_ROTOW_ETNOM(ETALO,ETAHI,PHILO(2),PHIHI(2))
                  IF (ICDJET) CALL CL2_ICDMG_ETNOM(ETALO,ETAHI,
     &                                  PHILO(2),PHIHI(2))
                ENDIF
                CALL TIMED(TSEC)
                TJT = TJT + TSEC
                NJT = NJT + 1
              ENDDO
              CALL HFILL(4,FLOAT(IQ(L2CAEP+3)-NSOFAR)+.0001,0.,1.)
              NSOFAR = IQ(L2CAEP+3)
              IF (COMPARE_CANDS) THEN
                CALL CL2TEST_ETSUM('RECO',ETALO,ETAHI,PHILO,PHIHI,
     &              NPHI, NLYRL,ETCAHITS)
                CALL HFILL(8,ETCAHITS,0.,1.)
                CALL CL2TEST_ETSUM('FILT',ETALO,ETAHI,PHILO,PHIHI,
     &              NPHI, NLYRL,ETCL2)
                IF (ETCAHITS.NE.0) THEN
                  CALL HFILL(9,ETCL2/ETCAHITS,0.,1.)
                  CALL CL1PHET(IPHIL1,IETAL1,DUM,ETL1)
                  CALL HFILL(10,ETL1/ETCAHITS,0.,1.)
                ENDIF
              ENDIF
            ENDDO
          ENDIF
        ENDIF
      ENDIF
      CL2TEST_CANDS = OK
  999 CONTINUE
      CL2TEST_LNKFLG(1) = 0             ! deactivate temporary link area
      CALL PATHST(OLD_PATH)
      RETURN
C#######################################################################
      ENTRY CL2TEST_CANDS_END()
C----------------------------------------------------------------------
      IF (NEM.GT.0) THEN
        AVGEM = TEM/NEM
        ERREM = SQRT(TICK*TEM+.0000001)/NEM
      ENDIF
      IF (NJT.GT.0) THEN
        AVGJT = TJT/NJT
        ERRJT = SQRT(TICK*TJT+.0000001)/NJT
      ENDIF
      WRITE(LUN,300)'EM CANDS',TEM,NEM,AVGEM,ERREM
      WRITE(LUN,300)'JT CANDS',TJT,NJT,AVGJT,ERRJT
  300 FORMAT(/1X,A8,' TOOK ',F9.3,' SEC FOR',I7,
     &    ' TRIES; AVG =',F6.4,'(',F5.4,')'/)
      CL2TEST_CANDS_END = .TRUE.
      RETURN
      END
