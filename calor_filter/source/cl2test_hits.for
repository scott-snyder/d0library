      FUNCTION CL2TEST_HITS()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Compare results from CL2 fast conversion
C-                         routines to CAHITS routines.
C-   Inputs  : CAD banks
C-   Outputs : histograms and dumps of differing channels; timing of unpacking
C-      in any case, produces CAEP CAEH and PTCAEP under RECO,
C-              and CAEP and PTCAEP2 under FILT
C-   Controls: CL2TEST_RCP,CAHITS_RCP
C-
C-   Created   9-NOV-1990   Richard V. Astur
C-   Updated 13-MAY-1991   James T. Linnemann
C-
C----------------------------------------------------------------------
C#######################################################################
C     ENTRY CL2TEST_HITS_END()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :end of run processing for cl2_test
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
      REAL TSEC,TCAHITS,TCL2            ! time interval and sums in CPU seconds
      INTEGER NTIMES,NTIMES2            ! number of trials
      REAL AVGT,AVGT2                   ! average time
      INTEGER LUN,USUNIT,IER            ! printing unit
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$INC:PTCAEP.INC'       ! CAHITS pointer to CAEP,CAEH
      INCLUDE 'D0$INC:CL2TEST_LINK.INC' ! CAHITS links
      INCLUDE 'D0$INC:CL2_LINK.INC'     ! pointers to CL2xxx banks
      INCLUDE 'D0$CALOR_FILTER$SOURCE:PTCAEP2.DEF'      ! Fast routine pointer
      INTEGER IETA,IPHI,ILAYER
      INTEGER GZCAEP,GZCAEH
      INTEGER IPOINT
      CHARACTER*4 OLD_PATH
      LOGICAL CEXIST,CL2TEST_INIT,CL2TEST_HITS,CL2TEST_HITS_END
      LOGICAL EZERROR,OK,REMAKE_CAD
      LOGICAL TIME_HITS,COMPARE_HITS,DO_CAEPFL,DO_CAEHFL,DO_C1PMET
      REAL ECAHITS,ETCAHITS,ECL2,ETCL2
      REAL    X,Y,Z                     ! cell position
      REAL ET_DIFF,E_DIFF,SINT,SINT2,DSINT,FDSINT,FRAC
      REAL    MPTCAHITS,MPTCL2,SUMETCAHITS,SUMETCL2
      INTEGER LPNUT,GZPNUT
      REAL CL2_DIFF_CUT                 ! RCP defined cuts
      LOGICAL CADMAKE
      INTEGER LCAD1,LCAD2,GZCAD1,GZCAD2
C----------------------------------------------------------------------
      SAVE LUN
      LOGICAL FIRST
      SAVE FIRST
      SAVE NTIMES,NTIMES2,TCAHITS,TCL2
      DATA FIRST/.TRUE./
      DATA NTIMES,NTIMES2/2*0/
      DATA TCAHITS,TCL2/2*0.0/
C----------------------------------------------------------------------
C
      IF (FIRST) THEN
        LUN = USUNIT()                  ! user summary unit
        CALL HCDIR('//PAWC',' ')          ! go to top directory
        CALL HMDIR('CL2TEST_HITS','S')       ! create CL2TEST directory
        OK = CL2TEST_INIT()             ! be sure INIT is done
        CALL EZPICK('CL2TEST_RCP')       ! Select bank
        OK = .NOT.EZERROR(IER)
        IF (IER .EQ. 0) CALL EZGET('TIME_HITS',TIME_HITS,IER)
        IF (IER .EQ. 0) CALL EZGET('REMAKE_CAD',REMAKE_CAD,IER)
        IF (IER .EQ. 0) CALL EZGET('COMPARE_HITS',COMPARE_HITS,IER)
        IF (IER .EQ. 0) CALL EZGET('CL2_DIFF_CUT',CL2_DIFF_CUT,IER) ! Get values
        IF (IER .NE. 0) THEN      ! Error reading RCP
          CALL ERRMSG('CL2TEST','CL2TEST_HITS',
     &  ' Error while reading CL2TEST_RCP','F')
        ELSE
          CALL EZRSET
        ENDIF
        CALL EZPICK('CAHITS_RCP')       ! Select bank
        OK = .NOT.EZERROR(IER)
        IF (IER .EQ. 0) CALL EZGET('DO_CAEPFL',DO_CAEPFL,IER)
        IF (IER .EQ. 0) CALL EZGET('DO_CAEHFL',DO_CAEHFL,IER)
        IF (IER .EQ. 0) CALL EZGET('DO_C1PMET',DO_C1PMET,IER)
        IF (IER .NE. 0) THEN      ! Error reading RCP
          CALL ERRMSG('CL2TEST','CL2TEST_HITS',
     &  ' Error while reading CAHITS_RCP','F')
        ELSE
          CALL EZRSET
        ENDIF
        IF(COMPARE_HITS) THEN
          WRITE(LUN,199)
  199     FORMAT(
     &    ' ETA PHI LAY DELTA ET, E (CL2-CAHITS),E,ET CAHITS,DET/ET')
C                                       ! book histograms
          CALL HBOOK1(1,' DELTA ET CELL: CL2-CAHITS',50,-5.,5.,0.)
          CALL HBOOK1(2,' DELTA E CELL: CL2-CAHITS',50,-5.,5.,0.)
          CALL HBOOK1(3,' DELTA ET CELL, NOICD : CL2-CAHITS',50,-5.,
     &    5., 0.)
          CALL HBOOK1(4,' DELTA E CELL, NOICD : CL2-CAHITS',50,-5.,
     &    5., 0.)
          CALL HBOOK1(5,'DELTA SIN THETA',50,-.1,.1,0.)
          CALL HBOOK1(6,'DELTA SIN THETA/SIN THETA',50,-.1,.1,0.)
          CALL HBPROF(7,'DELTA SIN THETA/SIN THETA vs ETA',76,
     &      -37.,38., -.5,.5,'S')
          CALL HBOOK1(8,'ET L2/ET CAHITS',50,.9,1.1,0.)
          CALL HIDOPT(8,'STAT')
          IF (DO_C1PMET.AND.DO_CAEHFL) THEN
            CALL HBOOK1(9,'DELTA MPT/ MPT CAHITS)',50,.9,1.1,0.)
            CALL HBOOK1(10,'DELTA SUMET/ SUMET CAHITS)',50,.9,1.1,
     &        0.)
          ENDIF
        ENDIF
        IF (TIME_HITS) THEN
          CALL HBOOK1(11,'HITS CAHITS',50,0.,5000.,0.)
          CALL HIDOPT(11,'STAT')
          CALL HBOOK1(12,'HITS CL2',50,0.,5000.,0.)
          CALL HIDOPT(12,'STAT')
        ENDIF
        FIRST = .FALSE.
      ENDIF
C----------------------------------------------------------------------
      CL2TEST_HITS = .FALSE.
      CALL PATHGT(OLD_PATH)
      CALL MZLINT(IXCOM,'/CL2TEST/',CL2TEST_LNKFLG,CL2TEST_LINK(NTLNK),
     &  CL2TEST_LNKFLG)                 ! init temp link area
      CALL HCDIR('//PAWC/CL2TEST_HITS',' ') ! Get correct PAW directory
C
C...build offline CAEP and CAEH banks
      IF (TIME_HITS) THEN
        CALL PATHST('RECO')
        CALL TIMED(TSEC)                  ! gives cpu time since last call
        CALL CAEPFL(OK)                   ! ignores zeroing time; see below
        CALL CAEHFL                       ! want CAEH even if claimed not in RCP
        CALL C1PMET                       ! calculate PNUT1
        IF (REMAKE_CAD) THEN
          LCAD1 = GZCAD1()
          CALL MZDROP(IXCOM,LCAD1,'L')
          LCAD2 = GZCAD2()
          CALL MZDROP(IXCOM,LCAD2,'L')
          OK = CADMAKE()               ! HERE IS WHERE CAD is REBUILT
        ENDIF
        CALL TIMED(TSEC)
        TCAHITS = TCAHITS + TSEC
        CALL TIMED(TSEC)
        CALL CPTCAZ                       ! zero ptcaep
        CALL TIMED(TSEC)
        TCAHITS = TCAHITS + TSEC
        NTIMES = NTIMES + 1
        LCAEP = GZCAEP()
        CALL HFILL(11,FLOAT(IQ(LCAEP+3)),0.,1.)
        IF (OK) THEN
          CALL CL2_VZERO_PTCAEP2          ! separate zeroing time from fill time
          CALL PATHST('FILT')             ! and set path
          CALL MKPATH
          CALL TIMED(TSEC)
C        OK = CL2HITS()                  ! vzero and fill whole CAEP
          CALL CL2_CAEPFL(OK)             ! fill CAEP without zeroing
          CALL TIMED(TSEC)
          TCL2 = TCL2 + TSEC
          NTIMES2 = NTIMES2 + 1
          CALL HFILL(12,FLOAT(IQ(L2CAEP+3)),0.,1.)
        ENDIF
      ELSE
        CALL PATHST('RECO')
        CALL CAEPFL(OK)
        CALL CAEHFL
        CALL C1PMET                       ! calculate PNUT1
        CALL PATHST('FILT')             ! and set path
        CALL MKPATH
        CALL CL2TEST_FORGET_EVT
        IF(OK) CALL CL2_CAEPFL(OK)
      ENDIF
      CALL PATHST('RECO')
      CALL CPTCAF                       ! rebuild ptcaep

C
C---Loop over all the indices and test difference.
      IF (COMPARE_HITS.AND.OK) THEN
        CALL PATHST('RECO')
        IF (DO_CAEHFL.AND.DO_C1PMET) THEN
          LCAEH = GZCAEH()
          IF (LCAEH .LE. 0) THEN
            CALL ERRMSG('CL2TEST','CL2TEST_HITS',
     &          ' CAEH bank not found','E')
            GO TO 999
          ENDIF
          LPNUT = GZPNUT(1)
          MPTCAHITS = 0.000001
          SUMETCAHITS = 0.0000001
          IF (LPNUT.GT.0) THEN
            MPTCAHITS = Q(LPNUT+7)+.000001        ! so can div afely
            SUMETCAHITS = Q(LPNUT+14)+.0000001
          ENDIF
          IF (L2CAEP .LE. 0) THEN
            CALL ERRMSG('CL2TEST','CL2TEST_HITS',
     &          ' CAEP bank not found','E')
            GO TO 999
          END IF
          MPTCL2 = 0
          SUMETCL2 = 0
          IF (L2PNUT.GT.0) THEN
            MPTCL2 = Q(L2PNUT+7)
            SUMETCL2 = Q(L2PNUT+14)
          ENDIF
          CALL HFILL(9,(MPTCL2/MPTCAHITS),0.,1.)
          CALL HFILL(10,(SUMETCL2/SUMETCAHITS),0.,1.)
        ENDIF
        DO IETA = -NETAL,NETAL
          DO IPHI = 1,NPHIL
            DO ILAYER = 1,NLYRL
C              IF (CEXIST(IETA,IPHI,ILAYER)) THEN   ! Only if cell exists
              ECAHITS = 0.0               ! Initialize
              ETCAHITS= 0.0
              ECL2    = 0.0
              ETCL2   = 0.0
              ET_DIFF = 0.0
              SINT = 1.0
              DSINT = 0.0
              FDSINT = 0.0
              SINT2 = 1.0
C
C---Get ET,E from CAEH
              IF (PTCAEP(IETA,IPHI,ILAYER) .GT. 0) THEN
                IPOINT = (PTCAEP(IETA,IPHI,ILAYER)-1)*IQ(LCAEH+2)
                ECAHITS = Q(LCAEH + IPOINT + 7)
                ETCAHITS= Q(LCAEH + IPOINT + 8)
                IF (ECAHITS.NE.0) SINT = ETCAHITS/ECAHITS
              ENDIF
C
C---Get ET from CL2TEST
              IF (PTCAEP2(ILAYER,IPHI,IETA) .GT. 0) THEN
C                IF (PTR2(ILAYER,IPHI,IETA) .GT. 0) THEN        !don't do this
                IPOINT = (PTCAEP2(ILAYER,IPHI,IETA)-1)*IQ(L2CAEP+2)
C                  IPOINT = (PTR2(ILAYER,IPHI,IETA)-1)*IQ(L2CAEP+2) !don't do
                ETCL2 = Q(L2CAEP + IPOINT + 5)
                ECL2 = ETCL2
C                 SINT2 = L2J_SIN(IETA)
                CALL CELXYZ(IETA,IPHI,ILAYER,X,Y,Z,IER)
                SINT2 = SQRT ( (X**2+Y**2)/(X**2+Y**2+Z**2) )
                IF (SINT2.NE.0) ECL2 = ECL2/SINT2
              END IF
C
C---Compare them:
              IF (ECAHITS.NE.0.) THEN
                E_DIFF = ECL2 - ECAHITS
                ET_DIFF = ETCL2 - ETCAHITS
                CALL HFILL(1,ET_DIFF,0.,1.)
                CALL HFILL(2,E_DIFF,0.,1.)
                IF (ILAYER.LE.7.OR.ILAYER.GE.12) THEN
                  CALL HFILL(3,ET_DIFF,0.,1.)
                  CALL HFILL(4,E_DIFF,0.,1.)
                ENDIF
                DSINT = SINT2 - SINT
                IF(SINT.NE.0) THEN
                  FDSINT = DSINT/SINT
                ELSE
                  FDSINT = DSINT
                ENDIF
                CALL HFILL(5,DSINT,0.,1.)
                CALL HFILL(6,FDSINT,0.,1.)
                CALL HFILL(7,FLOAT(IETA),FDSINT,1.)
                IF(ETCAHITS.NE.0)CALL HFILL(8,ETCL2/ETCAHITS,0.,
     &              1.)
              ENDIF
              IF (ABS(ET_DIFF) .GT. CL2_DIFF_CUT*ABS(ETCAHITS)) THEN
                FRAC = 0
                IF (ETCAHITS.NE.0) FRAC = ET_DIFF/ETCAHITS
                WRITE(LUN,99) IETA,IPHI,ILAYER,ET_DIFF,E_DIFF,
     &                ECAHITS,ETCAHITS,FRAC
   99           FORMAT(' ',3I4,5F8.3)
              ENDIF
C              END IF
            END DO
          END DO
        END DO
      ENDIF
      CL2TEST_HITS = OK
  999 CONTINUE
      CL2TEST_LNKFLG(1) = 0             ! deactivate temporary link area
      CALL PATHST(OLD_PATH)
      RETURN
C#######################################################################
      ENTRY CL2TEST_HITS_END()
C----------------------------------------------------------------------
      IF (NTIMES.GT.0) AVGT = TCAHITS/NTIMES
      IF (NTIMES2.GT.0) AVGT2 = TCL2/NTIMES2
      WRITE(LUN,300)'CAHITS',TCAHITS,NTIMES,AVGT
      WRITE(LUN,300)'CL2',TCL2,NTIMES2,AVGT2
  300 FORMAT(/1X,A10,' TOOK ',F10.3,' SEC FOR',I7,
     &    ' CAEP/PTCAEP FILLS; AVG =',F6.4/)
      CL2TEST_HITS_END = .TRUE.
      RETURN
      END
