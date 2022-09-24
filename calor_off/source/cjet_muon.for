      FUNCTION CJET_MUON
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Shell for MUON algorithm jet finder.
C-
C-   Inputs  :
C-   Outputs :
C-   Controls: CAJETS_RCP
C-
C-   Created : 22-DEC-1992  Alex Smith
C-   Revised : 14-FEB-1993  Alex Smith : make changes to CAEP instead 
C-             of CATE, so jet finding is done right.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZLINKC.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZHSTR.LINK'
      INCLUDE 'D0$LINKS:IZCATE.LINK'
      INCLUDE 'D0$LINKS:IZCAEP.LINK'
      INCLUDE 'D0$LINKS:IZCAEH.LINK'
      INCLUDE 'D0$LINKS:IZCAHT.LINK'
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$PARAMS:CAPH.DEF'
      INCLUDE 'D0$INC:CJET_ALGORITHM.INC'
      INCLUDE 'D0$INC:PTCATE.INC'
      INCLUDE 'D0$INC:PTCAEP.INC'
      INCLUDE 'D0$PARAMS:CATENM.PARAMS'
C
      INTEGER LPMUO,GZPMUO
      INTEGER IER,I,J,VERS,OLDNTWRS(2),ADDNTWRS
      INTEGER GZCAEH,GZCATE,GZCAHT,GZMUON,LMUON,GZJETS
      INTEGER NTRAKS,ITRAK,LLJETS,NRT,NJETS
      REAL    R
      EQUIVALENCE (J,R)
      CHARACTER ALG_NAME*20
      LOGICAL FIRST,OK,CLUFND,CJET_MUON,USE_MUJET_DEBUG_TOOLS
      LOGICAL DROP_UNASSOC_JETS,EZERR,GOOD_MUONS
      INTEGER PRUNIT,GZCAEP,NEW_WORD4(1000),NUM_NEWCELLS
      INTEGER PACKWD,IETA,IPHI,ILYR,MUJETS_IFW4_MAX
      BYTE    BYTE1(4)
      EQUIVALENCE (BYTE1(1),PACKWD)
C
      DATA PRUNIT/40/
      DATA FIRST/.TRUE./
      SAVE FIRST
      SAVE DROP_UNASSOC_JETS
      SAVE MUJETS_IFW4_MAX
C----------------------------------------------------------------------
      IF ( FIRST )THEN                  ! LOCAL INIT
        FIRST = .FALSE.
        CALL INRCP('CAJETS_RCP',IER)
        CALL EZPICK('CAJETS_RCP')       ! SELECT JETS RCP BANK
        IF (IER.EQ.0) 
     &    CALL EZGET_l('DROP_UNASSOC_JETS',DROP_UNASSOC_JETS,IER)
        IF (IER.EQ.0) 
     &    CALL EZGET('MUJETS_IFW4_MAX',MUJETS_IFW4_MAX,IER)
        IF (EZERR(IER)) THEN
          CALL ERRMSG('NO_CAJETS_RCP','CJET_MUON',
     &      'CAJETS_RCP not found in CJET_MUON.','W')
        ENDIF
        CALL EZRSET
      ENDIF
C
      CJET_MUON = .TRUE.
      GOOD_MUONS = .FALSE.
C
      LPMUO = GZPMUO(0)
      IF (LPMUO.EQ.0) GO TO 999   ! Do nothing if no muons in event
      DO WHILE (LPMUO.GT.0)
        IF ( IQ(LPMUO+9).LE.MUJETS_IFW4_MAX ) GOOD_MUONS = .TRUE.
        LPMUO = LQ(LPMUO)
      END DO
      IF ( .NOT. GOOD_MUONS ) GO TO 999 
C
      CALL CONCLI                     ! PARAMETERS FOR CONCLU
      CALL CLUPRI                     ! PARAMETERS FOR CLUPRE
      CALL SPLINI                     ! PARAMETERS FOR SPLJET
C
C *** Book and fill copy of CAEP bank.
C
      CALL CJET_MUCAEPFL(OK)      ! fill CAEP
C
C *** Loop over muons, add muon momenta to CAEP bank:
C
      CALL CJET_MUCAEPADD(NEW_WORD4,NUM_NEWCELLS)
C      CALL CPTCAZ             ! zero PTCAEP
C      CALL CPTCAF             ! fill PTCAEP pointer array
C
C *** Book and fill copies of CAEH,CATE banks:
C
      CALL CAEHFL_FORCE       ! Force creation of second CAEH
      CALL CAEHFL             ! fill CAEH
      CALL CPTCTZ             ! zero PTCATE pointer array
      CALL CATEFL_FORCE       ! Force creation of second CATE
      CALL CATEFL             ! fill CATE
C      CALL CPTCTF             ! fill PTCATE pointer array
C
C ****  Book/Fill CAPH bank for MUON algorithm
C
      CALL BKCAPH(LCAPH)
      IF ( LCAPH .LE. 0 ) THEN
        CALL ERRMSG
     &    ('CALORIMETER','CJET_MUON','Unable to book CAPH','W')
        OK = .FALSE.
        GOTO 999
      ENDIF
      CALL CAPHFL_INT(K_ALGORITHM,A_MUON_JET)
C
C ****  FILL remainder of CAPH bank with ALGORITHM parameters
C
      IF ( NPARAMS.NE.NPARAMS_MUON) THEN
        CALL ERRMSG
     &    ('CJET_MUON PARAMETERS WRONG','CJET_MUON',
     &    'Unable to FILL CAPH','W')
        OK = .FALSE.
        GOTO 999
      ENDIF
C
      DO I = 2, NPARAMS
        J = ALG_PARAMS(I)
        CALL CAPHFL_REAL (K_BASE + I - 1 , R )
      END DO
C
C *** Find jets (book/fill JETS bank)
C
      OK = CLUFND()
C
C *** Fill links in PMUO banks to associated JETS banks.
C
      CALL CJET_MULINKS()
C
C *** Subtract muons from JETS banks to get 'residual' JETS:
C
      CALL CJET_MUSUBTRACT()
C
C *** Restore original CAEP,CAEH,CATE banks.  Drop altered banks first, 
C     so links to old ones will be reestablished:
C
      LCAEP = GZCAEP()
      IF ( LCAEP.GT.0 ) THEN
        CALL MZDROP(IXMAIN,LCAEP,' ')
      END IF
      LCAEH = GZCAEH()
      IF ( LCAEH.GT.0 ) THEN
        CALL MZDROP(IXMAIN,LCAEH,' ')
      END IF
      LCATE = GZCATE()
      IF ( LCATE.GT.0 ) THEN
        CALL MZDROP(IXMAIN,LCATE,' ')
      END IF
C
C ***  Zero pointers to CAEP cells created by cjet_mucaepadd:
C     
      IF (NUM_NEWCELLS .GT. 0) THEN
        DO I = 1, NUM_NEWCELLS      
          PACKWD = NEW_WORD4(I)
          ILYR = BYTE1(2)     ! byte(2)
          IPHI = BYTE1(3)     ! byte(3)
          IETA = BYTE1(4)     ! byte(4)
          PTCAEP(IETA,IPHI,ILYR) = 0
        END DO
        LCATE = GZCATE()
        IF ( LCATE.GT.0 ) THEN
          CALL CPTCTZ     ! zero PTCATE pointer array
          CALL CPTCTF     ! fill PTCATE pointer array
        ELSE
          CALL ERRMSG('CATE bank not restored','CJET_MUON',
     &    'aborting CJET_MUON','W')
          GO TO 999
        END IF
      END IF
  999 CONTINUE
      CJET_MUON = OK
      RETURN
      END
