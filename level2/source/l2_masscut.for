      SUBROUTINE L2_MASSCUT( PARAM_SET_NUMBER,HARDWARE,RESULT_FLAG,
     &  EXTRA_FLAG)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : TOOL to cut on INVARIANT MASS of either the
C-                             leading 2 objects in ET, or on the leading
C-                             mass within an eta and eta-boost range.
C-
C-   Inputs  : PARAM_SET_NUMBER : # of parameter set to use
C-             HARDWARE:          mask of set bits for LV1 trigger which started
C-                                  this filter.
C-   Outputs : RESULT_FLAG :      Flag set to TRUE when we want to pass tool
C-                                  under this PARAM_SET_NUMBER
C-             EXTRA_FLAG  :      Set to TRUE when we want to pass event and
C-                                  do no further filtering. (NOT IMPLEMENTED)
C-   Controls:
C-
C-   Created  4-AUG-1992   Kathy Fatyga
C-   Modified 19-APR-1994  Lewis Taylor Goss
C----------------------------------------------------------------------
C*WARNING* because of the way in which the L2EM bank is filled, it is not
Crecommended that you set LEADING .TRUE. simultaneously with OBJECT = ELECTRON.
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER PARAM_SET_NUMBER,HARDWARE
      LOGICAL EXTRA_FLAG,RESULT_FLAG
      INCLUDE 'D0$INC:ZEBCOM.INC'               ! zebra main store
      CHARACTER*80 MSG
C
      INCLUDE 'D0$PARAMS:ESUM.PARAMS'
      INCLUDE 'D0$INC:L2_MASSCUT_CUTS.INC'
      INCLUDE 'D0$INC:RMAS_VALUES.INC'
C
C
      CHARACTER*10 OBJECT
      CHARACTER*6 MUO,PHO,ELE,JET,SUB
C
      INTEGER IP,NPARIN,IER,NOBJECTS,NUMF
      INTEGER NCHR,I,J,POS1,POS2
      INTEGER ID_OBJECT,TRULEN
      INTEGER L2JETS_CURRENT_ESUM_OBJECT
      INTEGER LRMAS
C
C
      REAL ET(NMAX),ETA(NMAX),PHI(NMAX)
C
      LOGICAL EZERROR,OK,MASSTEST
      LOGICAL OBJECT_HAS
C
      PARAMETER(MUO='MUO')
      PARAMETER(PHO='PHO')
      PARAMETER(ELE='ELE')
      PARAMETER(JET='JET')
C       parameters for booking each event ....
      INTEGER EVT_NUMBER,CURRENT_EVENT_NUMBER
      DATA    EVT_NUMBER / -9000/     ! Impossible event number
C----------------------------------------------------------------------
C...statement function returns true if substring SUB found in OBJECT
      OBJECT_HAS(SUB) = (INDEX(OBJECT,SUB(1:TRULEN(SUB))).NE.0)
C----------------------------------------------------------------------

C
C Initialize output logicals
C
      EXTRA_FLAG = .FALSE.
      RESULT_FLAG = .FALSE.
C
C Retrieve cuts from RCP
C
      IP = PARAM_SET_NUMBER
      CALL EZPICK('L2_MASSCUT') !downloaded from configuration file(.FILTs)
      OK = .NOT.EZERROR(IER)
      IF (OK) THEN
C
C Is IP consistent with the number of sets which exist?
C
        CALL EZGET('NUMBER_OF_SETS',NPARIN,IER)
        IF (IER.EQ.0) THEN
          IF ((IP.LE.0).OR.(IP.GT.NPARIN)) THEN
            WRITE(MSG,'(A,I5,A,I5)')' parameter set requested = ',
     &        PARAM_SET_NUMBER, ' but only had ',NPARIN
            CALL ERRMSG('L2_MASS_PARSET','L2_MASSCUT',MSG,'W')
            GO TO 999
          ENDIF
C
C Get the parameters from the IPth set
C
          CALL EZGETS('OBJECT',IP,OBJECT,NCHR,IER)
          IF (IER.EQ.0) CALL EZGETA('NOBJECTS',IP,IP,1,NOBJECTS,IER)
          IF(NOBJECTS.LT.2)THEN
             CALL ERRMSG('L2_MASSCUT_NOBJ','L2_MASSCUT',
     &         'Number of objects requested fewer than 2.','F')
          ENDIF
          IF (IER.EQ.0) CALL EZGETA('LEADING',IP,IP,1,LEADING,IER)
          IF (IER.EQ.0) CALL EZGETA('ETA1_MIN',IP,IP,1,ETA1_MIN,IER)
          IF (IER.EQ.0) CALL EZGETA('ETA1_MAX',IP,IP,1,ETA1_MAX,IER)
          IF (IER.EQ.0) CALL EZGETA('ETA2_MIN',IP,IP,1,ETA2_MIN,IER)
          IF (IER.EQ.0) CALL EZGETA('ETA2_MAX',IP,IP,1,ETA2_MAX,IER)
          IF (IER.EQ.0) CALL EZGETA('ABS_ETABOOST_MIN',IP,IP,1,
     &      ETABOOST_MIN,IER)
          IF (IER.EQ.0) CALL EZGETA('ABS_ETABOOST_MAX',IP,IP,1,
     &      ETABOOST_MAX,IER)
          IF (IER.EQ.0) CALL EZGETA('MASS_MIN',IP,IP,1,MASS_MIN,IER)
          IF (IER.EQ.0) CALL EZGETA('MASS_MAX',IP,IP,1,MASS_MAX,IER)
          IF (IER .NE. 0) THEN      ! Error reading RCP
             CALL ERRMSG('L2_MASSCUT_PARAMETERS','L2_MASSCUT',
     &          'Couldn''t find parameter','F')
          ENDIF
        ENDIF
      ELSE         !(.NOT.OK)
        CALL ERRMSG('L2_MASSCUT_RCP','L2_MASSCUT','Couldn''t find bank',
     &    'F')
      ENDIF
C
C Get type of object
C
      IF(OBJECT_HAS(JET)) THEN
        ID_OBJECT=L2JETS_CURRENT_ESUM_OBJECT()
        PAIR_TYPE=1
      ELSE IF(OBJECT_HAS(MUO)) THEN
        ID_OBJECT=ID_MUON
        PAIR_TYPE=3
      ELSE IF(OBJECT_HAS(ELE)) THEN
        ID_OBJECT=ID_ELECTRON
        PAIR_TYPE=2
      ELSE IF(OBJECT_HAS(PHO)) THEN
        ID_OBJECT=ID_PHOTON
        PAIR_TYPE=2
      ELSE
        CALL ERRMSG('L2_MASSCUT_OBJ_BAD','L2_MASSCUT',
     &    'Object type is not a jet muon electron or photon' ,'F')
      END IF
C
C  get Number of objects, Et, Eta, Phi, and positions of 2 leading
C
      CALL GET_NEEP(IP,ID_OBJECT,NUMF,ET,ETA,PHI,POS1,POS2)
C
      PAIR_STATUS=0
C  cut on number of objects
      OBJN_VAL=NUMF
      IF(NUMF.LT.NOBJECTS)THEN
         PAIR_STATUS=1000000
         GOTO 999
      ENDIF
C  pass events with greater than NMAX objects
      IF(NUMF.GT.NMAX)THEN
        RESULT_FLAG=.TRUE.
        CALL ERRMSG('L2_MASSCUT_NUMF','L2_MASSCUT','Number of objects
     +             is greater than NMAX, event is passed!' ,'W')
        PAIR_STATUS=-1000000
        GOTO 999
      ENDIF
      IF(LEADING)THEN  !cut on leading 2 objects
        RESULT_FLAG= MASSTEST(ET(POS1),ET(POS2),ETA(POS1),ETA(POS2),
     &    PHI(POS1),PHI(POS2))
      ELSE             !cut on all pairs
        PAIR_PASS     = .TRUE.
        PAIR_MASS     = -1.
        PAIR_BOOST    = 0.0
        PAIR_VAL(1,1) = 0.0
        PAIR_VAL(1,2) = 0.0
        PAIR_VAL(1,3) = 0.0
        PAIR_VAL(2,1) = 0.0
        PAIR_VAL(2,2) = 0.0
        PAIR_VAL(2,3) = 0.0
        N_STAT=0
        DO I=1,NUMF-1
           DO J= I+1, NUMF
              IF(MASSTEST(ET(I),ET(J),ETA(I),ETA(J),PHI(I),PHI(J)))
     &                  RESULT_FLAG=.TRUE.
           ENDDO
        ENDDO
      ENDIF
  999 CONTINUE
      IF (OK) CALL EZRSET
C
      IF (PAIR_MASS.EQ.0.0) CALL ERRMSG('ZERO_PAIR_MASS','L2_MASSCUT',
     &  'Invariant mass is zero!','W')
C
C *** Book the result bank on the first call ****
      CURRENT_EVENT_NUMBER = IQ( LHEAD+ 7 )
      IF(EVT_NUMBER .NE. CURRENT_EVENT_NUMBER) THEN
        CALL BKRMAS(NPARIN,LRMAS)
        EVT_NUMBER      = CURRENT_EVENT_NUMBER
      ENDIF
C
C *** Fill the result bank ***
      CALL RMASFL(IP,LRMAS)
C
C
      RETURN
      END
