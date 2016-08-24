      SUBROUTINE L2TAU( PARAM_SET_NUMBER,HARDWARE,RESULT_FLAG,
     &  EXTRA_FLAG)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : TAU TOOL :
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
C-   Created  21-JUL-1992   Amber Boehnlein, based on L2_GENERIC
C-   Modified 20-JUL-1993   Amber Boehnlein, fixed bug in calling JAUX results
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER PARAM_SET_NUMBER,HARDWARE
      LOGICAL EXTRA_FLAG,RESULT_FLAG
      INCLUDE 'D0$INC:ZEBCOM.INC'               ! zebra main store
      INCLUDE 'D0$INC:PI.DEF'
      INCLUDE 'D0$PARAMS:JAUX.PARAMS'
      INCLUDE 'D0$PARAMS:ESUM.PARAMS'
      CHARACTER*80 MSG
      REAL    JET_RMS_MAX, JET_MET_DPHI_MIN, JET_JET_DPHI_MAX
      INTEGER NUM_JETS_MIN
      REAL    JET_ET_MIN, MET_MIN
C
      INTEGER NJET,ICAND,NCAND
      INTEGER IP,NPARIN,IER
      INTEGER PAR_SET,L2JETS_CURRENT_PARAM
      LOGICAL START
      LOGICAL EZERROR,OK
      INTEGER NFOUND(ID_ALL:LAST_TYPE),ID,IFLAG
      REAL    ET,ETA_PHYS,ETA_DET,ETA,PHI
      REAL    ETASIZE,PHISIZE,EMET 
      INTEGER L1ETA,L1PHI 
      REAL    JET1_PHI,JET2_PHI,ETMISS_PHI
      REAL    DEL_PHI
      REAL    CONESIZE
      REAL RMS,RMS_MIN
      INTEGER NMAX
      PARAMETER(NMAX = 40)    !be generous
      INTEGER IORDER(NMAX)
      REAL    WORK(NMAX)
      INTEGER LJET,SJET
      DATA LJET/1/
      DATA SJET/2/
C----------------------------------------------------------------------
      EXTRA_FLAG = .FALSE.
      RESULT_FLAG = .FALSE.
C
C...first, carefully retrieve cuts from RCP
      IP = PARAM_SET_NUMBER !I'm getting tired of typing this
      CALL EZPICK('L2TAU') ! downloaded from configuration file (.FILTs)
      OK = .NOT.EZERROR(IER)
      IF (OK) THEN
C...is IP consistent with the number of sets which exist?
        IF (IER .EQ. 0) CALL EZGET('NUMBER_OF_SETS',NPARIN,IER)
        IF (IER.EQ.0) THEN
          IF ((IP.LE.0).OR.(IP.GT.NPARIN)) THEN
            WRITE(MSG,'(A,I5,A,I5)')' parameter set requested = ',
     &        PARAM_SET_NUMBER, ' but only had ',NPARIN
            CALL ERRMSG('L2TAU','L2TAU',MSG,'F')
            GO TO 999
          ENDIF
C...Get the parameters from the IPth set
          IF (IER.EQ.0) CALL EZGETA('JET_RMS_MAX',IP,IP,1,JET_RMS_MAX,
     &      IER)
          IF (IER.EQ.0) CALL EZGETA('JET_MET_DPHI_MIN',IP,IP,1,
     &      JET_MET_DPHI_MIN,IER)
          IF (IER.EQ.0) CALL EZGETA('JET_JET_DPHI_MAX',IP,IP,1,
     &      JET_JET_DPHI_MAX,IER)
          IF (IER.EQ.0) CALL EZGETA('NUM_JETS_MIN',IP,IP,1,
     &      NUM_JETS_MIN,IER)
          IF (IER.EQ.0) CALL EZGETA('JET_ET_MIN',IP,IP,1,
     &      JET_ET_MIN,IER)
          IF (IER.EQ.0) CALL EZGETA('MET_MIN',IP,IP,1,
     &      MET_MIN,IER)
        ENDIF
      ENDIF
      IF (.NOT.OK) THEN
        CALL ERRMSG('L2TAU','L2TAU','Couldn''t find bank','F')
      ELSEIF (IER .NE. 0) THEN      ! Error reading RCP
        CALL ERRMSG('L2TAU','L2TAU_PARAMETERS',
     &          'Couldn''t find parameter','F')
      ENDIF
C
C ****  Ascertain that the l2jets tool and l2etmiss tool were called
C
      CALL GTESUM_COUNTS('FILT',NFOUND,IER)
      IF (IER.NE.0) THEN
        WRITE(MSG,'(A,I6)')'IER From GTESUM_COUNTS = ',IER
        CALL ERRMSG('GTESUM_COUNTS','L2TAU',MSG,'W')
      ENDIF
      RESULT_FLAG = (NFOUND(ID_JET).GE.NUM_JETS_MIN)
      IF(RESULT_FLAG) THEN
        CALL GTESUM_SORT('FILT',ID_JET,NMAX,IORDER,WORK,IER)
        IF (IER.NE.0) THEN
C...this should only happen if you didn't have enough space reserved (NMAX)
          WRITE(MSG,'(A,I6)')'IER From GTESUM_SORT (FILT) = ',IER
          CALL ERRMSG('GTESUM_SORT','L2TAU',MSG,'W')
        ENDIF
        CALL GTESUM('FILT',ID_JET,IORDER(LJET),ET,ETA_PHYS,
     &                ETA_DET,JET1_PHI,IFLAG,IER)
        RESULT_FLAG = RESULT_FLAG.AND. (ET.GE.JET_ET_MIN)
        IF (RESULT_FLAG ) THEN  ! does leading jet et pass cut?
          CALL GTESUM('FILT',ID_ETMISS,NFOUND(ID_ETMISS),ET,ETA_PHYS,
     &                ETA_DET,ETMISS_PHI,IFLAG,IER)
C
          RESULT_FLAG= RESULT_FLAG.AND.(ET.GE.MET_MIN)
          IF ( RESULT_FLAG  ) THEN
            DEL_PHI = ABS(ETMISS_PHI-JET1_PHI)
            IF(DEL_PHI.GT.SNGL(PI)) DEL_PHI = SNGL(TWOPI) - DEL_PHI
            RESULT_FLAG = RESULT_FLAG.AND.(DEL_PHI
     &           .GT.JET_MET_DPHI_MIN)
C
C ****  Check on second jet, if available
C
            IF(RESULT_FLAG) THEN
              IF(NFOUND(ID_JET).GE.2) THEN
                CALL GTESUM('FILT',ID_JET,IORDER(SJET),ET,ETA_PHYS,
     &                         ETA_DET,JET2_PHI,IFLAG,IER)
                DEL_PHI = ABS(JET1_PHI-JET2_PHI)
                IF(DEL_PHI.GT.SNGL(PI)) DEL_PHI = SNGL(TWOPI) - DEL_PHI
                RESULT_FLAG = RESULT_FLAG.AND.(DEL_PHI.
     &              LT.JET_JET_DPHI_MAX)
              ENDIF !checking delta phi for two jets
C
C ****  Get the jaux bank and cut on the rms of the jet, making sure that
C       at least one jet in the event passes our criteria
C
              IF(RESULT_FLAG) THEN
                PAR_SET = L2JETS_CURRENT_PARAM()
                RMS_MIN = 99999.
                START = .TRUE.
                CONESIZE = -1.
                DO WHILE (IER.GE.0)
                CALL GTJAUX(START, CONESIZE, PAR_SET, ET, ETA, PHI,
     &            L1ETA,L1PHI, ETASIZE, PHISIZE, EMET, IER )
                  RMS = SQRT(ETASIZE**2+PHISIZE**2)/ET
                  IF(RMS.LT.RMS_MIN)RMS_MIN=RMS
                ENDDO
               RESULT_FLAG= RESULT_FLAG.AND.(RMS_MIN.LE.JET_RMS_MAX)
              ENDIF !checking phi between leading and second jet if available
            ENDIF !checking phi between met and leading jet
          ENDIF !making sure of et miss object
        ENDIF !making sure of leading jet et
      ENDIF  !making sure of a jet
  999 CONTINUE
      IF (OK) CALL EZRSET
      RETURN
      END
