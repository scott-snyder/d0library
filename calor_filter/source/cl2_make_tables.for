      SUBROUTINE CL2_MAKE_TABLES
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-      Build CADT and CAGS tables for CL2_xxx unpacking and move into position
C-      for downloading; to be called by TOOL_INIT's which use CL2_xxx
C-   Inputs  : calorimeter geometry and calibration banks (possibly DBL3)
C-   Outputs : CAGS, CADT, and CGEH geometry under SL2H
C-   Controls: CAHITS and CL2HITS RCP files
C-
C-   Created  27-APR-1991   James T. Linnemann
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      LOGICAL CALOR_INI,CHTINI
      LOGICAL EZERROR,OK
      INTEGER IER
      LOGICAL D0_DATA,MONTE_CARLO
      INTEGER SFTVSN,TBLOAD,NOISE_TYPE
      LOGICAL DO_GNSCOR,PLATE,BUILD_CSF,DATA_DRIVE_CSF
      LOGICAL FIRST
      SAVE FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      IF (FIRST) THEN
        CALL INZSTP
        OK = CALOR_INI()                 ! get geometry; read in CADT
        OK = OK.AND.CHTINI()             ! and set flags from rcp files
C
C...check validity of options in CAHITS_RCP
        CALL EZPICK('CAHITS_RCP')        ! Select bank
        OK = OK.AND..NOT.EZERROR(IER)
        IF (IER .EQ. 0) CALL EZGET('DO_GNSCOR',DO_GNSCOR,IER)
        IF (IER .EQ. 0) CALL EZGET('BUILD_CSF',BUILD_CSF,IER)
        IF (IER .EQ. 0) CALL EZGET('DATA_DRIVE_CSF',DATA_DRIVE_CSF,IER)
        IF (IER .NE. 0) THEN      ! Error reading RCP
          CALL ERRMSG('NO CAHITS_RCP','CL2_MAKE_TABLES',
     &  ' Error while reading CAHITS_RCP','F')
        ELSE
          IF (BUILD_CSF) THEN
            CALL ERRMSG('NO BUILD CSF','CL2_MAKE_TABLES',
     &        ' Can''t build sampling fractions in CL2','F')
          ENDIF
          IF (.NOT.DATA_DRIVE_CSF) THEN
            CALL ERRMSG('NEED DATA DRIVE CSF','CL2_MAKE_TABLES',
     &        ' Must use data driven sampling fractions in CL2','F')
          ENDIF
          CALL EZRSET
          CALL L2_VERT_INIT                 ! prepare for use of L2_VERT in CL2 
        ENDIF
        CALL INRCP('CL2HITS_RCP',IER)    ! Read in RCP file
        IF (IER .EQ. 0) THEN
          CALL EZPICK('CL2HITS_RCP')       ! Select bank
          OK = OK.AND..NOT.EZERROR(IER)
        ENDIF
        IF (IER .EQ. 0) CALL EZGET('D0_DATA',D0_DATA,IER)
        TBLOAD = 0
        IF (IER .EQ. 0) CALL EZGET('MONTE_CARLO',MONTE_CARLO,IER)
        IF (IER .EQ. 0) CALL EZGET('SFTVSN',SFTVSN,IER)
        IF (IER .EQ. 0) CALL EZGET('PLATE',PLATE,IER)
        IF (IER .EQ. 0) CALL EZGET('NOISE_TYPE',NOISE_TYPE,IER)
        IF (IER .NE. 0) THEN      ! Error reading RCP
          CALL ERRMSG('NO CL2HITS_RCP','CL2_MAKE_TABLES',
     &  ' Error while reading CL2HITS_RCP','F')
        ELSE
          CALL EZRSET
        ENDIF

        IF (.NOT.D0_DATA) THEN
          CALL ERRMSG('CADT','CL2_MAKE_TABLES',
     &      'NWA Data not supported in CL2','W')
        ELSE
          IF (.NOT.MONTE_CARLO) THEN
            CALL ERRMSG('CADT','CL2_MAKE_TABLES',
     &          'assume you are reading real D0 Data','W')
          ELSE
            IQ(LHEAD+1) = 1005  !MC record type
            IF (SFTVSN.EQ.1) THEN
              CALL ERRMSG('CADT','CL2_MAKE_TABLES',
     &'assume you are reading MC of series M or older','W')
            ELSEIF (SFTVSN.EQ.2) THEN
              CALL ERRMSG('CADT','CL2_MAKE_TABLES',
     &'assume you are reading MC of series N','W')
            ELSEIF (SFTVSN.EQ.3) THEN
              CALL ERRMSG('CADT','CL2_MAKE_TABLES',
     &'assume you are reading MC of series P, or Q','W')
            ELSEIF (SFTVSN.EQ.4) THEN
              CALL ERRMSG('CADT','CL2_MAKE_TABLES',
     &'assume you are reading MC of series R, S, or using CADMAKE','W')
            ELSE
              CALL ERRMSG('CADT','CL2_MAKE_TABLES',
     &'unknown SFTVSN','F')
            ENDIF
          ENDIF
        ENDIF
C
C... build gain*nominal sine theta table in their level 2 spots
C        CALL ERRMAX('USE OLD MC CAL ADDRESSING',0,0)  !turn off  redundant msgs
        CALL CL2_SET_STP(MONTE_CARLO,TBLOAD,SFTVSN,NOISE_TYPE,PLATE)
        CALL CAGSFL(DO_GNSCOR,D0_DATA,MONTE_CARLO,SFTVSN) 
        CALL CL2_CADTFL(DO_GNSCOR)        ! add pointer information to CADT
        FIRST = .FALSE.
      ENDIF
C
C...this is outside the IF(FIRST) so as to override any call to CL2_INI
C      CALL CL2_SHRINK_RCP
      CALL CL2_PUT_GEOM(MONTE_CARLO)    ! offline geometry ready for download
  999 RETURN
      END
