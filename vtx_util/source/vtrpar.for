      FUNCTION VTRPAR()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : run initialization for VTRAKS package
C-
C-   Inputs  :
C-   Outputs : .TRUE. if run is to be processed (.FALSE. if skipped)
C-   Controls:
C-
C-   Created  12-OCT-1988   Daria Zieminska
C-   Updated  17-JAN-1990   Peter Grudberg - default STP file name in RCP
C-   Updated   7-FEB-1991   Peter Grudberg  Handle real data, use DBL3
C-   Updated   8-APR-1991   Peter Grudberg  Restructure; more real data stuff
C-   Updated   4-NOV-1991   Peter M. Grudberg  Improve error handling
C-   Updated  17-MAR-1992   Peter M. Grudberg  Add different STP files for
C-                                             real/MC data.
C-   Updated   9-APR-1992   Peter M. Grudberg  Handle old rcp version
C-   Updated  22-MAY-1992   Peter M. Grudberg  Handle DBL3 failure
C-   Updated  25-SEP-1992   Peter M. Grudberg  Move DBL3 message
C-   Updated  19-OCT-1992   Peter M. Grudberg  Update call to VDBINI; change
C-                                             messages
C-   Updated  11-DEC-1992   Liang-ping Chen    Add BYPASS_DBL3_ERROR
C-   Updated  12-FEB-1994   Ed Oltman          SKIP DBL3 ACCESS IF VCAL BANK 
C-                                               EXISTS 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL VTRPAR
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
C
      INTEGER RUN, RUNNO, IDX, LENGTH, IER
      INTEGER MAX_VTXCRT, PREV_RUN
      INTEGER LVTMW, GZVTMW, IVERS
      CHARACTER*(*) GEOFIL
      CHARACTER*50 FILNAM
      CHARACTER*80 TEXT
      CHARACTER*60 VPDHFILE, VGNHFILE, VTMHFILE
      INTEGER PD_METH, GN_METH, TM_METH
      INTEGER LEN, TRULEN
      LOGICAL OK, MCDATA, FIRST, EZERROR
      LOGICAL INIT_DTM, PD_INI, GN_INI, TM_INI
      LOGICAL BYPASS_DBL3_ERROR,DO_DBL3
      PARAMETER( GEOFIL = 'VTX_STPFILE' )
      INTEGER GZVCAL,LVCAL
C
      REAL DFLT_PD, DFLT_PD_SIG, DFLT_TM
      REAL DFLT_TM_SIG, DFLT_GN, DFLT_GN_SIG
C
      EXTERNAL EZERROR
      SAVE FIRST, MAX_VTXCRT, PREV_RUN
      DATA MCDATA / .FALSE. /
      DATA FIRST / .TRUE. /
      DATA PREV_RUN / -1 /
C----------------------------------------------------------------------
      VTRPAR = .FALSE.
      RUN = RUNNO()
      IF( RUN .EQ. PREV_RUN .AND. .NOT. FIRST) THEN
        VTRPAR = .TRUE.
        GO TO 999
      ELSE
        PREV_RUN=RUN
      ENDIF
C
      IF ( IQ(LHEAD+1) .GT. 1000 ) MCDATA = .TRUE.
      LVCAL = GZVCAL(1)
      DO_DBL3 = LVCAL .LE. 0
      IF (MCDATA) DO_DBL3 = .FALSE.
C
C ****  Only read in STP file for the first run processed.  For real data,
C ****  run dependent parameters will be obtained from the DBL3 database.
C
      IF( FIRST ) THEN
        FIRST = .FALSE.
        CALL EZPICK('VTRAKS_RCP')
        IF ( EZERROR(IER) ) THEN
          CALL INTMSG(' VTRPAR: VTRAKS_RCP not found!')
          GO TO 999
        ELSE
          IDX = 1
          IF ( .NOT. MCDATA ) IDX = 2
          CALL EZGETS(GEOFIL, IDX, FILNAM, LENGTH, IER)
          IF ( LENGTH .EQ. 0 ) THEN
C
C ****  Handle old version of VTRAKS.RCP that does not have the second element
C ****  of the VTX_STPFILE array
C
            CALL INTMSG('STP file name for real data not in VTRAKS_RCP')
            CALL INTMSG('Using MC version')
            IDX = 1
            CALL EZGETS(GEOFIL, IDX, FILNAM, LENGTH, IER)
          ENDIF
C
          PD_METH = 0
          GN_METH = 0
          TM_METH = 0
          CALL EZGET('PD_INI',PD_INI,IER)
          CALL EZGET('GN_INI',GN_INI,IER)
          CALL EZGET('TM_INI',TM_INI,IER)
          IF ( PD_INI ) CALL EZGET('PD_METH',PD_METH,IER)
          IF ( GN_INI ) CALL EZGET('GN_METH',GN_METH,IER)
          IF ( TM_INI ) CALL EZGET('TM_METH',TM_METH,IER)
          CALL EZGETS('VPDHFILE',1,VPDHFILE,LENGTH,IER)
          CALL EZGETS('VGNHFILE',1,VGNHFILE,LENGTH,IER)
          CALL EZGETS('VTMHFILE',1,VTMHFILE,LENGTH,IER)
          CALL EZGET('INIT_DTM', INIT_DTM, IER)
          CALL EZGET('MAX_VTXCRT', MAX_VTXCRT, IER)
C
          CALL EZGET('DFLT_PD',DFLT_PD,IER)
          CALL EZGET('DFLT_PD_SIG',DFLT_PD_SIG,IER)
          CALL EZGET('DFLT_TM',DFLT_TM,IER)
          CALL EZGET('DFLT_TM_SIG',DFLT_TM_SIG,IER)
          CALL EZGET('DFLT_GN',DFLT_GN,IER)
          CALL EZGET('DFLT_GN_SIG',DFLT_GN_SIG,IER)
          CALL EZGET('BYPASS_DBL3_ERROR',BYPASS_DBL3_ERROR,IER)
C
          CALL EZRSET
C
          IF ( DO_DBL3 ) THEN
            IF ( PD_METH .EQ. 0 ) THEN
              CALL INTMSG(' VTX PEDS: Using values from STP file')
            ELSEIF ( PD_METH .EQ. 1 ) THEN
              CALL INTMSG(' VTX PEDS: Using online DBL3 calibration')
            ELSEIF ( PD_METH .EQ. 2 ) THEN
              CALL INTMSG(' VTX PEDS: Using offline DBL3 calibration')
            ELSEIF ( PD_METH .EQ. 3 ) THEN
              LEN = TRULEN(VPDHFILE)
              WRITE(TEXT,'(A,A)')
     &          ' VTX PEDS: Using VPDH file ', VPDHFILE(1:LEN)
              CALL INTMSG(TEXT)
            ELSE
              CALL INTMSG(' VTX PEDS: Invalid method, using default')
              PD_METH = 1       ! Use online peds from DBL3
            ENDIF
C
            IF ( GN_METH .EQ. 0 ) THEN
              CALL INTMSG(' VTX GAINS: Using values from STP file')
            ELSEIF ( GN_METH .EQ. 1 ) THEN
              CALL INTMSG(' VTX GAINS: Using online DBL3 calibration')
            ELSEIF ( GN_METH .EQ. 2 ) THEN
              CALL INTMSG(' VTX GAINS: Using offline DBL3 calibration')
            ELSEIF ( GN_METH .EQ. 3 ) THEN
              LEN = TRULEN(VGNHFILE)
              WRITE(TEXT,'(A,A)')
     &          ' VTX GAINS: Using VGNH file ', VGNHFILE(1:LEN)
              CALL INTMSG(TEXT)
            ELSE
              CALL INTMSG(' VTX GAINS: Invalid method, using default')
              GN_METH = 0       ! Use gains from STP file
            ENDIF
C
            IF ( TM_METH .EQ. 0 ) THEN
              CALL INTMSG(' VTX TIMES: Using values from STP file')
            ELSEIF ( TM_METH .EQ. 1 ) THEN
              CALL INTMSG(' VTX TIMES: Using online DBL3 calibration')
            ELSEIF ( TM_METH .EQ. 2 ) THEN
              CALL INTMSG(' VTX TIMES: Using offline DBL3 calibration')
            ELSEIF ( TM_METH .EQ. 3 ) THEN
              LEN = TRULEN(VTMHFILE)
              WRITE(TEXT,'(A,A)')
     &          ' VTX TIMES: Using VTMH file ', VTMHFILE(1:LEN)
              CALL INTMSG(TEXT)
            ELSE
              CALL INTMSG(' VTX TIMES: Invalid method, using default')
              TM_METH = 0       ! Use times from STP file
            ENDIF
C
          ELSEIF (.NOT. MCDATA) THEN
            CALL INTMSG(' VTX GAINS and T0''S on DATA file in VCAL')
          ENDIF
C
        ENDIF
C
C **** Read in VTX STP file
C
        CALL VTISTP(FILNAM, IER)
C
        IF( IER .NE. 0 ) THEN
          CALL INTMSG(' VTRPAR: can not open file '//FILNAM)
          CALL ERRMSG('VTRAKS','VTRPAR',
     &                'VTX STP file not found, abort','F')
C
C ****  Shouldn't ever get here, but just in case . . .
C
          VTRPAR = .FALSE.
          GO TO 999
        ELSE
          VTRPAR=.TRUE.
          CALL INTMSG(' Read Static Parameter File '//FILNAM)
        ENDIF
C
C ****  If requested, drop the default time banks, and rebook them, hanging the
C ****  appropriate time-to-distance banks (VDTM) from them
C
        IF ( .NOT. MCDATA ) THEN
C
C ****  Check for correct version of VTMW; if not correct, force initialization
C ****  of time-distance maps
C
          LVTMW = GZVTMW(0)
          IF ( LVTMW .GT. 0 ) THEN
C
C ****  Check version number
C
            IVERS = IBITS(IC(LVTMW),13,5)
            IF ( IVERS .LT. 1 ) INIT_DTM = .TRUE.
          ELSE
            CALL ERRMSG('VTRAKS','VTRPAR',
     &                  'No VTMW bank in STP file','F')
          ENDIF
          IF ( INIT_DTM ) THEN
            CALL INIT_VDTM(OK)
            IF ( .NOT. OK ) THEN
              CALL ERRMSG('VTRAKS','VTRPAR',
     &                'Error in INIT_VDTM, abort','F')
              VTRPAR = .FALSE.
              GO TO 999
            ENDIF
          ENDIF
        ENDIF
      ENDIF
C
C ****  For real data, we must get the calibration numbers from the database,
C ****  and use a realistic time to distance conversion.
C
      IF ( DO_DBL3 ) THEN
C
C **** Read in VTX STP values from DBL3, if necessary
C **** Get calibration choices
C
        CALL VDBINI(RUN, MAX_VTXCRT, PD_METH, TM_METH, GN_METH, OK)
        IF ( .NOT. OK ) THEN
          IF (BYPASS_DBL3_ERROR) THEN
            CALL ERRMSG('VTRAKS','VTRPAR',
     &           'Error in VDBINI, use default values','W')
            IF ( PD_METH .GT. 0 ) CALL V_DFLT_PD(DFLT_PD, DFLT_PD_SIG)
            IF ( TM_METH .GT. 0 ) CALL V_DFLT_TM(DFLT_TM, DFLT_TM_SIG)
            IF ( GN_METH .GT. 0 ) CALL V_DFLT_GN(DFLT_GN, DFLT_GN_SIG)
          ELSE
            CALL ERRMSG('VTRAKS','VTRPAR',
     &           'Error in VDBINI, crash the job','F')
          END IF
         ELSE
          VTRPAR = .TRUE.
        END IF
      ELSE
C
C ****  MC data: VTX_STPFILE already read in
C ****  or else calibration is in VCAL bank
C
        VTRPAR = .TRUE.
      END IF
  999 RETURN
      END
