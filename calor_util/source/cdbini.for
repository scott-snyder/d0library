      SUBROUTINE CDBINI(CRUN,NCRT,CRATES,CALTYPE,LOK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Initialzation routine for Pedestals.
C-                         To be called at the beginning of every run.
C-   Inputs  : CRUN = Current Run Number
C-             NCRT = Number of ADC crates being read
C-             CRATES = Array containing the ADC crates to be read
C-                      If NCRT=12, then assumes all 12 D0 ADC crates
C-             CALTYPE = Calibration type code
C-                       0 = both peds and gains
C-                       1 = pedestal only
C-                       2 = gains only
C-   Controls: LOK = .TRUE. if everything goes well.
C-
C-   Created  14-DEC-1989   Srini Rajagopalan (FDC version)
C-   Modified 14-MAR-1990   Jan Guida  (Cal version)
C-   Modified 20-JUN-1990   Jan Guida, Srini Rajagopalan
C-                             Calls CPDINI and CGNINI
C-   Updated  13-NOV-1990   Jan Guida  Add CRATES variable
C-   Updated  24-FEB-1991   Jan Guida  Remove DB from data base name
C-                                      now dbcalib$cal is logical
C-   Updated  22-APR-1991   Jan Guida  Open database with default rather than S
C-   Updated   3-MAR-1992   Jan Guida  Add calib version number  (LSCAL+9)
C-   Updated  28-APR-1992   Jan Guida  Add CAD crate version number (LSCAL+10)
C-   Updated  28-AUG-1992   Jan Guida, Chip Stewart  Remove extra EZRSET's,
C-                                      fill SRCPNAME and call EZPICK to pick
C-                                      SRCP bank
C-   Updated  15-APR-1993   Jan Guida  Fix checking of IER after EZGETS
C-   Updated  23-APR-1993   Meenakshi Narain  add fatal crash on dbl3 error
C-   Updated   8-JUL-1993   Jan Guida  If CAHITS.RCP does not exist, book it
C-                                      and fill BYPASS_DBL3_ERROR 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:QUEST.INC'
C
      INTEGER CRUN,CALTYPE,NCRT,CRATES(*)
C
      REAL VSN
      INTEGER IERR,CADVSN,LENF,IER,LOC
      INTEGER CRTID
      INTEGER WRDIDS,NUMIDS,NUMVAL
      LOGICAL LOK,FIRST
      CHARACTER DBCALIB*132
      CHARACTER*32 SRCPNAME
      LOGICAL BYPASS_DBL3_ERROR
C
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
      IF (CALTYPE.LT.0.OR.CALTYPE.GT.3) THEN
        LOK = .FALSE.
        GO TO 999
      ENDIF
      LOK = .TRUE.
      IF (FIRST) THEN
        IF (LSCAL.EQ.0) CALL BKSCAL('STPC',LSCAL)
        CALL EZLOC('CAHITS_RCP',LOC)
        IF(LOC.LE.0) THEN
          WRDIDS = 32         ! number of 32-bit words/record
          NUMVAL = 10         ! initial number of values
          NUMIDS = 10         ! initial number of identifiers
          CALL EZBOOK('CAHITS_RCP',WRDIDS,NUMIDS,NUMVAL)
          CALL EZERR(IER)
          IF (IER.NE.0) CALL ERRMSG
     &      ('NO CAHITS_RCP','CDBINI','Error booking CAHITS.RCP','W')
          CALL EZPICK('CAHITS_RCP')
          CALL EZERR(IER)
          IF (IER.NE.0) CALL ERRMSG
     &      ('NO CAHITS_RCP','CDBINI','Error picking CAHITS.RCP','W')
          BYPASS_DBL3_ERROR = .TRUE.
          CALL EZFILL('BYPASS_DBL3_ERROR','BYPASS DBL3',
     &      BYPASS_DBL3_ERROR,'L',1)
          CALL EZERR(IER)
          IF (IER.NE.0) CALL ERRMSG
     &      ('NO CAHITS_RCP','CDBINI','Error filling CAHITS.RCP','W')
          CALL EZEND
        ENDIF
C
        CALL EZLOC('CAHITS_RCP',LOC)
        IF(LOC.LE.0) THEN
          LOK = .FALSE.
          CALL ERRMSG('NO CAHITS_RCP','CDBINI','Quitting','W')
          GO TO 999
        ENDIF
C
        CALL EZGETS('DBCALIB$CAL',1,DBCALIB,LENF,IER)
        IF(IER.NE.0) THEN
          CALL ERRMSG('NO DBCALIB$CAL IN CAHITS_RCP','CDBINI',
     &      'USE DBCALIB$CAL AS LOGICAL','W')
          DBCALIB = 'DBCALIB$CAL'
        ENDIF
        CALL EZGET('BYPASS_DBL3_ERROR',BYPASS_DBL3_ERROR,IER)
        IF(IER.NE.0) THEN
          CALL ERRMSG('NO_ERR_SWITCH','CDBINI',
     &      'USE DBL3 BYPASS ERROR SWITCH  = FALSE AS DEFAULT','W')
          BYPASS_DBL3_ERROR = .FALSE.
        ENDIF
        CALL EZRSET
        FIRST = .FALSE.
      ENDIF
C
C  *** initialize DBL3 database
C
      CALL DBCLB_INITIALIZE(DBCALIB,'S',LOK)
      IF (.NOT.LOK) THEN
        IF (BYPASS_DBL3_ERROR) THEN
          CALL ERRDB(' Error in DBL3 intialization ')
        ELSE
          CALL ERRMSG('CALORIMETER','CDBINI',
     &        ' failed in DBL3 initialization','F')
          GOTO 999
        ENDIF
      ENDIF
C
      IF(CALTYPE.NE.2 .AND. LOK) THEN
        CALL CPDINI(CRUN,NCRT,CRATES,LOK)
        CRTID = CRATES(NCRT)
        WRITE(SRCPNAME,5)CRTID
    5   FORMAT('STPC/CALP',I2.2)
      ENDIF
      IF(CALTYPE.NE.1 .AND. LOK) THEN
        CALL CGNINI(CRUN,NCRT,CRATES,LOK)
        CRTID = CRATES(NCRT)
        WRITE(SRCPNAME,6)CRTID
    6   FORMAT('STPC/CALG',I2.2)
      ENDIF
C
      IF (LOK) THEN
        CALL EZPICK(SRCPNAME)
        CALL EZGET('CALIB_VERSION',VSN,IERR)  !FROM SRCP BANK LAST PICKED
        CALL EZGET('CAD_VERSION',CADVSN,IERR)
        C(LSCAL+9)=VSN
        IC(LSCAL+10)=CADVSN
        CALL EZRSET
      ENDIF
C
      CALL DBCLB_FINISH
  999 RETURN
      END
