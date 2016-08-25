      SUBROUTINE DDBINI(CRUN,MAX_CDCRT,PD_INI,TM_INI,GN_INI,IOK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Initialzation routine for Pedestals, T0s and
C-                         gains.
C-                         To be called at the beginning of every run.
C-   Inputs  : CRUN = Current Run Number (I6 format)
C-             MAX_CDCRT = number of crates being read in (count from 0)
C-             PD_INI: flag. True: pedestals will be read from DBL3
C-             TM_INI: flag. True: T0s will be read from DBL3
C-             GN_INI: flag. True: Gains will be read from DBL3
C-   Outputs : none
C-   Controls: IOK = .TRUE. if everything goes well.
C-
C-   Created  14-DEC-1989   Srini Rajagopalan
C-   Updated  20-JUN-1990   Srini Rajagopalan, Accomodate Gain's and T0's
C-   Updated  13-DEC-1990   Qizhong Li-Demarteau  adapted for CDC
C-   Updated   3-APR-1991   Qizhong Li-Demarteau  read DBL3 file name from
C-                                                RCP file
C-   Updated  29-MAY-1991   Qizhong Li-Demarteau  added EZERROR
C-   Updated  13-AUG-1991   Susan K. Blessing   Add logical DB_OPENED to
C-    flag if DBCLB_INITIALIZE has been called for this run.
C-   Updated   1-NOV-1991   Qizhong Li-Demarteau  remove calls to BLDxxH
C-                       but added check on the existance of these banks
C-   Updated   5-NOV-1992   Qizhong Li-Demarteau  added option for reading
C-                       offline calibrated values from DBL3 database
C-   Updated   3-FEB-1995   Norman A. Graf   Added FIRST_RUN for 
C-                          TM_PROC_INI to avoid problems if offline
C-                          T0 and velocities are called for runs prior
C-                          to database population.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBSTP.INC'
C
      INTEGER CRUN, IER
      INTEGER MAX_CDCRT, GZSCDC, GZDPDH, GZDTMH, GZDGNH
      INTEGER RUN,RUNNO
C
      LOGICAL IOK, FIRST, EZERROR
      LOGICAL GN_INI, PD_INI, TM_INI
      LOGICAL GN_PROC_INI, PD_PROC_INI, TM_PROC_INI, DBG_OFF
      LOGICAL TM_PROC_INI_FIRST_RUN
      LOGICAL DB_OPENED
      integer i_tm_proc_ini_first_run
C
      SAVE FIRST,DB_OPENED
C
      DATA FIRST /.TRUE./
      DATA DB_OPENED/.FALSE./
C----------------------------------------------------------------------
C
      IOK = .TRUE.
C
C  *** Check top level Logical STP tree structure.
C
      IF (FIRST) THEN
        IF (LSCDC.LE.0) LSCDC = GZSCDC()
        IF (LSCDC.LE.0) THEN
          CALL ERRMSG('DTRAKS','DDBINI','SCDC bank not found','W')
          IOK = .FALSE.
          GOTO 999
        ENDIF
        IF (LDPDH.LE.0) LDPDH = GZDPDH()
        IF (LDPDH.LE.0) THEN
          CALL ERRMSG('DTRAKS','DDBINI','DPDH bank not found','W')
          IOK = .FALSE.
          GOTO 999
        ENDIF
        IF (LDGNH.LE.0) LDGNH = GZDGNH()
        IF (LDGNH.LE.0) THEN
          CALL ERRMSG('DTRAKS','DDBINI','DGNH bank not found','W')
          IOK = .FALSE.
          GOTO 999
        ENDIF
        IF (LDTMH.LE.0) LDTMH = GZDTMH()
        IF (LDTMH.LE.0) THEN
          CALL ERRMSG('DTRAKS','DDBINI','DTMH bank not found','W')
          IOK = .FALSE.
          GOTO 999
        ENDIF
        CALL EZPICK('DTRAKS_RCP')
        IF ( EZERROR(IER) ) THEN
          CALL ERRMSG('DTRAKS','DDBINI',
     &    'Unable to find bank DTRAKS_RCP','W')
          GOTO 999
        ENDIF
        CALL EZGET('PD_PROC_INI',PD_PROC_INI,IER)
        CALL EZGET('TM_PROC_INI',TM_PROC_INI,IER)
        CALL EZGET('TM_PROC_INI_FIRST_RUN',TM_PROC_INI_FIRST_RUN,IER)
        CALL EZGET('GN_PROC_INI',GN_PROC_INI,IER)
        CALL EZGET('DBG_OFF',DBG_OFF,IER)
        CALL EZRSET
        FIRST = .FALSE.
      ENDIF
C
        IF (DBG_OFF) CALL DBCLB_INITIALIZE_FORCE_RUN(0)
        IF (PD_INI .AND. IOK) THEN
          IF (PD_PROC_INI) THEN
            CALL DSTP_FETCH(CRUN,'PEDESTAL',DB_OPENED,IOK)
          ELSE
            CALL DPDINI(CRUN,MAX_CDCRT,DB_OPENED,IOK)
          ENDIF
        ENDIF
        IF (TM_INI .AND. IOK) THEN
          RUN = RUNNO()
          i_tm_proc_ini_first_run = tm_proc_ini_first_run
          IF (TM_PROC_INI.AND.RUN.LT.i_TM_PROC_INI_FIRST_RUN) THEN
            CALL DSTP_FETCH(CRUN,'TIMES',DB_OPENED,IOK)
          ELSE
            CALL DTMINI(CRUN,MAX_CDCRT,DB_OPENED,IOK)
          ENDIF
        ENDIF
        IF (GN_INI .AND. IOK) THEN
          IF (GN_PROC_INI) THEN
            CALL DSTP_FETCH(CRUN,'GAINS',DB_OPENED,IOK)
          ELSE
            CALL DGNINI(CRUN,MAX_CDCRT,DB_OPENED,IOK)
          ENDIF
        ENDIF
C
C  *** Close DBL3 database if necessary.
C
        IF (DB_OPENED) THEN
          CALL DBCLB_FINISH
          DB_OPENED = .FALSE.
        END IF
C
  999 RETURN
      END
