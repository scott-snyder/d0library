      SUBROUTINE VDBINI(CRUN,MAX_VTXCRT,PD_METH,TM_METH,GN_METH,OK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Initialization routine to get pedestals, times and
C-                         gains from the database at the beginning of a run.
C-
C-   Inputs  : CRUN = current run number
C-             MAX_VTXCRT = number of fadc crates to be read (count from 0)
C-             PD_INI,TM_INI,GN_INI = logicals to control what is retrieved
C-             from the database
C-   Outputs : none
C-   Controls: returns OK = .TRUE. if all goes well
C-
C-   Created   7-FEB-1991   Peter Grudberg
C-   Updated   8-APR-1991   Peter Grudberg  Get DB file from RCP
C-   Updated  13-AUG-1991   Susan K. Blessing   Add logical DB_OPENED to
C-    flag if DBCLB_INITIALIZE has been called for this run.
C-   Updated  30-OCT-1991   Peter M. Grudberg  Remove calls to BLVXXX routines
C-   Updated  19-OCT-1992   Peter M. Grudberg  change arguments, structure
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
C
      INTEGER CRUN
      INTEGER MAX_VTXCRT
      INTEGER PD_METH, TM_METH, GN_METH
C
      LOGICAL OK, FIRST
      LOGICAL DB_OPENED
C
      SAVE DB_OPENED, FIRST
C
      DATA FIRST / .TRUE. /
      DATA DB_OPENED/.FALSE./
C----------------------------------------------------------------------
C
      OK = .TRUE.
C
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        IF ( LSVTX .LE. 0 ) THEN
          CALL ERRMSG('VTRAKS','VDBINI',
     &               'VTX constants header bank does not exist','W')
          OK = .FALSE.
        ENDIF
        IF ( LVPDH .LE. 0 ) THEN
          CALL ERRMSG('VTRAKS','VDBINI',
     &               'Pedestal header bank does not exist','W')
          OK = .FALSE.
        ENDIF
        IF ( LVTMH .LE. 0 ) THEN
          CALL ERRMSG('VTRAKS','VDBINI',
     &               'Times header bank does not exist','W')
          OK = .FALSE.
        ENDIF
        IF ( LVGNH .LE. 0 ) THEN
          CALL ERRMSG('VTRAKS','VDBINI',
     &               'Gains header bank does not exist','W')
          OK = .FALSE.
        ENDIF
        IF ( .NOT. OK ) GO TO 999
      ENDIF
C
      IF ( PD_METH .GT. 0 .AND. OK ) THEN
        IF ( PD_METH .EQ. 1 )
     &    CALL VPDINI(CRUN, MAX_VTXCRT, DB_OPENED, OK)
        IF ( PD_METH .EQ. 2 )
     &    CALL VSTP_FETCH(CRUN, 'PEDESTAL', DB_OPENED, OK)
        IF ( PD_METH .EQ. 3 )
     &    CALL VPDH_READ(OK)
        IF ( PD_METH .GT. 3 ) 
     &    CALL ERRMSG('Invalid Method','VDBINI',
     &    'Requested pedestal calibration method is invalid','W')
      ENDIF
C
      IF ( TM_METH .GT. 0 .AND. OK ) THEN
        IF ( TM_METH .EQ. 1 )
     &    CALL VTMINI(CRUN, MAX_VTXCRT, DB_OPENED, OK)
        IF ( TM_METH .EQ. 2 )
     &    CALL VSTP_FETCH(CRUN, 'TIMES', DB_OPENED, OK)
        IF ( TM_METH .EQ. 3 )
     &    CALL VTMH_READ(OK)
        IF ( TM_METH .GT. 3 ) 
     &    CALL ERRMSG('Invalid Method','VDBINI',
     &    'Requested time calibration method is invalid','W')
      ENDIF
C
      IF ( GN_METH .GT. 0 .AND. OK ) THEN
        IF ( GN_METH .EQ. 1 )
     &    CALL VGNINI(CRUN, MAX_VTXCRT, DB_OPENED, OK)
        IF ( GN_METH .EQ. 2 )
     &    CALL VSTP_FETCH(CRUN, 'GAINS', DB_OPENED, OK)
        IF ( GN_METH .EQ. 3 )
     &    CALL VGNH_READ(OK)
        IF ( GN_METH .GT. 3 ) 
     &    CALL ERRMSG('Invalid Method','VDBINI',
     &    'Requested gain calibration method is invalid','W')
      ENDIF
C
C
C  **** Close DBL3 database if necessary and do a garbage collection
C
      IF (DB_OPENED) THEN
        CALL DBCLB_FINISH
        DB_OPENED = .FALSE.
      END IF
      CALL MZGARB(IDVSTP,0)
C
C----------------------------------------------------------------------
  999 RETURN
      END
