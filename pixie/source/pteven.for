      SUBROUTINE PTEVEN
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Action subroutines to display the end
C-             view of the TRD and allows you to pick a wire to see
C-             its numbe, layer and energy released.
C-
C-   Created  17-JAN-1989   Lupe Rosas
C-   Updated   6-JUN-1990   Norman A. Graf
C-   Updated  24-SEP-1990   Lupe Rosas Howell Implementing PIXIE_RCP
C-   Updated  16-APR-1991   Lupe Howell  PUHEAD after drawing was taken  
C-           out to leave the display on graphics window.
C-   Updated  17-SEP-1991   JFG Booking and set TRD_ANO_CATH Flag
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INTEGER INIT
      LOGICAL TRONLY,EZERROR
      LOGICAL TRDISPLAY,FLGVAL
      LOGICAL FIRST
C
      CHARACTER*4 CVAL, REM
C
      INTEGER TYP,IER
C
      DATA INIT/0/
      DATA FIRST/.TRUE./
C---------------------------------------------------------------------
C
C ****  Picking PIXIE RCP
C
      CALL EZPICK('PX_TRDDIS_RCP')
      IF ( EZERROR(IER) ) THEN
        CALL ERRMSG('PIXIE','PTEVEN','Cannot find PX_TRDDIS_RCP','W')
        GOTO 999
      ENDIF
C
C ****  Get some TRD constants
C
      CALL EZ_GET_ARRAY('PXPARAMS','TRD ONLY',1,TRONLY,CVAL,
     &       TYP,REM,IER)
      IF ( IER .NE. 0 ) THEN
        CALL ERRMSG('PIXIE','PTEVEN',
     &      'PXPARAMS NOT FOUND','W')
        GOTO 900
      ENDIF
      CALL PUGETV('TRD HITS DISPLAY',TRDISPLAY)
C
      IF ( FIRST ) THEN
      CALL FLGBK('TRD_ANO_CATH',1)  ! Initialize the display for anodes
      CALL FLGSET('TRD_ANO_CATH',.TRUE.)
        FIRST = .FALSE.
      ENDIF
C
C ****  Skip the display of end view if hardcopy flag in set and
C ****  TRD HITS DISPLAY parameter is not.  This is to satisfy the
C ****  case of hardcopy of FADC of a wire, the end view should not
C ****  be redrawn.
C
      IF ( FLGVAL('HARDCOPY') .AND. (.NOT. TRDISPLAY) ) 
     &  GOTO 900

      CALL TRDHIT        ! Fills array TRHITW
      CALL PTRDVW        ! Displays the TRD
C
C ****  Reseting RCP file
C
  900 CALL EZRSET
  999 RETURN
      END

