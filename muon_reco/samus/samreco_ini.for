      LOGICAL FUNCTION SAMRECO_INI()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Initialize SAMUS reconstruction package,
C-                         'SAMRECO'.
C-
C-   Returned value  :
C-   Inputs  : (none)
C-   Outputs : (none)
C-   Controls:
C-
C-   Created   10-MAY-1991   O.Eroshin
C-   Updated  13-JUN-1991   Daria Zieminska:  get the STP file from D0$STP
C-   Updated   9-SEP-1991   Daria Zieminska  protect against repeated calls
C-   Updated  23-DEC-1991   Daria Zieminska  removed reading STP file
C-   Updated  15-JAN-1993   Dmitri Denisov   New RCP file name
C-   Updated   8-MAY-1994   Andrei A. Mayorov  read  MURECO_RCP
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) RCPFILE
      PARAMETER ( RCPFILE = 'SAMRECO_RCP' )
C
      INTEGER IERR
      LOGICAL SAMRECO_DDF,FIRST
      SAVE FIRST
      DATA FIRST/.TRUE./

C
      IF (FIRST.EQ..FALSE.) GO TO 999
      SAMRECO_INI = .FALSE.
      CALL INZCOM(2)            !Initialize Zebra data banks
      CALL INZSTP               !Initialize Zebra STP banks
C
C
      CALL SAMUS_BOOK_FLAGS     !Book logical flags for application
C
C  read RCP file.
C ================
C
      CALL INRCP ('MURECO_RCP',IERR)
      IF(IERR.NE.0) THEN
        CALL ERRMSG('Error return from S/R INRCP(MURECO_RCP)',
     &    'SAMRECO_INI',
     +               'stop processing.','F')
        SAMRECO_INI = .FALSE.
        GO TO 999
      ENDIF
      CALL INRCP (RCPFILE,IERR)
      IF(IERR.NE.0) THEN
        CALL ERRMSG('Error return from S/R INRCP(SAMRECO_RCP)',
     &    'SAMRECO_INI',
     +               'stop processing.','F')
        SAMRECO_INI = .FALSE.
        GO TO 999
      ENDIF
C
C  set RCP bank to 'SAMUS_RCP'.
C=============================
C
      CALL EZPICK('SAMRECO_RCP')
C
C  Reset RCP bank.
C  ===============
C
      CALL EZRSET
C
C  User initialization.
C  ====================
C
      CALL SAMUSER_INIT
C
C  Every thing is OK.  Reset the flag.
C =====================================
C
      SAMRECO_INI = SAMRECO_DDF()
C
      FIRST=.FALSE.
  999 RETURN
      END
