      FUNCTION SCAN_INI()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :  Scan Initialization 
C-
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  05-AUG-1992  Andrew Brandt
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL SCAN_INI
      LOGICAL FIRST,OK
      SAVE FIRST,OK
      DATA FIRST/.TRUE./
      INTEGER IER
      CHARACTER*(*) RCPFILE
      PARAMETER( RCPFILE = 'PX_SCANNING_RCP' )
C----------------------------------------------------------------------
      SCAN_INI = .TRUE.
      IF ( FIRST ) THEN
        FIRST=.FALSE.
C
C ****  Read in RCP file for this package
C
        CALL INRCP(RCPFILE,IER)       ! read in RCP file
        OK = IER.EQ.0
        SCAN_INI = OK
        IF( .NOT.OK) GOTO 999              ! failed
C
C ****  Build COMPACK menus
C
        CALL EZ_SETUP_COMPACK(RCPFILE,IER)
        IF ( IER .NE. 0 ) THEN
          CALL ERRMSG('SCANNING','SCAN_INI',
     &                  'Problem accessing PX_SCANNING_RCP','F')
        ENDIF
        OK = IER .EQ. 0
        SCAN_INI = OK
        IF( .NOT.OK) GOTO 999              ! failed
C
C ****  Read overwrite file (RCPE)
C
        CALL INRCPE('PX_SCANNING_RCPE',IER)  
        IF(IER.EQ.0)
     &    CALL ERRMSG('SCANNING','SCAN_INI',
     &    ' Default PX_SCANNING_RCP modified','W')
        ENDIF
C
  999 RETURN
      END
