      FUNCTION GMD_INIT ()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : INIT Interface Routine for PIXIE package
C-                         GM_4VECT
C-
C-   Read RCP file PX_GM_DISPLAY.RCP into memory.
C-
C-   Returned value  : TRUE
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Created  11-DEC-1991   PXBUILD V1.00
C-   Updated  29-JAN-1994   Harrison B. Prosper
C-    Change RCP bank name
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL GMD_INIT
C----------------------------------------------------------------------
      INTEGER STATUS
      CHARACTER*(*) RCPFILE
      PARAMETER( RCPFILE = 'PX_GM_DISPLAY_RCP' )
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:GM_4VECT.INC'
C----------------------------------------------------------------------
      LOGICAL OK, FIRST
      DATA FIRST/.TRUE./
      SAVE OK, FIRST
C----------------------------------------------------------------------
      IF( FIRST ) THEN
        FIRST = .FALSE.
        CALL INRCP (RCPFILE,STATUS)
        OK = STATUS .EQ. 0
        IF ( .NOT. OK ) THEN
          CALL ERRMSG
     &      ('BAD_RCP_OPEN',
     &       'INRCP',
     &       'Unable to open PX_GM_DISPLAY_RCP','F')
        ENDIF
C
C ****  Initialize menu
C
        CALL EZ_SETUP_COMPACK(RCPFILE,STATUS)
        IF ( STATUS .NE. 0 ) THEN
          CALL ERRMSG
     &      ('BAD_SETUP_COMPACK',
     &       'EZ_SETUP_COMPACK',
     &       'No PX_GM_DISPLAY_RCP bank','F')
        ENDIF
      ENDIF
      GMD_INIT = OK
      RETURN
      END
