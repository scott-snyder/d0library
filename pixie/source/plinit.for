      FUNCTION PLINIT ()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Read RCP file PX_USER_LEGO_RCP. The routines
C-   PLINIT and PLEXEC together form a package.
C-
C-   Returned value  : TRUE if successful
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Created  26-NOV-1990   Lupe Howell
C-   Updated  28-NOV-1990   Harrison B. Prosper   
C-      Remove call to CAISTP (not needed)
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL PLINIT
C
      INTEGER IER
      CHARACTER*(*) RCPFILE
      PARAMETER( RCPFILE = 'PX_USER_LEGO_RCP' )   ! Logical name of control file
C
      LOGICAL OK,FIRST
      DATA FIRST/.TRUE./
      SAVE OK,FIRST
C----------------------------------------------------------------------
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        CALL INRCP (RCPFILE,IER)! Read parameter file into an SRCP bank
C
C ****  Builds up the menus
C
        CALL EZ_SETUP_COMPACK(RCPFILE,IER)
        IF ( IER .NE. 0 ) THEN
          CALL ERRMSG
     &      ('PIXIE','PLINIT','Problem accessing PX_USER_LEGO_RCP','F')
        ENDIF
        OK = IER .EQ. 0
      ENDIF
C
      PLINIT = OK
  999 RETURN
      END
