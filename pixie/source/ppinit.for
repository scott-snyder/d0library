      FUNCTION PPINIT ()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Read RCP file PX_PHYDIS_RCP. The routines
C-   PPINIT and PPEXEC together form a package.
C-
C-   Returned value  : TRUE if successful
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Created  18-SEP-1991   Nobuaki Oshima
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      LOGICAL PPINIT
C
      INTEGER IER
      CHARACTER*(*) RCPFILE
      PARAMETER( RCPFILE = 'PX_PHYDIS_RCP' )   ! Logical name of control file
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
     &      ('PIXIE','PPINIT','Problem accessing PX_PHYDIS_RCP','F')
        ENDIF
        OK = IER .EQ. 0
      ENDIF
C
      PPINIT = OK
  999 RETURN
      END
