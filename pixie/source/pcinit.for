      FUNCTION PCINIT ()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Read RCP file PXCALDIS_RCP. The routines
C-   PCINIT and PCEXEC together form a package.
C-
C-   Returned value  : TRUE if successful
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Created  XX-XXX-1990   Lupe Howell
C-   Updated   6-SEP-1990   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      LOGICAL PCINIT
C
      INTEGER IER
      CHARACTER*(*) RCPFILE
      PARAMETER( RCPFILE = 'PX_CALDIS_RCP' )   ! Logical name of control file
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
     &      ('PIXIE','PCINIT','Problem accessing PX_CALDIS_RCP','F')
        ENDIF
        OK = IER .EQ. 0
      ENDIF
C
C ****  Read geometry data
C
      CALL CAISTP ('CAL_STPFILE',IER)
      OK = OK .AND. (IER.EQ.0)
C
C ****  Create Calorimeter link area
C
      CALL CZLINI
C
      PCINIT = OK
  999 RETURN
      END
