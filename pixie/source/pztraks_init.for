      FUNCTION PZTRAKS_INIT
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Read into memory the RCP file
C-   PX_ZTRAKS_RCP for the package ZTRAKS, and setup the ZTRAKS menu.
C-
C-   Returned value  : TRUE if OK
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Created  10-NOV-1990   Harrison B. Prosper, Lupe Howell
C-   Updated  25-APR-1991   Lupe Howell  Name of RCPFILE was updated 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL PZTRAKS_INIT
C
      INTEGER IER
      CHARACTER*(*) RCPFILE
      PARAMETER( RCPFILE = 'PX_ZTRAKSDIS_RCP' )
C
      LOGICAL OK,FIRST
      DATA FIRST/.TRUE./
      SAVE OK,FIRST
C----------------------------------------------------------------------
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        CALL PU_INIT_COMBINED_DISPLAYS(RCPFILE,IER)
        OK = IER .EQ. 0
      ENDIF
      PZTRAKS_INIT = OK
  999 RETURN
      END
