      SUBROUTINE PU_INIT_COMBINED_DISPLAYS(RCPFILE,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Read into memory the specified RCP file
C-   and setup the COMPACK menu defined in the file. The RCP file should
C-   contain not only the definition of the COMPACK menu but also the
C-   definitions of the combined view and/or multi-port displays, with
C-   one such definition per menu item. See the file
C-   D0$PIXIE:PX_ALLDIS.RCP for an example of how to define 
C-   multi-view displays.
C-
C-   Inputs  : RCPFILE  [C*]    Logical name of RCP file.
C-   Outputs : IER      [I]     0 -- OK (file found).
C-   Controls: None
C-
C-   Created   6-NOV-1990   Lupe Howell , Harrison B. Prosper
C-   Updated  24-JAN-1992   Lupe Howell  Update for SGI 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) RCPFILE
      INTEGER IER
      CHARACTER*80 MESS
C----------------------------------------------------------------------
C
C ****  Read RCP file into an RCP bank
C
      CALL INRCP (RCPFILE,IER)
C
C ****  Initialize Menus
C
      IF( IER .EQ. 0 ) THEN
        CALL EZ_SETUP_COMPACK(RCPFILE,IER)
        IF ( IER .NE. 0 ) THEN
          CALL ERRMSG
     &      ('PU_INIT_COMBINED_DISPLAYS','EZ_SETUP_COMPACK',
     &       'Problem defining menu','F')
        ENDIF
      ELSE
        MESS = ' Problem reading '//RCPFILE
        CALL ERRMSG
     &    ('PU_INIT_COMBINED_DISPLAYS','INRCP',
     &     MESS,'F')
      ENDIF
  999 RETURN
      END
