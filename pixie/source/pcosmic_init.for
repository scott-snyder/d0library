      FUNCTION PCOSMIC_INIT
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Read into memory the RCP file PX_COSMIC_RAYS_RCP.
C-
C-   Returned value  : TRUE
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Created  18-MAR-1991   Lupe Howell   
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL PCOSMIC_INIT
C
      INTEGER IER
      CHARACTER*(*) RCPFILE
      PARAMETER( RCPFILE = 'PX_COSMIC_RAYS_RCP' )   ! Logical name 
                                                    ! of control file
C
      LOGICAL OK,FIRST
      DATA FIRST/.TRUE./
      SAVE OK,FIRST
C----------------------------------------------------------------------
      IF ( FIRST ) THEN
        FIRST = .FALSE.
C
        CALL INRCP (RCPFILE,IER)! Read parameter file into an SRCP bank
        IF ( IER .NE. 0 ) THEN
          CALL ERRMSG
     &    ('PIXIE','PCOSMIC_INIT',
     &     'Problem reading PX_COSMIC_RAYS_RCP','F')
        ENDIF
C
C ****  Build up the menus
C
        CALL EZ_SETUP_COMPACK(RCPFILE,IER)
        IF ( IER .NE. 0 ) THEN
          CALL ERRMSG
     &  ('PIXIE','PCOSMIC_INIT',
     &   'Problem accessing PX_COSMIC_RAYS_RCP','F')
        ENDIF
        OK = IER .EQ. 0
      ENDIF
      PCOSMIC_INIT = OK
  999 RETURN
      END
