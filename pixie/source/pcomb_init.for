      FUNCTION PCOMB_INIT
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Read into memory the RCP file PX_COMBDIS_RCP.
C-
C-   Returned value  : TRUE
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Created   6-NOV-1990   LUPE HOWELL , Harrison B. Prosper
C-   Updated   5-DEC-1990   Harrison B. Prosper  
C-      Remove call to ALL_DISPLAY_INI 
C-   Updated   7-DEC-1990   Harrison B. Prosper  
C-      Rename PAINIT to PCOMB_INIT 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL PCOMB_INIT
C
      INTEGER IER
      CHARACTER*(*) RCPFILE
      PARAMETER( RCPFILE = 'PX_COMBDIS_RCP' )   ! Logical name of control file
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
     &    ('PIXIE','PCOMB_INIT','Problem reading PX_COMBDIS_RCP','F')
        ENDIF
C
C ****  Build up the menus
C
        CALL EZ_SETUP_COMPACK(RCPFILE,IER)
        IF ( IER .NE. 0 ) THEN
          CALL ERRMSG
     &  ('PIXIE','PCOMB_INIT','Problem accessing PX_COMBDIS_RCP','F')
        ENDIF
        OK = IER .EQ. 0
      ENDIF
      PCOMB_INIT = OK
  999 RETURN
      END
