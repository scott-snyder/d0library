      FUNCTION PMSCINT ()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :Exec Interface routine for PIXIE package
C-                        PX_MUODIS (called as a submenu of PX_MUODIS)
C-   Returned value  : TRUE
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Created   1-NOV-1993   V. Bhatnagar
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL PMSCINT
C----------------------------------------------------------------------
      CHARACTER*(*) TITLE
      PARAMETER( TITLE  = 'Muon Scintillator Display' )
      CHARACTER*(*) MENNAM
      PARAMETER( MENNAM = 'SCINT_DISPLAY' )
      CHARACTER*40 COMMAND
C---------------------------------------------------------------------
      PMSCINT = .TRUE.
C
      COMMAND = ' '
      DO WHILE ( COMMAND .NE. 'EXIT')
        CALL PUMENUDO(TITLE,MENNAM,COMMAND)
        IF     (COMMAND .EQ. 'X_Y VIEW' ) THEN
          CALL PMSCINT_V2
        ELSEIF (COMMAND .EQ. 'Y_Z VIEW' ) THEN
          CALL PMSCINT_V1
        ELSEIF (COMMAND .EQ. 'Z_X VIEW' ) THEN
          CALL PMSCINT_V3
        ENDIF
      ENDDO
C
  999 RETURN
      END
