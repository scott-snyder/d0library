      FUNCTION P0EXEC()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : EXEC Interface Routine for PIXIE package
C-                         LV0DIS
C-
C-   Called from the PIXIE hook PXEXEC.
C-
C-   Returned value  : TRUE
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Created  29-JUN-1992   Jeffrey Bantly   
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL P0EXEC
C----------------------------------------------------------------------
      CHARACTER*(*) TITLE
      PARAMETER( TITLE   = 'LV0 Event Display' )
      CHARACTER*(*) MENNAM
      PARAMETER( MENNAM  = 'LV0DIS' )
      CHARACTER*40 COMMAND
C----------------------------------------------------------------------
      P0EXEC = .TRUE.
C
C ****  Process commands
C
      COMMAND = ' '
      DO WHILE ( COMMAND .NE. 'EXIT' )
        CALL PUMENUDO(TITLE,MENNAM,COMMAND)
        IF     ( COMMAND .EQ. 'LV0 PADS HIT N') THEN
          CALL P0PADS(1)
        ELSEIF ( COMMAND .EQ. 'LV0 PADS HIT S') THEN
          CALL P0PADS(2)
        ELSEIF ( COMMAND .EQ. 'LV0 R-Z') THEN
          CALL P0RZ
        ELSEIF ( COMMAND .EQ. 'LV0_X-Z_VIEW') THEN
          CALL P0XZ
        ELSEIF ( COMMAND .EQ. 'LV0_Y-Z_VIEW') THEN
          CALL P0YZ
        ENDIF
      ENDDO
C----------------------------------------------------------------------
  999 RETURN
      END
