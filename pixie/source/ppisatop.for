      FUNCTION PPISATOP
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : EXEC Interface Routine for PIXIE sub-package
C-                         ISATOP (called as a sub-menu of PHYDIS).
C-
C-   Returned value  : TRUE
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Created   5-MAY-1993   Nobuaki Oshima
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL PPISATOP
C----------------------------------------------------------------------
      CHARACTER*(*) TITLE
      PARAMETER( TITLE   = 'Isajet Top displays' )
      CHARACTER*(*) MENNAM
      PARAMETER( MENNAM  = 'ISATOP' )
      CHARACTER*40 COMMAND
C----------------------------------------------------------------------
      PPISATOP = .TRUE.
C
C ****  Process commands
C
      COMMAND = ' '
      DO WHILE ( COMMAND .NE. 'EXIT' )
        CALL PUMENUDO(TITLE,MENNAM,COMMAND)
        IF ( COMMAND .EQ. 'LEGO ISP1'        ) THEN
          CALL PPTOP_LEGO(0)
        ELSE IF ( COMMAND .EQ. 'LEGO ISAQ'   ) THEN
          CALL PPTOP_LEGO(1)
        ELSE IF ( COMMAND .EQ. 'TOP TRACKS'  ) THEN
          CALL PPTOP_ISP1
        ELSE IF ( COMMAND .EQ. 'TOP PARTONS' ) THEN
          CALL PPTOP_ISAQ
        ELSE IF ( COMMAND .EQ. 'ISAJET TRACKS' ) THEN
          CALL PCISATRACK
        ELSE IF ( COMMAND .EQ. 'PJET LEGO'   ) THEN
          CALL PPJET_LEGO
        ELSE IF ( COMMAND .EQ. 'DST LEGO'    ) THEN
          CALL PCDST_LEGO
        ENDIF
      ENDDO
  999 RETURN
      END
