      FUNCTION PCAJETS ()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : EXEC Interface Routine for PIXIE package
C-                         CALDIS (called as a sub-menu of CALDIS).
C-
C-   Returned value  : TRUE
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Created  20-SEP-1990   PXBUILD V1.00
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL PCAJETS
C----------------------------------------------------------------------
      CHARACTER*(*) TITLE
      PARAMETER( TITLE   = 'Calorimeter Jets Event Display' )
      CHARACTER*(*) MENNAM
      PARAMETER( MENNAM  = 'CAJETS' )
      CHARACTER*40 COMMAND
C----------------------------------------------------------------------
      PCAJETS = .TRUE.
C
C ****  Set parameters for CAJETS
C
      CALL PCAJETS_SET
C
      COMMAND = ' '
      DO WHILE ( COMMAND .NE. 'EXIT' )
        CALL PUMENUDO(TITLE,MENNAM,COMMAND)
        IF     ( COMMAND .EQ. 'TOWER LEGO CATE' ) THEN
          CALL PCATEL
        ELSEIF ( COMMAND .EQ. 'JETS_LEGO PLOT' )  THEN
          CALL PCJETL
        ELSEIF ( COMMAND .EQ. 'ONE JET LEGO' )    THEN
          CALL PCJET1
        ELSEIF ( COMMAND .EQ. 'JETS_3D CELL' )    THEN
          CALL PC3DJT
        ELSEIF ( COMMAND .EQ. 'ISAJET TRACKS')    THEN
          CALL PCISATRACK                             
        ENDIF
      ENDDO
C
C ****  Reset parameters for CAJETS
C
      CALL PCAJETS_RESET
  999 RETURN
      END
