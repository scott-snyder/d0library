      FUNCTION PCATRIG ()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : EXEC Interface Routine for PIXIE package
C-              CALDIS (called as a sub-menu of CALDIS).
C-   
C-
C-   Returned value  : TRUE
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Created  8-APR-1991   Nobuaki Oshima( PXBUILD V1.00 )
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL PCATRIG
C----------------------------------------------------------------------
      LOGICAL EZERROR
      INTEGER IER
      CHARACTER*(*) TITLE
      PARAMETER( TITLE   = 'Calorimeter Trigger Display' )
      CHARACTER*(*) MENNAM
      PARAMETER( MENNAM  = 'CATRIG' )
      CHARACTER*40 COMMAND
C----------------------------------------------------------------------
      PCATRIG = .TRUE.
C
C ****  Process commands
C
      COMMAND = ' '
      DO WHILE ( COMMAND .NE. 'EXIT' )
        CALL PUMENUDO(TITLE,MENNAM,COMMAND)
        IF     ( COMMAND .EQ. 'TRIG END VIEW'  ) THEN
          CALL PCTREV
        ELSEIF ( COMMAND .EQ. 'TRIG SIDE VIEW' ) THEN
          CALL PCTRSV
        ELSEIF ( COMMAND .EQ. 'TRIG LEGO PLOT' ) THEN
          CALL PCTRLG
        ENDIF
      ENDDO
C
  999 RETURN
      END
