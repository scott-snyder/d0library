       LOGICAL FUNCTION PZEEXEC()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : EXEC Interface Routine for PIXIE package
C-                         CD_ELECTRONICS.
C-   
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Created 9-AUG-1991   Lupe Howell   
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      CHARACTER*(*) TITLE
      PARAMETER( TITLE   = 'CD_ELECTRONICS System Display' )
      CHARACTER*(*) MENNAM
      PARAMETER( MENNAM  = 'CD_ELECTRONICSDIS' )
      CHARACTER*40 COMMAND
C----------------------------------------------------------------------
      PZEEXEC = .TRUE.
C
C ****  Process commands
C
      COMMAND = ' '
      DO WHILE ( COMMAND .NE. 'EXIT' )
        CALL PUMENUDO(TITLE,MENNAM,COMMAND)
        IF     ( COMMAND .EQ. 'CD VCRATE FADCS' ) THEN
          CALL PZVCRT
        ELSEIF ( COMMAND .EQ. 'CD FOURIER TRANS' ) THEN
        CALL PZFOUR
        ELSEIF ( COMMAND .EQ. 'CD CRATE FADCS' ) THEN
        CALL PZCRAT
        ENDIF
      ENDDO
  999 RETURN
      END
