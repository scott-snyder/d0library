      FUNCTION PLEXEC ()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : EXEC Interface Routine for PIXIE package
C-              USER_LEGO 
C-
C-   Created  21-NOV-1990   Lupe Howell
C-   Updated  28-NOV-1990   Harrison B. Prosper  
C-      Changed name from PCUSERLEGO to PLEXEC 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL PLEXEC
C----------------------------------------------------------------------
      CHARACTER*(*) TITLE
      PARAMETER( TITLE   = 'User Lego Plot Display' )
      CHARACTER*(*) MENNAM
      PARAMETER( MENNAM  = 'USER_LEGO' )
      CHARACTER*40 COMMAND
C----------------------------------------------------------------------
      PLEXEC = .TRUE.
C
C ****  Process commands
C
      COMMAND = ' '
      DO WHILE ( COMMAND .NE. 'EXIT' )
        CALL PUMENUDO(TITLE,MENNAM,COMMAND)
        IF     ( COMMAND .EQ. 'USER LEGO PLOT' )  THEN
          CALL PLEGOZ
        ELSEIF ( COMMAND .EQ. 'SUM USER LEGO PLOT' ) THEN
          CALL PLEGOS
        ENDIF
      ENDDO
  999 RETURN
      END
