      FUNCTION PMUTRAKS_EXEC ()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : EXEC Interface Routine for PIXIE package
C-                         MUTRAKSDIS
C-
C-   Called from the PIXIE hook PXEXEC.
C-
C-   Returned value  : TRUE
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Created  17-JAN-1992   PXBUILD V1.00
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL PMUTRAKS_EXEC
C----------------------------------------------------------------------
      CHARACTER*(*) TITLE
      PARAMETER( TITLE   = 'MUTRAKS display' )
      CHARACTER*(*) MENNAM
      PARAMETER( MENNAM  = 'MUTRAKSDIS' )
      CHARACTER*40 COMMAND
C----------------------------------------------------------------------
      PMUTRAKS_EXEC = .TRUE.
C
C ****  Process commands
C
      COMMAND = ' '
      DO WHILE ( COMMAND .NE. 'EXIT' )
        CALL PUMENUDO(TITLE,MENNAM,COMMAND)
        IF ( COMMAND .EQ. 'MUON_DUMP$' ) THEN
          CALL PMDUMP
        ENDIF
      ENDDO
  999 RETURN
      END
