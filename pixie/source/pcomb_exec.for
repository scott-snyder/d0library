      FUNCTION PCOMB_EXEC
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Execute combined view displays. These displays
C-   are built from elementary actions specified in the RCP file
C-   PX_COMBDIS_RCP.
C-
C-   Returned value  : TRUE
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Created   6-NOV-1990   LUPE HOWELL
C-   Updated   5-DEC-1990   Harrison B. Prosper  
C-      Remove call to PX_COMBINE_VIEWS (now done in SETVIEW) 
C-   Updated   7-DEC-1990   Harrison B. Prosper  
C-      Rename PAEXEC to PCOMB_EXEC 
C-
C----------------------------------------------------------------------
      INTEGER IER
      CHARACTER*(*) TITLE
      PARAMETER( TITLE   = 'Dzero Combined Event Display' )
      CHARACTER*(*) MENNAM
      PARAMETER( MENNAM  = 'COMBDIS' )
      CHARACTER*(*) RCPFILE
      PARAMETER( RCPFILE = 'PX_COMBDIS_RCP' )
      CHARACTER*40 COMMAND
      LOGICAL PCOMB_EXEC
C----------------------------------------------------------------------
      PCOMB_EXEC = .TRUE.
C
C **********************************
C ****  EXECUTE commands
C **********************************
C
      COMMAND = ' '
      DO WHILE ( COMMAND(1:4) .NE. 'EXIT' )
        CALL PUMENUDO(TITLE,MENNAM,COMMAND)
      ENDDO
C
  999 RETURN
      END
