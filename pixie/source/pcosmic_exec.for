      FUNCTION PCOSMIC_EXEC
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Execute combined view displays. These displays
C-   are built from elementary actions specified in the RCP file
C-   PX_COSMIC_RAYS_RCP.
C-
C-   Returned value  : TRUE
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Creayed  18-MAR-1991   Lupe Howell   
C-
C----------------------------------------------------------------------
      INTEGER IER
      CHARACTER*(*) TITLE
      PARAMETER( TITLE   = 'Dzero Combined Event Display' )
      CHARACTER*(*) MENNAM
      PARAMETER( MENNAM  = 'COSMIC_RAYS' )
      CHARACTER*(*) RCPFILE
      PARAMETER( RCPFILE = 'PX_COSMIC_RAYS_RCP' )
      CHARACTER*40 COMMAND
      LOGICAL PCISZGRF_EXEC
C----------------------------------------------------------------------
      PCOSMIC_EXEC = .TRUE.
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
