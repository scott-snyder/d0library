      FUNCTION PCISZG_EXEC
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Execute combined view displays. These displays
C-   are built from elementary actions specified in the RCP file
C-   PX_ISZGRF_RCP.
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
      PARAMETER( MENNAM  = 'ISZGRF' )
      CHARACTER*(*) RCPFILE
      PARAMETER( RCPFILE = 'PX_ISZGRF_RCP' )
      CHARACTER*40 COMMAND
      LOGICAL PCISZG_EXEC
C----------------------------------------------------------------------
      PCISZG_EXEC = .TRUE.
C
C **********************************
C ****  EXECUTE commands
C **********************************
C
      COMMAND = ' '
      DO WHILE ( COMMAND .NE. 'EXIT' )
        CALL PUMENUDO(TITLE,MENNAM,COMMAND)
        IF     ( COMMAND .EQ. '2D ISAJET EVENT' ) THEN
          CALL ISZ2D
        ELSEIF ( COMMAND .EQ. '3D ISAJET EVENT' ) THEN
          CALL ISZ3D
        ENDIF
      ENDDO
C
  999 RETURN
      END
