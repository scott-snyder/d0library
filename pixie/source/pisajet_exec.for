      FUNCTION PISAJET_EXEC()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Execute displays of ISAJET tracks. These displays
C-   are built from elementary actions specified in the RCP file
C-   PX_ISAJETDIS_RCP.
C-
C-   Returned value  : TRUE
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Creayed  8-MAR-1992 S. Hagopian
C-
C----------------------------------------------------------------------
      INTEGER IER
      CHARACTER*(*) TITLE
      PARAMETER( TITLE   = 'ISAJET Event Display' )
      CHARACTER*(*) MENNAM
      PARAMETER( MENNAM  = 'ISAJETDIS' )
      CHARACTER*(*) RCPFILE
      PARAMETER( RCPFILE = 'PX_ISAJETDIS_RCP' )
      CHARACTER*40 COMMAND
      LOGICAL PISAJET_EXEC
C----------------------------------------------------------------------
      PISAJET_EXEC = .TRUE.
C
C **********************************
C ****  EXECUTE commands
C **********************************
C
      COMMAND = ' '
      DO WHILE ( COMMAND .NE. 'EXIT' )
        CALL PUMENUDO(TITLE,MENNAM,COMMAND)
        IF     ( COMMAND .EQ. 'ISAJET_Y-Z_VIEW' ) THEN
          CALL ISZ2D
        ELSEIF ( COMMAND .EQ. 'ISAJET_X-Y_VIEW' ) THEN
          CALL ISZ2D
        ELSEIF ( COMMAND .EQ. 'ISAJET_Z-X_VIEW' ) THEN
          CALL ISZ2D
        ELSEIF ( COMMAND .EQ. '3D ISAJET EVENT' ) THEN
          CALL ISZ3D
        ENDIF
      ENDDO
C
  999 RETURN
      END
