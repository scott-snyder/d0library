      FUNCTION PCOM3DIS_EXEC
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Execute combined 3D view displays. These 
C-   displays are built from elementary actions specified in the RCP 
C-   file PX_COM3DIS_RCP.
C-
C-   Returned value  : TRUE
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Created  22-JUN-1992   Nobuaki Oshima
C-
C----------------------------------------------------------------------
      INTEGER IER
      CHARACTER*(*) TITLE
      PARAMETER( TITLE   = 'D0 Combined 3D Event Display' )
      CHARACTER*(*) MENNAM
      PARAMETER( MENNAM  = 'COM3DIS' )
      CHARACTER*(*) RCPFILE
      PARAMETER( RCPFILE = 'PX_COM3DIS_RCP' )
      CHARACTER*40 COMMAND
      LOGICAL PCOM3DIS_EXEC
C----------------------------------------------------------------------
      PCOM3DIS_EXEC = .TRUE.
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
