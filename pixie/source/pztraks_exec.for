      FUNCTION PZTRAKS_EXEC
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Execute combined view displays defined in the
C-   RCP file D0$PIXIE:PX_ZTRAKS.RCP. This routine belongs to the
C-   package ZTRAKS.
C-
C-   Returned value  : TRUE
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Created  10-NOV-1990   Harrison B. Prosper, Lupe Howell
C-   Updated  26-APR-1991   Lupe Howell   Updating the structure to simulate
C-   other P*EXEC routines
C-   Updated  24-JUN-1991   Nobuaki Oshima   
C-   Updated  30-OCT-1992   Lupe Howell  Added '3D CD TRACKS' 
C-   Updated  25-NOV-1992   Nobuaki Oshima 
C-      Add 'CALL PZ_VERTEX_3D' for the command '3D CD TRACKS'.
C-
C----------------------------------------------------------------------
      LOGICAL PZTRAKS_EXEC
      CHARACTER*(*) TITLE
      PARAMETER( TITLE    = 'ZTRAKS Combined View Displays' )
      CHARACTER*(*) MENNAM
      PARAMETER( MENNAM = 'ZTRAKSDIS' )
      CHARACTER*(*) RCPFILE
      PARAMETER( RCPFILE = 'PX_ZTRAKSDIS_RCP' )
      CHARACTER*40 COMMAND
      LOGICAL PCOMB_EXEC

C----------------------------------------------------------------------
      PZTRAKS_EXEC = .TRUE.
C
C **********************************
C ****  EXECUTE commands
C **********************************
C
      COMMAND = ' '
      DO WHILE ( COMMAND(1:4) .NE. 'EXIT' )
        CALL PUMENUDO(TITLE,MENNAM,COMMAND)
        IF     ( COMMAND .EQ. 'VTX+CDC+FDC_3D_VIEW' ) THEN
          CALL PZCD3D
        ELSEIF ( COMMAND .EQ. 'ZTRAKS_R-Z VIEW' ) THEN
          CALL PZTRAKS_RZ
        ELSEIF ( COMMAND .EQ. 'ZTRAKS_R-PHI VIEW' ) THEN
          CALL PZTRAKS_RPHI
        ELSEIF ( COMMAND .EQ. 'ZTRAKS_TOP VIEW' ) THEN
          CALL PZTRAKS_TOP
        ELSEIF ( COMMAND .EQ. 'ZTRAKS_SIDE VIEW' ) THEN
          CALL PZTRAKS_SIDE
        ELSEIF ( COMMAND .EQ. '3D CD TRACKS' ) THEN
          CALL PZTRK_3D
          CALL PZ_VERTEX_3D
        ENDIF
      ENDDO
C
  999 RETURN
      END
