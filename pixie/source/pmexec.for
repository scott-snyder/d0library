      FUNCTION PMEXEC( )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : EXEC Interface Routine for PIXIE package
C-                         MUODIS
C-
C-   Called from the PIXIE hook PXEXEC.
C-
C-   Returned value  : TRUE
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Created  25-SEP-1990   Lupe Howell
C-   Updated  28-NOV-1990   Harrison B. Prosper
C-      Changed name to from MUEXEC to PMEXEC
C-   Updated  25-FEB-1991   Lupe Howell  Updating MUON to the latest release
C-   Updated  12-APR-1991   Silvia Repond
C-      Add PMDUMP
C-   Updated  02-JAN-1994   Vipin Bhatnagar  Added submenu SCINT_DISPLAY
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL PMEXEC
C----------------------------------------------------------------------
      CHARACTER*(*) TITLE
      PARAMETER( TITLE   = 'MUON Event Display' )
      CHARACTER*(*) MENNAM
      PARAMETER( MENNAM  = 'MUODIS' )
      CHARACTER*40 COMMAND
C----------------------------------------------------------------------
      PMEXEC = .TRUE.
C
C ****  Process commands
C
      COMMAND = ' '
      DO WHILE ( COMMAND .NE. 'EXIT' )
        CALL PUMENUDO(TITLE,MENNAM,COMMAND)
        IF (     COMMAND .EQ. 'TK BLOWUP' ) THEN
          CALL MU_TK_BLOWUP
        ELSEIF ( COMMAND .EQ. 'Y-Z VIEW'  ) THEN
          CALL PMVIE1
        ELSEIF ( COMMAND .EQ. 'X-Y VIEW' ) THEN
          CALL PMVIE2
        ELSEIF ( COMMAND .EQ. 'X-Z VIEW' ) THEN
          CALL PMVIE3
        ELSEIF ( COMMAND .EQ. 'YZ CUT'    ) THEN
          CALL MUON_YZCUT_F
        ELSEIF ( COMMAND .EQ. 'XY CUT'    ) THEN
          CALL MUON_XYCUT_F
        ELSEIF ( COMMAND .EQ. 'ZX CUT'    ) THEN
          CALL MUON_ZXCUT_F
        ELSEIF ( COMMAND .EQ. 'YZ CEN'    ) THEN
          CALL MUON_YZCUT_C
        ELSEIF ( COMMAND .EQ. 'R-Z VIEW'  ) THEN
          CALL PMRZTK
        ELSEIF (COMMAND  .EQ. 'SCINT_DISPLAY$'  ) THEN
          CALL PMSCINT
        ELSEIF ( COMMAND .EQ. 'X-Y_A-LAYER CUT' ) THEN
          CALL MUON_XYCUT_A
        ELSEIF ( COMMAND .EQ. 'Y-Z_A-LAYER CUT' ) THEN
          CALL MUON_YZCUT_A
        ELSEIF ( COMMAND .EQ. 'Z-X_A-LAYER CUT' ) THEN
          CALL MUON_ZXCUT_A
        ELSEIF ( COMMAND .EQ. 'MUON 3-D' ) THEN
          CALL PM3DTH
          CALL PM3DMD
        ELSEIF ( COMMAND .EQ. 'MUON_DUMP$' ) THEN
          CALL PMDUMP
        ENDIF
      ENDDO
  999 RETURN
      END
