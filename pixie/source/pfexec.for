      FUNCTION PFEXEC()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : EXEC Interface Routine for PIXIE package
C-                         FDCDIS
C-
C-   Called from the PIXIE hook PXEXEC.
C-
C-   Returned value  : TRUE
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Created  19-SEP-1990   PXBUILD V1.00
C-   Updated  20-FEB-1991   Lupe Howell  Updated to match the new PIXIE 
C-   Updated   7-MAY-1991   Lupe Howell  Fixing the action names 
C-   Updated  29-MAY-1991   Robert E. Avery   Added some displays
C-   Updated  26-SEP-1991   Sharon Hagopian Added DRAW ROAD commands
C-   Updated  14-NOV-1991   Robert E. Avery  Delete obsolete NWA routine 
C-   Updated  25-JAN-1992   Robert E. Avery  Add flip segment display. 
C-   Updated  28-FEB-1992   Robert E. Avery  Add  'FDC ISA SECTORS' display
C-   Updated  11-MAR-1992   Robert E. Avery  Put back PFFADC display 
C-   Updated  25-JUN-1992   Robert E. Avery  Add 3d  of  full fdc.
C-   Updated  14-OCT-1992   Robert E. Avery  Add  'FDC ROAD LAYERS' display
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL PFEXEC
C----------------------------------------------------------------------
      CHARACTER*(*) TITLE
      PARAMETER( TITLE   = 'FDC Event Display' )
      CHARACTER*(*) MENNAM
      PARAMETER( MENNAM  = 'FDCDIS' )
      CHARACTER*40 COMMAND
C----------------------------------------------------------------------
      PFEXEC = .TRUE.
C
C ****  Process commands
C
      COMMAND = ' '
      DO WHILE ( COMMAND .NE. 'EXIT' )
        CALL PUMENUDO(TITLE,MENNAM,COMMAND)
        IF     ( COMMAND .EQ. 'FDC DL & TRACKS') THEN
          CALL PFTHTA
        ELSEIF ( COMMAND .EQ. 'FDC HITS') THEN
          CALL PFHITS
        ELSEIF ( COMMAND .EQ. 'FDC Y-Z VIEW') THEN
          CALL PFDCYZ
        ELSEIF ( COMMAND .EQ. 'FDC X-Z VIEW') THEN
          CALL PFDCXZ
        ELSEIF ( COMMAND .EQ. 'FDC THETA FADC') THEN
          CALL PF8ADC
        ELSEIF ( COMMAND .EQ. 'FDC PHI FADC') THEN
          CALL PF16AD
        ELSEIF ( COMMAND .EQ. 'FDC_R-Z VIEW') THEN
          CALL PFDCRZ
        ELSEIF ( COMMAND .EQ. 'FDC CRATE FADC') THEN
          CALL PZCRAT
        ELSEIF ( COMMAND .EQ. 'FDC VCRATE FADC') THEN
          CALL PZVCRT
        ELSEIF ( COMMAND .EQ. 'FDC 3D TRACKS') THEN
          CALL PFTB3D
        ELSEIF ( COMMAND .EQ. 'FDC 3D 1 TRACK') THEN
          CALL PFD03D
        ELSEIF ( COMMAND .EQ. 'FDC 3D SEGMENTS') THEN
          CALL PFSG3D
        ELSEIF ( COMMAND .EQ. 'FDC THETA SECTOR') THEN
          CALL PF_THETA_SECTOR
        ELSEIF ( COMMAND .EQ. 'FDC THETA BOUNDARY') THEN
          CALL PF_THETA_BOUND
        ELSEIF ( COMMAND .EQ. 'FDC PHI BOUNDARY') THEN
          CALL PF_PHI_BOUND
        ELSEIF ( COMMAND .EQ. 'FDC PHI SECTOR') THEN
          CALL PF_PHI_SECTOR
        ELSEIF ( COMMAND .EQ. 'LAYER FADC' ) THEN   
          CALL PFRZAD                                     
        ELSEIF ( COMMAND .EQ. 'LAYER SECTOR' ) THEN   
          CALL PF_3_SECTORS
        ELSEIF ( COMMAND .EQ. 'LAYER SEGMENTS' ) THEN   
          CALL PF_FIT_SEGMENTS
        ELSEIF ( COMMAND .EQ. 'FDC PRINT TRACKS') THEN
          CALL PF_PR_TRACKS
        ELSEIF ( COMMAND .EQ. 'FDC_DRAW_PHI_ROAD' ) THEN
          CALL PFPHI_ROAD
        ELSEIF ( COMMAND .EQ. 'FDC_DRAW_THETA_ROAD' ) THEN
          CALL PFTHETA_ROAD
        ELSEIF ( COMMAND .EQ. 'FDC FLIP SEGMENT') THEN
          CALL PF_SEGMENT_FLIP
        ELSEIF ( COMMAND .EQ. 'FDC ISA SECTORS') THEN
          CALL PF_ISA_SECTORS
        ELSEIF ( COMMAND .EQ. 'FDC ROAD LAYERS') THEN   
          CALL PF_ROAD_LAYERS
        ELSEIF ( COMMAND .EQ. 'FDC ONE FADC') THEN
          CALL PFFADC
        ELSEIF ( COMMAND .EQ. 'FDC 3D FULL') THEN
          CALL PFDC3D
        ENDIF
      ENDDO
  999 RETURN
      END
