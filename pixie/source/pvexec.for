      FUNCTION PVEXEC ()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : EXEC Interface Routine for PIXIE package
C-                         VTX
C-
C-   Called from the PIXIE hook PXEXEC.
C-
C-   Returned value  : TRUE
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Created  20-SEP-1990   PXBUILD V1.00
C-   Updated  25-FEB-1991   Lupe Howell  Updating to match the newest release 
C-   Updated  26-APR-1991   Lupe Howell  Adding a new action routine 
C-   Updated  26-SEP-1991   Sharon Hagopian Added DRAW ROAD commands
C-   Updated  29-OCT-1992   Lupe Howell  VTX_3D added 
C-   Updated  28-FEB-1993   Myungyun Pang VTX_IMPACT added
C-   Updated  28-June-1994  Danilo Puseljic Got rid of PVSADC option
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL PVEXEC
C----------------------------------------------------------------------
      CHARACTER*(*) TITLE
      PARAMETER( TITLE   = 'VTX Event Display' )
      CHARACTER*(*) MENNAM
      PARAMETER( MENNAM  = 'VTXDIS' )
      CHARACTER*(*) RCPFILE
      PARAMETER( RCPFILE = 'PX_VTXDIS_RCP' )
      CHARACTER*40 COMMAND
C----------------------------------------------------------------------
      PVEXEC = .TRUE.
C
C ****  Process commands
C
      COMMAND = ' '
      DO WHILE ( COMMAND .NE. 'EXIT' )
        CALL PUMENUDO(TITLE,MENNAM,COMMAND)
        IF     ( COMMAND .EQ. 'VTX_R-PHI VIEW' ) THEN
          CALL PVVIEW
        ELSEIF ( COMMAND .EQ. 'VTX_R-Z VIEW' ) THEN
          CALL PVRZVW
        ELSEIF ( COMMAND .EQ. 'VTX_Z-Y VIEW' ) THEN
          CALL PVERTX
        ELSEIF ( COMMAND .EQ. 'VTX_FADC_WIRES' ) THEN
          CALL PVWADC
        ELSEIF ( COMMAND .EQ. 'VTX_XY_TK' ) THEN
          CALL PVXYTK
        ELSEIF ( COMMAND .EQ. 'VTX_CHAMBERS' ) THEN
          CALL PVHSEC
        ELSEIF ( COMMAND .EQ. 'VTX_DRAW_PHI_ROAD' ) THEN
          CALL PVPHI_ROAD
        ELSEIF ( COMMAND .EQ. 'VTX_DRAW_THETA_ROAD' ) THEN
          CALL PVTHETA_ROAD
        ELSEIF ( COMMAND .EQ. 'VTX_3D' ) THEN
          CALL PVTX3D
        ELSEIF ( COMMAND .EQ. 'VTX_IMPACT' ) THEN
          CALL PVIMPACT
        ENDIF
      ENDDO
  999 RETURN
      END
