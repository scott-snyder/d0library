      FUNCTION PPEXEC()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Driving routine for the Event Display of DST
C-   /MicroDST and ISAJET. Interaction is via COMPACK menus and parameters
C-   from the RCP bank PX_PHYDIS_RCP.
C
C-
C-   Returned value  : .TRUE.
C-   Inputs  : NONE
C-   Outputs : NONE
C-   Controls: NONE
C-
C-   Updated  05-MAY-1993   Nobuaki Oshima
C-                           Added Chip's Top Display stuffs.
C-   Modified 27-APR-1993   V. Bhatnagar( Add '3D ESUM DISPLAY' menu.)
C-   Updated  08-FEB-1993   N. Oshima( Add 'PELC_CASH LEGO' menu. )
C-   Updated  15-JUL-1992   N. Oshima( Add 'ESUM LEGO' menu. )
C-   Updated  16-MAR-1992   N. Oshima( Add 'CAL TOWER LEGO' menu. )
C-   Created  16-SEP-1991   Nobuaki Oshima
C-    
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL PPEXEC
C-
      CHARACTER*(*) TITLE
      PARAMETER( TITLE   = 'PHYSICS Event Display' )
      CHARACTER*(*) MENNAM
      PARAMETER( MENNAM  = 'PHYDIS' )
      CHARACTER*40 COMMAND
C----------------------------------------------------------------------
      PPEXEC = .TRUE.
C-
      COMMAND = ' '
      DO WHILE ( COMMAND .NE. 'EXIT' )
        CALL PUMENUDO(TITLE,MENNAM,COMMAND)
        IF     ( COMMAND .EQ. '3D DST DISPLAY'    ) THEN
          CALL PC3DST
        ELSEIF ( COMMAND .EQ. '3D ESUM DISPLAY'   ) THEN
	  CALL PESUM_TRACK
        ELSEIF ( COMMAND .EQ. 'DST LEGO'          ) THEN
          CALL PCDST_LEGO
        ELSEIF ( COMMAND .EQ. 'PELC_CASH LEGO'    ) THEN
          CALL PCALEG_CASH
        ELSEIF ( COMMAND .EQ. 'CAL TOWER LEGO'    ) THEN
          CALL PCATDL
        ELSEIF ( COMMAND .EQ. 'CAL CAEQ LEGO'     ) THEN
          CALL PPCAEQ
        ELSEIF ( COMMAND .EQ. 'ESUM LEGO'         ) THEN
          CALL PESUM_LEGO
        ELSEIF ( COMMAND .EQ. 'PELC CASH PLOT'    ) THEN
          CALL PELC_CASH_PLOT
        ELSEIF ( COMMAND .EQ. 'ISATOP$'           ) THEN
          CALL PPISATOP
        ENDIF
      ENDDO
C-
  999 RETURN
      END
