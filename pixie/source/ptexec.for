      FUNCTION PTEXEC ()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : EXEC Interface Routine for PIXIE package
C-                         TRDDIS
C-
C-   Called from the PIXIE hook PXEXEC.
C-
C-   Returned value  : TRUE
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Created   1-APR-1991   PXBUILD V1.00
C-   Updated  18-APR-1991   Lupe Howell   
C-   Updated  29-OCT-1992   Lupe Howell  Added TRD_3D action 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL PTEXEC
C----------------------------------------------------------------------
      CHARACTER*(*) TITLE
      PARAMETER( TITLE   = 'TRD EVENT DISPLAY' )
      CHARACTER*(*) MENNAM
      PARAMETER( MENNAM  = 'TRDDIS' )
      CHARACTER*40 COMMAND
C----------------------------------------------------------------------
      PTEXEC = .TRUE.
C
C ****  Process commands
C
      COMMAND = ' '
      DO WHILE ( COMMAND .NE. 'EXIT' )
        CALL PUMENUDO(TITLE,MENNAM,COMMAND)
        IF     ( COMMAND .EQ. 'TRD_TK_R-PHI' ) THEN
          CALL PTRDTK
        ELSEIF ( COMMAND .EQ. 'TRD_HITS_DISPLAY' ) THEN
          CALL PTEVEN
        ELSEIF ( COMMAND .EQ. 'ZSTRIP PLOT' ) THEN
          CALL PTZSTRIP
        ELSEIF ( COMMAND .EQ. 'SELECT_WIRE_HITS$' ) THEN
          CALL PTPICK
        ELSEIF ( COMMAND .EQ. 'SELECT_WIRE_TK_R-PHI$' ) THEN
          CALL PTKPICK
        ELSEIF ( COMMAND .EQ. 'TRD_3D_GEOMETRY' ) THEN
          CALL PTRD3D_GEO
        ENDIF
      ENDDO
  999 RETURN
      END
