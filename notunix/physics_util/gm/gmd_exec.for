      FUNCTION GMD_EXEC ()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : EXEC Interface Routine for PIXIE package
C-                         GM_DISPLAY
C-
C-   Called from the PIXIE hook PXEXEC.
C-
C-   Returned value  : TRUE
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Created  11-DEC-1991   PXBUILD V1.00
C-   Updated   8-MAY-1993   Harrison B. Prosper
C-      Add call to check flags
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL GMD_EXEC
C----------------------------------------------------------------------
      CHARACTER*(*) TITLE
      PARAMETER( TITLE   = '4-Vectors from GM Ntuple DST' )
      CHARACTER*(*) MENNAM
      PARAMETER( MENNAM  = 'GM_DISPLAY' )
      CHARACTER*40 COMMAND
C----------------------------------------------------------------------
      GMD_EXEC = .TRUE.
C
      CALL GMD_CREATE_LIST
C
C ****  Process commands
C
      COMMAND = ' '
      DO WHILE ( COMMAND .NE. 'EXIT' )
        CALL PUMENUDO(TITLE, COMMAND)
        IF     ( COMMAND .EQ. 'PARTONS' ) THEN
          CALL GMD_DRAW_PARTONS
        ELSEIF ( COMMAND .EQ. 'RECO' ) THEN
          CALL GMD_DRAW_RECO
        ELSEIF ( COMMAND .EQ. 'FULL_EVENT' ) THEN
          CALL GMD_DRAW_FULL_EVENT
        ELSEIF ( COMMAND .EQ. 'PARTIAL_EVENT' ) THEN
          CALL GMD_DRAW_PARTIAL_EVENT
        ENDIF
      ENDDO
C
      CALL GMD_CHECK_FLAGS
      RETURN
      END
