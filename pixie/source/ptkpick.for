      SUBROUTINE PTKPICK
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Displays a submenu with the following items:
C-      - FADC DISPLAY OF A WIRE:  Lets the user pick a wire of the TRD
C-             end view and it will display the FADC display of the wire
C-             picked.
C-      - ENERGY AND WIRE NUMBER: Lets the user pick a wire of the TRD end
C-             view display and the wire energy and number will be displayed
C-             on the screen.
C-
C-   Output: None
C-
C-   Created  26-JAN-1989   LUPE ROSAS
C-   Updated   4-JUN-1990   Norman A. Graf
C-   Updated  16-APR-1991   Lupe Howell  the menu system was implemented using
C-                      COMPACK, call to PUHEAD_NOCLEAR was use.
C-   Updated  22-FEB-1994   A. Zylberstejn:add an argument to PT_ENRGWIRE_NUM
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      CHARACTER*(*) TITLE
      PARAMETER( TITLE   = 'SELECT WIRE' )
      CHARACTER*(*) MENNAM
      PARAMETER( MENNAM  = 'SELECT_WIRE_TK_R-PHI' )
      CHARACTER*40 COMMAND
      LOGICAL TRONLY,FIRST
      DATA FIRST /.TRUE./
C----------------------------------------------------------------------
      CALL PUGETV('TRD ONLY',TRONLY)
      IF( TRONLY )THEN
        COMMAND = ' '
        DO WHILE ( COMMAND .NE. 'EXIT' )
          CALL PUMENUDO(TITLE,MENNAM,COMMAND)
          write(73,*)' command in ptkpick ',command
C
C ****  Checking CommandS
C
          IF    ( COMMAND .EQ. 'FADC_DISPLAY' ) THEN
            CALL PT_FADCWIRE
          ELSEIF( COMMAND .EQ. 'TRD_HITS_DISPLAY') THEN
            CALL PTEVEN
          ELSEIF( COMMAND .EQ. 'TRD_TK_R-PHI') THEN
            CALL PTRDTK
          ELSEIF( COMMAND .EQ. 'ENERGY AND WIRE NUMBER') THEN
            CALL PT_ENRGWIRE_NUM(2)
          ENDIF
        ENDDO
      ENDIF
  999 RETURN
      END
