      SUBROUTINE PXBUILD_MODIFY
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Builds the menu to modify RCP file
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created   8-MAY-1991   Lupe Howell
C-   Updated  19-JUN-1991   Lupe Howell  Tidy up
C-   Updated   6-NOV-1991   Lupe Howell  Merge implemented
C-   Updated  30-JAN-1993   Lupe Howell Fix bad goto
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:PXBUILDCOM.INC'
C----------------------------------------------------------------------
      CHARACTER*6 MODNAME
      CHARACTER*32 COMMAND,COMMAND2,RCPFILE,RCPMENU_NAME(MAXMENU)
      LOGICAL FIRST,EZERROR,FOUND
      INTEGER I,J,JJ,K,LM,IER,RCPMENU_COUNT
C
      DATA FIRST/.TRUE./
      SAVE FIRST
      SAVE RCPMENU_COUNT,RCPMENU_NAME
C----------------------------------------------------------------------
C
C ****  If no RCP files have been read exit.
C
      IF ( RCPFILE_COUNT .EQ. 0 ) THEN
        CALL INTMSG(' You MUST read a RCP file to Modify one')
        GOTO 999
      ENDIF

      COMMAND = ' '
C
C ****  Skip the display of the RCP files menu if it is only one RCP file
C
      IF ( RCPFILE_COUNT .EQ. 1 ) THEN
        COMMAND = RCPFILE_NAME(1)
      ENDIF
      DO WHILE ( COMMAND .NE. 'EXIT' )
        IF ( RCPFILE_COUNT .GT. 1 ) THEN
          CALL MENUDO('    Modify RCP files','RCP_FILES',COMMAND)
        ENDIF
C
        IF ( COMMAND .NE. 'EXIT' ) THEN
   10     CONTINUE
          COMMAND2 = ' '
          CALL  EZPICK(COMMAND)
          DO WHILE (COMMAND2 .NE. 'EXIT' )
            CALL MENUDO('      MODIFY '//COMMAND,COMMAND,COMMAND2)
            IF ( COMMAND2 .EQ. 'EDIT MENU' ) THEN
              CALL PXBUILD_SELECT(COMMAND)
            ELSEIF ( COMMAND2 .EQ. 'EDIT SCREEN' ) THEN
              CALL PXMODIFY_EDIT(COMMAND)
            ELSEIF ( COMMAND2 .EQ. 'MERGE' ) THEN
              CALL PXMODIFY_MERGE(COMMAND)
            ELSEIF ( COMMAND2 .EQ. 'CHANGE' ) THEN
              CALL PXMODIFY_CHANGE_SCREEN(COMMAND)
            ELSEIF ( COMMAND2 .EQ. 'WRITE RCP' ) THEN
              CALL PXMODIFY_WRITE(COMMAND)
            ELSEIF ( COMMAND2 .EQ. 'WRITE ROUTINES' ) THEN
              CALL PXBUILD_WRITE_ROUTINES(COMMAND)
            ENDIF
          ENDDO
          CALL EZRSET
        ENDIF
        IF ( RCPFILE_COUNT .EQ. 1 ) THEN
          COMMAND = 'EXIT'
        ENDIF
      ENDDO
  999 RETURN
      END
