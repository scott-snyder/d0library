      SUBROUTINE PXBUILD_WRITE_ROUTINES(CURRENT_RCP)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Write out PIXIE interface routines
C-
C-   Inputs  : CURRENT_RCP [C*]: Name of the current RCP file
C-   Outputs : None
C-
C-   Created  31-DEC-1991   Lupe Howell  Based on PXBUILD_WRITE
C-                          by Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) CURRENT_RCP
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:PXBUILDCOM.INC'
C----------------------------------------------------------------------
      CHARACTER*5  VERSION
      CHARACTER*80 TITLE
      CHARACTER*32 INIT_ROUTINE,EXEC_ROUTINE(MAXLIST),PACKAGE
C
      INTEGER I,J,II,JJ,LSM,MENU_INDX,LRCP,PFNUM
C----------------------------------------------------------------------
C
C ****  Get the menu index of the current RCP file
C
      CALL WORD(CURRENT_RCP,I,J,LRCP)
      I = 0
      DO WHILE ( I .LT. RCPFILE_COUNT ) 
        I = I + 1
        IF ( CURRENT_RCP(1:LRCP) .EQ. RCPFILE_NAME(I)(1:LRCP) ) THEN
          MENU_INDX = I 
        ENDIF
      ENDDO
C
C ****  Getting the package Name form the RCP name
C
      J = INDEX(CURRENT_RCP,'PX_')
      IF ( J .GT. 0 ) THEN
        J = J + 2
      ENDIF
      I = INDEX(CURRENT_RCP,'_RCP')
      PACKAGE = CURRENT_RCP(J+1:I-1)
C
C ****  Prompt for some info
C
      TITLE = ' '
      CALL GETPAR(1,' Menu Title   > ','C',TITLE)
      IF ( PFNUM() .EQ. 4 ) GOTO 999
      IF ( TITLE(1:1) .EQ. ' ' ) THEN
        TITLE = 'The time has come the walrus said..'
      ENDIF
C
      INIT_ROUTINE = ' '
      CALL GETPAR(1,' Routine Name (INIT)> ','U',INIT_ROUTINE)
      IF ( PFNUM() .EQ. 4 ) GOTO 999
C
C ****  The *EXEC routines match the number of submenus
C ****  For each sub menu there must be ab EXEC routine
C
      EXEC_ROUTINE(1) = ' '
      IF ( SUBMENU_COUNT(MENU_INDX) .EQ. 1 ) THEN
        CALL GETPAR(1,' Routine Name (EXEC)> ','U',EXEC_ROUTINE(1))
      ELSE
        DO I = 1, SUBMENU_COUNT(MENU_INDX)
          CALL WORD(ACTION_SUBMENU(MENU_INDX,I),II,JJ,LSM)
          CALL GETPAR(1,' Routine Name (EXEC)for submnet '
     &          //ACTION_SUBMENU(MENU_INDX,I)(1:LSM)//'> ','U',
     &          EXEC_ROUTINE(
     &          I))
        ENDDO
      ENDIF
      IF ( PFNUM() .EQ. 4 ) GOTO 999
C
      CALL PXBUILD_VERSION(VERSION)
C
C ****  Write INIT routine
C
      IF ( INIT_ROUTINE(1:1) .NE. ' ' ) THEN
        CALL PXBUILD_INIT(VERSION,PACKAGE,INIT_ROUTINE)
      ENDIF
C
C ****  Write EXEC routine
C
      IF ( EXEC_ROUTINE(1)(1:1) .NE. ' ' ) THEN
        DO I = 1, SUBMENU_COUNT(MENU_INDX)
          CALL PXBUILD_EXEC(VERSION,PACKAGE,EXEC_ROUTINE(I),
     &          MENU_INDX,I,TITLE)
        ENDDO
      ENDIF
  999 RETURN
      END
