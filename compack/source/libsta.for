      INTEGER FUNCTION LIBSTA()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Paste display 3 (MINID2) (message) on top of display
C-
C-   Inputs  : None
C-   Outputs : None
C-   Controls: Uses MINID2 in /SMGCOM/ as ID of virtual display
C-
C-   Created  21-OCT-1988   Jan S. Hoftun
C-   Updated 7-NOV-1990 Scott Snyder: don't erase the top part of the display.
C-   Updated 18-SEP-1991   Herbert Greenlee
C-   Modified    13-AUG-1992 sss - compile on ibm
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:MAXLEV.DEF'
      INCLUDE 'D0$INC:COMNUM.INC'
      INCLUDE 'D0$INC:SMGCOM.INC'
      LOGICAL SMG$PASTE_VIRTUAL_DISPLAY,SMG$MOVE_VIRTUAL_DISPLAY,I
      LOGICAL SMG$ERASE_DISPLAY,SMG$DELETE_VIRTUAL_DISPLAY
      LOGICAL SMG$CREATE_VIRTUAL_DISPLAY,SMG$CHANGE_VIRTUAL_DISPLAY
      INTEGER STASAV
      INTEGER SMG$M_BORDER
      DATA SMG$M_BORDER/1/
C----------------------------------------------------------------------
      IF(SPLFLG) THEN
c$$$        I=SMG$ERASE_DISPLAY(MINID1,,,,)
        I=SMG$MOVE_VIRTUAL_DISPLAY(MINID1,PASTID,STALIN+1,2,%VAL(0))
      ENDIF
      I=SMG$ERASE_DISPLAY(MAINID,%VAL(0),%VAL(0),%VAL(0),%VAL(0))
      IF(SPLFLG) THEN
        I=SMG$MOVE_VIRTUAL_DISPLAY(MAINID,PASTID,STALIN+SPLLIN,1,
     &                             %VAL(0))
      ELSE
        I=SMG$MOVE_VIRTUAL_DISPLAY(MAINID,PASTID,STALIN+1,1,%VAL(0))
      ENDIF
      IF(STASAV.NE.STALIN) THEN
        I=SMG$DELETE_VIRTUAL_DISPLAY(MINID2)
C
C     Create display for messages in MESSAGE screen, using STALIN lines on top
C
        I=SMG$CREATE_VIRTUAL_DISPLAY(STALIN-2,PBCOLS-2,MINID2,
     &      SMG$M_BORDER,%VAL(0),%VAL(0))
        IF(.NOT.I) THEN
          CALL MSGSCR(I,'VIRTUAL_DISPLAY3-->')
        ENDIF
        STASAV=STALIN
      ENDIF
      LIBSTA=SMG$PASTE_VIRTUAL_DISPLAY(MINID2,PASTID,2,2,%VAL(0))
      IF(SPLFLG) THEN
        PBROWS=PBSAVE-SPLLIN-STALIN+1     ! Use only bottom part of screen for
      ELSE
        PBROWS=PBSAVE-STALIN
      ENDIF
      RETURN
      END
