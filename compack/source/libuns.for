      INTEGER FUNCTION LIBUNS()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Unpaste display 3 (MINID2)
C-                         VAX-specific (Uses SMG)
C-
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Created  21-OCT-1988   Jan S. Hoftun
C-   Updated 7-NOV-1990 Scott Snyder: don't erase the top part of the display.
C-   Updated 27-SEP-1991 Herbert Greenlee
C-   Modified    13-AUG-1992 sss - compile on ibm
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:MAXLEV.DEF'
      INCLUDE 'D0$INC:COMNUM.INC'
      INCLUDE 'D0$INC:SMGCOM.INC'
      INTEGER SMG$UNPASTE_VIRTUAL_DISPLAY,SMG$MOVE_VIRTUAL_DISPLAY,I
      INTEGER SMG$ERASE_DISPLAY
C----------------------------------------------------------------------
      LIBUNS=SMG$UNPASTE_VIRTUAL_DISPLAY(MINID2,PASTID)
      I=SMG$ERASE_DISPLAY(MAINID,%VAL(0),%VAL(0),%VAL(0),%VAL(0))
      IF(SPLFLG) THEN
c$$$        I=SMG$ERASE_DISPLAY(MINID1,,,,)
        I=SMG$MOVE_VIRTUAL_DISPLAY(MINID1,PASTID,2,2,%VAL(0))
        I=SMG$MOVE_VIRTUAL_DISPLAY(MAINID,PASTID,SPLLIN+1,1,%VAL(0))
        PBROWS=PBSAVE-SPLLIN
      ELSE
        I=SMG$MOVE_VIRTUAL_DISPLAY(MAINID,PASTID,1,1,%VAL(0))
        PBROWS=PBSAVE
      ENDIF
      RETURN
      END
