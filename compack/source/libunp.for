      INTEGER FUNCTION LIBUNP()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Unpaste display 2 (MINID1)
C-                         VAX-specific (Uses SMG)
C-
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Documented 26-SEP-1988   Jan S. Hoftun
C-   Updated    27-SEP-1991   Herbert Greenlee
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
      LIBUNP=SMG$UNPASTE_VIRTUAL_DISPLAY(MINID1,PASTID)
      I=SMG$ERASE_DISPLAY(MAINID,
     &                    %VAL(0),%VAL(0),%VAL(0),%VAL(0))
      IF(STAFLG) THEN
        I=SMG$MOVE_VIRTUAL_DISPLAY(MAINID,PASTID,1+STALIN,1,%VAL(0))
        PBROWS=PBSAVE-STALIN
      ELSE
        I=SMG$MOVE_VIRTUAL_DISPLAY(MAINID,PASTID,1,1,%VAL(0))
        PBROWS=PBSAVE
      ENDIF
      RETURN
      END
