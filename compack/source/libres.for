      INTEGER FUNCTION LIBRES()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Restore physical screen as saved before with LIBSAV
C-                         VAX-specific (Uses SMG)
C-
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Documented 26-SEP-1988   Jan S. Hoftun
C-   Updated    30-SEP-1991   Herbert Greenlee
C-   Modified    13-AUG-1992 sss - compile on ibm
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:MAXLEV.DEF'
      INCLUDE 'D0$INC:COMNUM.INC'
      INCLUDE 'D0$INC:SMGCOM.INC'
      INTEGER SMG$COPY_VIRTUAL_DISPLAY,SMG$PASTE_VIRTUAL_DISPLAY,I
C----------------------------------------------------------------------
      MAINID=SAVEID
      IF(SPLFLG) THEN
        IF(STAFLG) THEN
          I=SMG$PASTE_VIRTUAL_DISPLAY(MAINID,PASTID,SPLLIN+STALIN+1,1,
     &                                %VAL(0))
        ELSE
          I=SMG$PASTE_VIRTUAL_DISPLAY(MAINID,PASTID,SPLLIN+1,1,%VAL(0))
        ENDIF
      ELSE
        IF(STAFLG) THEN
          I=SMG$PASTE_VIRTUAL_DISPLAY(MAINID,PASTID,STALIN+1,1,%VAL(0))
        ELSE
          I=SMG$PASTE_VIRTUAL_DISPLAY(MAINID,PASTID,1,1,%VAL(0))
        ENDIF
      ENDIF
      LIBRES=I
      RETURN
      END
