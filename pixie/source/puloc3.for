      SUBROUTINE PULOC3(XIN, YIN, XOUT, YOUT, ZOUT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Put the echo position at the center of
C-               WINDOW coordinates. Wait for user input(cursor movement).
C-               Return the WINDOW position, current COMMAND and its 
C-               INDEX.
C-
C-   Inputs  : XIN    [F] : X-Window coordinate for Echo point
C-             YIN    [F] : Y-                "
C-   Outputs   XOUT   [F] : X-Window coordinate of the selected point
C-             YOUT   [F] : Y-                "
C-             ZOUT   [F] : Z-                "
C-   Controls:
C-
C-   Created  13-DEC-1990   Nobuaki Oshima
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:PXPARA.INC/LIST'
      INCLUDE 'D0$PARAMS:ESCAPE_CODE_NAMES.DEF'
      INTEGER ILIST,ICHAR
      REAL    XIN, YIN, XOUT, YOUT, ZOUT
      REAL    RLEVEL
      REAL    RLIST(102),XV, YV
C----------------------------------------------------------------------
      CALL JIQDIL(RLEVEL)
C-
      CALL JCONWV( XIN, YIN, 0., XV, YV )
      CALL JPECHO( 1, 2, 1, XV, YV )
      CALL JIENAB( 1, 2, 1 )
      CALL JLOCAT( 1, 1, 1, ICHAR, XV, YV )
      CALL JIDISA( 1, 2, 1 )
      IF (RLEVEL.LT.0. .AND. RLEVEL.GT. -5.)   THEN
        ILIST = MAXSEG
        CALL JESCAP(FLAG_3D_MATRIX, 1, 0, ILIST, RLIST)
      ENDIF
      CALL JCONVW( XV, YV, XOUT, YOUT, ZOUT )
  999 RETURN
      END
