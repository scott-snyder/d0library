      SUBROUTINE LEGEND3D(COLORS,LABELS,CODE)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Prints a label explaining the meaning of
C-              the colors, shades or line width styles in a display.
C-              This routine is very similar to LEGEND.   This routine is
C-              desing to print labels in the case of displays that use
C-              colors, in a color device and line styles in monochome
C-              devices.
C-
C-   Inputs  : COLORS - Array [I ] that has the color desired in the legends
C-             LABELS - Array [C*15] that has ths Labels for the legends
C-             CODE   - give the number of different labels to make.
C-                     This number can not be higher than the size of the
C-                     COLORS and LABELS.
C-
C-   Created   26-FEB-1990   LUPE HOWELL
C-   Modified  04-DEC-1990   Nobu. (Separated ENTRY LEGENDPT)
C-   Modified  18-DEC-1990   Nobu. (Retained to Temporary segment)
C-
C----------------------------------------------------------------------
C-  Argument Declaration:
C-  ---------------------
      IMPLICIT NONE
      INTEGER CODE
      INTEGER COLORS(*)
      INTEGER KCOL, KINT, KFIL, KSTY, LIST
      CHARACTER*(*) LABELS(*)
C----------------------------------------------------------------------
C-  Local Variables:
C-  ----------------
      REAL    XMIN, XMAX, YMIN, YMAX, XPT, YPT, X0, Y0, X1, Y1
      REAL    XSIZ, YSIZ, YMED
      REAL    VWSAVE(85)
      INTEGER I
      LOGICAL INTENSITY, FLGVAL
C----------------------------------------------------------------------
      IF ( FLGVAL('ZOOMING') ) GO TO 999
      IF ( FLGVAL('BLOW_UP') ) GO TO 999
      CALL JVSAVE(VWSAVE)
      CALL JUPVEC(0.,1.,0.)
      CALL JNORML(0.,0.,-1.)
      CALL J4RGET(1, XMIN, XMAX, YMIN, YMAX)
      CALL JIQDEV( 1, 1, LIST )
      XSIZ = (XMAX-XMIN)* .0115
      YSIZ = (XMAX-XMIN)* .0215
      XPT  = XMAX - ( XMAX - XMIN ) * 0.16
      YPT  = YMAX - ( YMAX - YMIN ) * 0.09
      X0   = XPT
      Y0   = YPT
      X1   = X0 + XSIZ
      Y1   = Y0 - YSIZ
      CALL JOPEN
      CALL JLSTYL(0)         ! Setting the line style back to normal
      CALL JFONT(5)
      CALL JSIZE(XSIZ, YSIZ)
      CALL JJUST(1, 1)
      DO 100 I = 1, CODE
        CALL J3MOVE(XPT, YPT, 0.)
        CALL PXCOLN('CDC',COLORS(I),3,.FALSE.,KCOL,KINT,KFIL,KSTY)
        CALL JCOLOR(KCOL)
        CALL JLWIDE(KSTY)
        IF ( (LIST.LE.2).OR.(FLGVAL('INTENSITY')) ) THEN
          CALL J3MOVE(X0,Y0,0.)
          CALL J3DRAW(X1,Y0,0.)
          CALL J3MOVE(X1,Y0,0.)
          CALL J3DRAW(X1,Y1,0.)
          CALL J3MOVE(X1,Y1,0.)
          CALL J3DRAW(X0,Y1,0.)
          CALL J3MOVE(X0,Y1,0.)
          CALL J3DRAW(X0,Y0,0.)
        ELSE
          CALL JRECT(X0,Y0, X1, Y1)
        ENDIF
        CALL PXCOLR('FOR')   ! Setting color back to foreground for the labels
        CALL JSIZE(XSIZ, YSIZ)
        CALL J3MOVE(X1, Y1, 0.)
        CALL JLSTYL(0)         ! Setting the line style back to normal
        CALL JLWIDE(16383)     ! Setting line thickness to normal
        CALL J1STRG(LABELS(I))
        YPT = YPT - YSIZ * 2.
        Y0  = YPT
        Y1  = Y0 - YSIZ
  100 CONTINUE
      CALL JCLOSE
      CALL JVLOAD(VWSAVE)
  999 RETURN
      END
