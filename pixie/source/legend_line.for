      SUBROUTINE LEGEND_LINE(COLORS,LABELS, CODE)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Prints a label explaining the meaning of
C-              the colors of lines. The labels will be
C-              displayed down.
C-
C-   Inputs  : COLORS - Array [C*3] that has the color desired in the legends
C-             LABELS - Array [C*15] that has ths Labels for the legends
C-             CODE   - give the number of different labels to make.
C-                     This number can not be higher than the size of the
C-                     COLORS and LABELS.
C-
C-   Created   8-DEC-1989   Lupe Rosas
C-   Modified 14-JAN-1991   Nobu. (Retained to Temporary segment)
C-   Modified 06-SEP-1991   Nobu. (Add CALL JVSAVE/JVLOAD for 3D View)
C-   Updated   3-OCT-1991   Lupe Howell Drawing the lines of the menu using the
C-             color table.  Color if in color device line style if B/W
C----------------------------------------------------------------------
C-  Argument Declaration:
C-  ---------------------
      IMPLICIT NONE
      INTEGER CODE
      CHARACTER*(*) COLORS(*)
      CHARACTER*(*) LABELS(*)
C----------------------------------------------------------------------
C-  Local Variables:
C-  ----------------
      REAL    XMIN, XMAX, YMIN, YMAX, XPT, YPT, X0, Y0, X1, Y1
      REAL    XSIZ, YSIZ
      REAL    VWSAVE(85)
      INTEGER I,ICOL,KCOL,KINT,KFIL,KSTY
      LOGICAL FLGVAL
C----------------------------------------------------------------------
      IF ( FLGVAL('ZOOMING') ) GO TO 999  ! If zooming do not print legend
      IF ( FLGVAL('BLOW_UP') ) GO TO 999  ! If Blowup flag do not print legend
      CALL JVSAVE(VWSAVE)
      CALL JUPVEC(0.,1.,0.)
      CALL JNORML(0.,0.,-1.)
      CALL J4RGET(1, XMIN, XMAX, YMIN, YMAX)
      XSIZ = (XMAX-XMIN)*.0155
      YSIZ = (XMAX-XMIN)*.0155
      XPT  = XMAX - ( XMAX - XMIN ) * 0.10
      YPT=YMIN + 0.07*(YMAX-YMIN) + 9.*YSIZ
      X0   = XPT
      Y0   = YPT
      X1   = X0 + XSIZ
      Y1   = Y0 - YSIZ
      CALL JOPEN
      CALL JFONT(5)
      CALL JSIZE(XSIZ, YSIZ)
      CALL JJUST(1, 1)
      DO 100 I = 1, CODE
C
C ****  Drawing line for legend
C
        CALL J3MOVE(XPT, YPT, 0.)
        CALL PXCOLCTOI(COLORS(I),ICOL) !Getting the inde code of color
        CALL PXCOLN('CDC',ICOL,4,.TRUE.,KCOL,KINT,KFIL,KSTY) ! Defining color
        CALL J3MOVE(X0,Y1,0.)
        CALL J3DRAW( X1, Y1,0.)
        CALL J3MOVE(X1, Y1, 0.)
        CALL PXCOLR('FOR')  ! Setting color back to foreground for the labels
C
C ****  Writing labels
C
        CALL J1STRG(LABELS(I))
        YPT = YPT - YSIZ * 2.
        Y0  = YPT
        Y1  = Y0 - YSIZ
  100 CONTINUE
  900 CALL JCLOSE
      CALL JVLOAD(VWSAVE)
  999 RETURN
      END
