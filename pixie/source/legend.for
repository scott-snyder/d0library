      SUBROUTINE LEGEND(COLORS,LABELS, CODE)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Prints a label explaining the meaning of
C-              the colors or shades in a lego plot. The labels will be
C-              displayed down.
C-
C-   Inputs  : COLORS - Array [C*3] that has the color desired in the legends
C-             LABELS - Array [C*15] that has ths Labels for the legends
C-             CODE   - give the number of different labels to make.
C-                     This number can not be higher than the size of the
C-                     COLORS and LABELS.
C-
C-   Created   8-DEC-1989   LUPE ROSAS
C-   Modified 14-JAN-1991   Nobu. (Retained to Temporary segment)
C-   Modified  6-MAY-1993   Nobu. (Free from 3D Rotation)
C-
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
      INTEGER I
      LOGICAL FLGVAL
C----------------------------------------------------------------------
      IF ( FLGVAL('ZOOMING') ) GO TO 999
      IF ( FLGVAL('BLOW_UP') ) GO TO 999
C---
      CALL JVSAVE(VWSAVE)
      CALL JUPVEC(0.,1.,0.)
      CALL JNORML(0.,0.,-1.)
C---
      CALL J4RGET(1, XMIN, XMAX, YMIN, YMAX)
      XSIZ = (XMAX-XMIN)*.0115
      YSIZ = (XMAX-XMIN)*.0215
      XPT  = XMAX - ( XMAX - XMIN ) * 0.16
      YPT  = YMAX - ( YMAX - YMIN ) * 0.09
      X0   = XPT
      Y0   = YPT
      X1   = X0 + XSIZ
      Y1   = Y0 - YSIZ
      CALL JOPEN
      CALL JFONT(5)
      CALL JSIZE(XSIZ, YSIZ)
      CALL JJUST(1, 1)
      DO 100 I = 1, CODE
        CALL J3MOVE(XPT, YPT, 0.)
        CALL PXCOLFILL(COLORS(I))
        CALL JRECT(X0,Y0, X1, Y1)
        CALL J3MOVE(X1, Y1, 0.)
        CALL PXCOLR('FOR')  ! Setting color back to foreground for the labels
        CALL J1STRG(LABELS(I))
        YPT = YPT - YSIZ * 2.
        Y0  = YPT
        Y1  = Y0 - YSIZ
  100 CONTINUE
      CALL JCLOSE
C---
      CALL JVLOAD(VWSAVE)
C---
  999 RETURN
      END
