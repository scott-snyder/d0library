      SUBROUTINE LEGEND_MARKS(COLORS,MARKS,LABELS, CODE)
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
C-   Created   8-JAN-1992 S. Hagopian
C-                       Based on LEGEND.FOR
C-
C----------------------------------------------------------------------
C-  Argument Declaration:
C-  ---------------------
      IMPLICIT NONE
      INTEGER CODE
      CHARACTER*(*) COLORS(*)
      INTEGER MARKS(*)
      CHARACTER*(*) LABELS(*)

C----------------------------------------------------------------------
C-  Local Variables:
C-  ----------------
      REAL    XMIN, XMAX, YMIN, YMAX, XPT, YPT, X0, Y0, X1, Y1
      REAL    XSIZ, YSIZ
      INTEGER I
      CHARACTER*4 IMESS
      CHARACTER*1 IMARK(5)
      CHARACTER*3 IDASH
      LOGICAL FLGVAL
      DATA IDASH/' - '/
      DATA IMARK/'.','+','*','O','X'/
C----------------------------------------------------------------------
      IF ( FLGVAL('ZOOMING') ) GO TO 999
      IF ( FLGVAL('BLOW_UP') ) GO TO 999
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
      CALL PXCOLR(COLORS(I))
      IMESS=IMARK(MARKS(I))//IDASH
        CALL J3MOVE(XPT,Y1,0.)
        CALL J1STRG(IMESS)
        CALL J3MOVE(X1, Y1, 0.)
        CALL PXCOLR('FOR')  ! Setting color back to foreground for the labels
        CALL J1STRG(LABELS(I))
        YPT = YPT - YSIZ * 2.
        Y0  = YPT
        Y1  = Y0 - YSIZ
  100 CONTINUE
  900 CALL JCLOSE
  999 RETURN
      END
