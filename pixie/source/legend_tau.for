      SUBROUTINE LEGEND_TAU(COLOR,LABEL,CODE)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Draws a "T" for a tau and a label
C-                          explaining its meaning.
C-
C-   Inputs  : COLOR - the color of the arrow
C-             LABEL - Label for the legend
C-             CODE   - Tells how many rows donw to skip before printing
C-                      the arrow and its legend. (So it can be used
C-                      together with SUBROUTINE LEGEND.)
C-
C-   Created  21-OCT-1992   Sharon Hagopian
C-                 Based on SUBROUTINE LEGEND written by Lupe Rosas Howell
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INTEGER CODE
      CHARACTER*(*) COLOR
      CHARACTER*(*) LABEL

C----------------------------------------------------------------------
C-  Local Variables:
C-  ----------------
      REAL    XMIN, XMAX, YMIN, YMAX, XPT, YPT, X0, Y0, X1, Y1
      REAL    XSIZ, YSIZ
      REAL XTAU
      INTEGER I
      LOGICAL FLGVAL
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
        YPT = YPT - YSIZ * 2.
        Y0  = YPT
        Y1  = Y0 - YSIZ
  100 CONTINUE
C DRAW TAU
        CALL J3MOVE(X0,Y1,0.)
        CALL J3DRAW(X0,Y0,0.)
        XTAU=(X1-X0)/4.
        CALL J3MOVE(X0,Y0,0.)
        CALL J3DRAW(X0-XTAU,Y0,0.)
        CALL J3MOVE(X0,Y0,0.)
        CALL J3DRAW(X0+XTAU,Y0,0.)
C PRINT LABEL
        CALL J3MOVE(X1, Y1, 0.)
        CALL PXCOLR('FOR')  ! Setting color back to foreground for the labels
        CALL J1STRG(LABEL)
  900 CALL JCLOSE
  999 RETURN
      END
