      SUBROUTINE LEGENDPT
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Prints a label explaining the meaning of
C-              the colors, shades or line width styles in a display.
C-              This routine is very similar to LEGEND.   This routine is
C-              desing to print labels in the case of displays that use
C-              colors, in a color device and line styles in monochome
C-              devices.
C-
C-   Created   26-FEB-1990   LUPE HOWELL
C-   Modified  04-DEC-1990   Nobu.(Separated from SUBROUTINE LEGEND3D)
C-   Modified  18-DEC-1990   Nobu. (Retained to Temporary segment)
C-
C----------------------------------------------------------------------
C-  Argument Declaration:
C-  ---------------------
      IMPLICIT NONE
      INTEGER CODE
      INTEGER KCOL, KINT, KFIL, KSTY, LIST
C----------------------------------------------------------------------
C-  Local Variables:
C-  ----------------
      REAL    XMIN, XMAX, YMIN, YMAX, XPT, YPT, X0, Y0, X1, Y1
      REAL    XSIZ, YSIZ, YMED
      REAL    PTMIN,DPT,PTMINRNG,PTMAXRNG
      REAL    VWSAVE(85)
      INTEGER I, COLINES(5)
      CHARACTER*15 LABEL3(5)
      CHARACTER*5 CMIN,CMAX,CPTMINRNG,CPTMAXRNG,PTLAB1
      CHARACTER*4 PTLAB
      LOGICAL FLGVAL
C----------------------------------------------------------------------
      DATA COLINES/6,7,9,14,13/!COLORS OF PT: BLUE,CYAN,GREEN,MAGENTA,RED
      DATA PTLAB/'<PT<'/
      DATA PTLAB1/'PT > '/
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
      YPT=YMIN + 0.09*(YMAX-YMIN) + 9.*YSIZ
      X0   = XPT
      Y0   = YPT
      X1   = X0 + XSIZ
      Y1   = Y0 - YSIZ
C-
      CALL PUGETV ('TRACK PTMIN',PTMIN)
      CALL PUGETV ('TRACK DPT',DPT)
C-
      CALL JOPEN
      CALL JLSTYL(0)         ! Setting the line style back to normal
      CALL JLWIDE(16383)     ! Setting line thickness to normal
      YMED = Y0 - ( (Y0 - Y1)/2.)
      DO 175 I=1,4
        PTMINRNG=PTMIN+(I-1)*DPT
        PTMAXRNG=PTMIN+I*DPT
        WRITE(CMIN,160)PTMINRNG
        WRITE(CMAX,160)PTMAXRNG
        READ(CMIN,170)CPTMINRNG
        READ(CMAX,170)CPTMAXRNG
  160   FORMAT(F4.0)
  170   FORMAT(A5)
        LABEL3(I)=CPTMINRNG//PTLAB//CPTMAXRNG
  175 CONTINUE
      LABEL3(5)=PTLAB1//CPTMAXRNG
      DO 200 I=1, 5
        CALL J3MOVE(XPT, YPT, 0.)
        CALL PXCOLN('CDC',COLINES(I),4,.TRUE.,KCOL,KINT,KFIL,KSTY)
        CALL J3MOVE(X0,YMED,0.)
        CALL J3DRAW(X1,YMED,0.)
        CALL PXCOLR('FOR')     ! Setting color back to foreground for the labels
        CALL JSIZE(XSIZ, YSIZ)
        CALL J3MOVE(X1, Y1, 0.)
        CALL J1STRG(LABEL3(I))
        YPT = YPT - YSIZ * 2.
        Y0  = YPT
        Y1  = Y0 - YSIZ
        YMED = Y0 - ( (Y0 - Y1)/2.)
  200 CONTINUE
  900 CALL JCLOSE
      CALL JVLOAD(VWSAVE)
  999 RETURN
      END
