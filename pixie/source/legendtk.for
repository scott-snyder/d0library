      SUBROUTINE LEGENDTK
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Prints a track name explaining the meaning of
C-              the colors, shades or line width styles in a display.
C-
C-   Created   8-FEB-1991   Nobuaki Oshima ( Modified LEGENDPT.FOR )
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INTEGER CODE
      INTEGER KCOL, KINT, KFIL, KSTY, LIST
      REAL    XMIN, XMAX, YMIN, YMAX, XPT, YPT, X0, Y0, X1, Y1
      REAL    XSIZ, YSIZ, YMED
      REAL    PTMIN,DPT,PTMINRNG,PTMAXRNG
      REAL    VWSAVE(85)
      INTEGER I, COLINES(7)
      CHARACTER*10 LABEL3(7)
      LOGICAL FLGVAL
C----------------------------------------------------------------------
C-    COLORS OF TK: CYA,GRE,RED,DRD,BLU,MAG,FOR
      DATA COLINES /  7,  9, 13, 12,  6, 14, 17/ 
      DATA LABEL3  /' JETS',' MUON',' ELECTRON',' PHOTON',' TAU'
     &             ,' MISS. ET',' ZTRACKS'/
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
C-
      CALL JOPEN
      CALL JLSTYL(0)         ! Setting the line style back to normal
      CALL JLWIDE(16383)     ! Setting line thickness to normal
      YMED = Y0 - ( (Y0 - Y1)/2.)
C-
      DO I = 1,7
        IF (I .EQ. 4)    CALL JLSTYL(2)
        IF (I .EQ. 5)    CALL JLSTYL(0)
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
      ENDDO
      CALL JCLOSE
      CALL JVLOAD(VWSAVE)
  999 RETURN
      END
