      SUBROUTINE ZSAXLB(XMIN,XMAX,YMIN,YMAX,ZMIN,DISTY)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Will display the axis labels of the Z-strip
C-                   TRD display
C-
C-   Inputs  : XIMN - Minimum coordenate in the X direction
C-             XMAX - Maximum coordenate in the X direction
C-             YMIN - Minimum coordenate in the Y direction
C-             YMAX - Maximum coordenate in the Y direction
C-             ZMIN - Minimum coordinate in the Z-strip axis
C-             DISTY- Amount of space between each z-strip line on the
C-                    Y direction
C-
C-   Created  16-JUN-1989   LUPE ROSAS
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL        XMIN, XMAX, YMIN, YMAX, TYMIN, TXMIN, YMOD,
     X            ZMIN, ZMAX, TMPY, CNT, DISTY, TMPX, XMOD1,
     X            XMOD2, ZSLAB, ZSMAX, ZSMIN, YMOD1, YMOD2
      INTEGER     I
      CHARACTER*4 CLABEL, CX
      
C----------------------------------------------------------------------
C- Printing the y axis
      YMOD = (YMAX+1.)/4.
      TXMIN = XMIN - 4.
      TYMIN = YMIN - 1.
      CALL PUOPEN
      CALL PUFTOC((YMIN+1.),'F4.0',CX,CLABEL)
      CALL JFONT(5)      
      CALL JJUST(2,2)
      CALL JSIZE(2.,4.)
      CALL J3MOVE(TXMIN, YMIN, 0.)
      CALL J1STRG(CLABEL)
      CALL J3MOVE(XMIN-.5,YMIN, 0.) ! Drawing first axis 
      CALL J3DRAW(XMIN,YMIN,0.)
      DO 20 I=1, 4
      TYMIN = TYMIN + YMOD 
      CALL PUFTOC((TYMIN+1.),'F4.0',CX,CLABEL)
      CALL JFONT(5)      
      CALL JJUST(2,2)
      CALL JSIZE(2.,4.)
      CALL J3MOVE(TXMIN, TYMIN, 0.)
      CALL J1STRG(CLABEL)
      CALL J3MOVE(XMIN-.5,TYMIN, 0.) ! Drawing line on axis
      CALL J3DRAW(XMIN,TYMIN,0.)
   20 CONTINUE
      CALL JRCLOS
C-  Printing the horizontal Z-strip axis
      ZSMAX = FLOAT( IFIX ((256. - YMIN)/DISTY) ) - 1.
      ZSMIN = ZMIN + 1.
      XMOD1 = (ZSMAX-ZSMIN)/4.
      XMOD2 = (XMAX)/4.
      ZSLAB = ZSMIN - XMOD1
      TMPX = XMIN - XMOD2
      DO 30 I=1, 5
        ZSLAB = ZSLAB + XMOD1
        TMPX = TMPX + XMOD2  ! ???
        CALL PUFTOC(ZSLAB,'F4.0',CX,CLABEL)
        CALL PUOPEN
        CALL JFONT(5)      
        CALL JJUST(2,2)
        CALL JSIZE(2.,4.)
        CALL J3MOVE(TMPX, YMAX+4.,0.)
        CALL J1STRG(CLABEL)
        CALL J3MOVE(TMPX,YMAX+.5,0.)
        CALL J3DRAW(TMPX,YMAX,0.)
        CALL JRCLOS
   30 CONTINUE
C-  Printing the vertical Z-strip axis 
      YMOD1 = ((256. - ZSLAB + 1.) + ZMIN)/4.
      YMOD2 = (YMAX-YMIN)/4.
      TMPY = YMIN - YMOD2
      ZSLAB = ZMIN + YMOD1
      DO 40 I=1, 5
        ZSLAB = ZSLAB - YMOD1
        TMPY = TMPY + YMOD2
        IF (ZSLAB.LE.0) 
     X    ZSLAB = 256.+ ZSLAB
        CALL PUOPEN
        CALL PUFTOC(ZSLAB,'F4.0',CX,CLABEL)
        CALL JFONT(5)      
        CALL JJUST(2,2)
        CALL JSIZE(2.,4.)
        CALL J3MOVE((XMAX+6.),TMPY, 0.)
        CALL J1STRG(CLABEL)
        CALL J3MOVE(XMAX+.5,TMPY, 0.) ! Drawing first axis 
        CALL J3DRAW(XMAX,TMPY,0.)
        CALL JRCLOS
   40 CONTINUE
  999 RETURN
      END
