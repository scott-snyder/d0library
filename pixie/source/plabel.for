      SUBROUTINE PLABEL(VMIN,XMIN,XMAX,YMIN,YMAX,ZMIN,ZMAX,
     &                  XLAB,YLAB,ZLAB,TYMAX,TXMAX,TZMAX,NX,NXMIN,
     &                  NY,NYMIN,ZSCAL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :  Printing the label in the axis of a lego plot
C-
C-   Inputs  :   VMIN  - Minimum value for the vertical window coordenate of
C-                       display
C-               XMIN  - Minimum value for the X axis label
C-               XMAX  - Maximum value for the X axis label
C-               YMIN  - Minimum value for the Y axis label
C-               YMAX  - Maximum value for the Y axis label
C-               ZMIN  - Minimum value for the Z axis label
C-               ZMAX  - Maximum value for the Z axis label
C-               XLAB  - Label of the X axis
C-               YLAB  - Label of the Y axis
C-               ZLAB  - Label of the Z axis
C-               NX    - Maximum value for the X axis (actual coord.)
C-               NXMIN - Minimum value for the X axis (actual coord.)
C-               NY    - Minimum value for the Y axis (actual coord.)
C-               NYMIN - Minimum value for the Y axis (actual coord.)
C-               ZSCAL - Scale for z axis
C-
C-   Outputs :
C-   Controls:
C-
C-   Created   9-AUG-1988   LUPE ROSAS
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
C-  Argument Declaration:
C-  ---------------------
      REAL VMIN,XMIN,XMAX,YMIN,YMAX,ZMIN,ZMAX,TYMAX,TXMAX,TZMAX,ZSCAL
      CHARACTER*(*) XLAB,YLAB,ZLAB
      INTEGER NX,NXMIN,NY,NYMIN
C----------------------------------------------------------------------
C-  Local Declaration:
C-  ------------------
      INTEGER SEGNUM
      REAL XMED,YMED,ZMED,CXMED,CYMED,CZMED
      REAL XSIZE,YSIZE ! Size of characters
      REAL XX,YY,ZZ ! Position of label in world coord.
      REAL U0,V0 ! Position of corner or mid-point of grid in virtual coor.
      REAL UU,VV ! Position of label in virtual coor.
      REAL X,Y
      REAL STEP
      INTEGER NTEST
      CHARACTER*8 CX
      CHARACTER*6 CZ
      DATA NTEST/0/
C----------------------------------------------------------------------
C  CALCULATING MED VALUES FOR LABELS AND COORDENATES
      CYMED=(YMAX+YMIN)/2.
      YMED=NY/2.
      CXMED=(XMAX+XMIN)/2.
      IF (CXMED .EQ. 32.5) CXMED=32.
      XMED=NX/2.
      ZMED=(ZMAX+ZMIN)/2.
C  CHECKING FOR SIZE OF THE WINDOW TO DETERMINE SCALE OF PLOT
      CALL PUOPEN
      IF (VMIN.GE.-5.) THEN
        XSIZE=0.35
        YSIZE=0.09
        STEP=.7
      ELSEIF (VMIN.LT.-5..AND.VMIN.GE.-10.) THEN
        XSIZE=0.6
        YSIZE=0.15
        STEP=.75
      ELSE
        XSIZE=1.5
        YSIZE=.5
        STEP=1.
      ENDIF
      X=XSIZE
      Y=YSIZE
C  LABELING
      CALL JSIZE(XSIZE,YSIZE)
      CALL JFONT(5)
C  X AXIS LABELING
      CALL JBASE(-1.,0.,0.)
      CALL JPLANE(0.,0.,1.)
      CALL PXFTOC(XMIN,CX)
      CALL JJUST(3,2)
      XX=1.0
      YY=FLOAT(NY+1)+STEP
      ZZ=0.
      CALL J3MOVE(XX,YY,ZZ)
      CALL J3STRG(CX)
      CALL PXFTOC(XMAX,CX)
      XX=FLOAT(NX+1)+8.*STEP
      YY=FLOAT(NY+1)+STEP
      CALL J3MOVE(XX,YY,.0)
      CALL JJUST(1,2)
      CALL J3STRG(CX)
      IF(NX.GT.7)THEN
        XX=XMED+4.*STEP
        YY=FLOAT(NY+1)+STEP
        CALL J3MOVE(XX,YY,.0)
        CALL PXFTOC(CXMED,CX)
        CALL JJUST(2,2)
        CALL J3STRG(CX)
      ENDIF
      YY=YY + 5.*STEP
      XX=0.4*XMED
      CALL J3MOVE(XX,YY,0.)
      CALL J3STRG(XLAB)
C-
C---  Y AXIS LABELING
C-
      CALL JBASE(0.,-1.,0.)
      CALL JPLANE(0.,0.,1.)
      IF (YLAB .EQ. 'EM LAYER') THEN
        XX=-1.*STEP
        YY=FLOAT(NY+1)+STEP
        CALL J3MOVE(XX,YY,.0)
        CALL JJUST(2,2)
        CALL J3STRG('EM-1')
        YY=FLOAT(NY+1)*.7 + STEP
        CALL J3MOVE(XX,YY,.0)
        CALL JJUST(2,2)
        CALL J3STRG('EM-2')
        YY=FLOAT(NY+1)*.45 + STEP
        CALL J3MOVE(XX,YY,.0)
        CALL JJUST(2,2)
        CALL J3STRG('EM-3')
        YY=FLOAT(NY+1)*.2 + STEP
        CALL J3MOVE(XX,YY,.0)
        CALL JJUST(2,2)
        CALL J3STRG('EM-4')
      ELSE
        CALL PXFTOC(YMAX,CX)
        XX=-1.*STEP
        YY=FLOAT(NY+1)+STEP
        CALL J3MOVE(XX,YY,.0)
        CALL JJUST(2,2)
        CALL J3STRG(CX)
        IF(NY.GT.7)THEN
          CALL PXFTOC(CYMED,CX)
          XX=-1.*STEP
          YY=YMED+STEP*6.
          CALL J3MOVE(XX,YY,.0)
          CALL JJUST(2,2)
          CALL J3STRG(CX)
        ENDIF
        CALL PXFTOC(YMIN,CX)
        XX=-1.*STEP
        YY=.2*YMED+6.*STEP
        CALL J3MOVE(XX,YY,.0)
        CALL JJUST(1,2)
        CALL J3STRG(CX)
      ENDIF
      XX=-5.*STEP
      YY= YMED + 3.*STEP
      CALL J3MOVE(XX,YY,0.)
      CALL J3STRG(YLAB)
C-
C--- |IETA|=12 Label
C-
      IF (YMAX.EQ.37.) THEN
        CALL J3MOVE(-1.,29.,.0)
        CALL JJUST(1,2)
        CALL J3STRG('-12.0')
        CALL J3MOVE(-1.,52.,.0)
        CALL JJUST(1,2)
        CALL J3STRG('12.0')
      ELSEIF (YMAX.EQ.3.7) THEN
        CALL J3MOVE(-1.,29.,.0)
        CALL JJUST(1,2)
        CALL J3STRG('-1.2')
        CALL J3MOVE(-1.,52.,.0)
        CALL JJUST(1,2)
        CALL J3STRG('1.2')
      ENDIF
C-
C--  Z AXIS LABELING
C-
      CALL PXFTOC(ZMAX,CZ)
      CALL J3MOVE(FLOAT(NXMIN)-.28,FLOAT(NYMIN),ZMAX/(ZMAX*ZSCAL))
      CALL J3STRG(CZ)
      CALL PXFTOC(ZMED,CZ)
      CALL J3MOVE(FLOAT(NXMIN)-.28,FLOAT(NYMIN),ZMED/(ZMAX*ZSCAL))
      CALL J3STRG(CZ)
      CALL JSIZE(X*.55,Y*3.33)
      CALL JBASE(0.,0.,1.)
      CALL JPLANE(1.,0.,0.)
      IF (YLAB .EQ. 'EM LAYER') THEN
        CALL J3MOVE(FLOAT(NXMIN)+STEP/2., FLOAT(NYMIN),
     &              6.*ZMED/(ZMAX*ZSCAL))
      ELSE
        CALL J3MOVE(FLOAT(NXMIN)+STEP/.7, FLOAT(NYMIN),
     &              .5*ZMED/(ZMAX*ZSCAL))
      ENDIF
      CALL J3STRG(ZLAB)
  900 CALL JRCLOS
      CALL JUPDAT
  999 RETURN
      END
