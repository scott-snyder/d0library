      SUBROUTINE PLGRID(NXMIN,NX,NYMIN,NY,IXG,IYG,XMIN,XMAX,
     X             YMIN,YMAX)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Print the grid for a Lego plot and the up axis
C-
C-   Inputs  : NXMIN -  Min num index in the array that will be display (X dir)
C-             NX    -  Maxnum index in the array that will be display (xdir)
C-             NYMIN -  Min num index in the array that will be display (Y dir)
C-             NY    -  Maxnum index in the array that will be display (xdir)
C-             IXG   -  IF.eq.0, all x-grid lines drawn,IF.EQ.1, may skip some
C-             IYG   -  IF.eq.0, all Y-grid lines drawn,IF.EQ.1, may skip some
C-                      (If number of grid lines.GT.20, does every 5th, every
C-                       8th, or every 10th line, depending on TOT number.)
C-             XMIN  -  Minimum value for x (axis label)
C-             XMAX  -  Maximum value foR X (axis label)
C-             YMIN  -  Minimum value for Y (axis label)
C-             YMAX  -  Maximum value for Y (axis label)
C-
C-   Local variables:
C-             TOTX  -  Total of lines in the X direction (real lines)
C-             TOTY  -  Total of lines in the Y direction
C-             XMOD  -  Spacing of drawn X-grid lines in grid units
C-             YMOD  -  Spacing of drawn Y-grid lines in grid units
C-             NI1   -  Var use to reduce no. of grid lines drawn
C-             NI2   -  Var use to reduce no. of grid lines drawn
C-             IETA  -  Var use for loops
C-             IPHI  -  Var use for loops
C-             MAX   -  Total of lines to draw
C-             TMPX  -  Temporary var use to store the X coor to draw
C-             TMPY  -  Temporary var use to store the Y coor to draw
C-
C-   Created  28-JUN-1988   LUPE ROSAS
C-   Updated  19-DEC-1989   Lupe Rosas Set up to be able to use color table
C-
C----------------------------------------------------------------------
      IMPLICIT NONE

C- Argument declaration:
C- ---------------------
      INTEGER NXMIN,NX,NYMIN,NY,IXG,IYG
      REAL XMIN,XMAX,YMIN,YMAX

C- Local Declaration:
C- ------------------
      INTEGER IETA,IPHI,NI1,NI2,MAX,TOTX,TOTY
      REAL XMOD,YMOD
      REAL TMPX,TMPY
C----------------------------------------------------------------------
      TOTX = NX-NXMIN+1
      TOTY = NY-NYMIN+1
      XMOD=1.
      YMOD=1.
      IF (IYG.GT.0.AND.TOTY.GT.20.) THEN
        NI1=MOD(TOTY,5)
        NI2=MOD(TOTY,8)
        IF (NI1.EQ.0) THEN
          YMOD=5.
        ELSEIF (NI2.EQ.0) THEN
          YMOD=8.
        ELSE
          YMOD=FLOAT(TOTY)/10.
        ENDIF
      ENDIF
      IF (IXG.GT.0.AND.TOTX.GT.20.) THEN
        NI1=MOD(TOTX,5)
        NI2=MOD(TOTX,8)
        IF (NI1.EQ.0) THEN
          XMOD=5.
        ELSEIF (NI2.EQ.0) THEN
          XMOD=8.
        ELSE
          XMOD=FLOAT(TOTX)/10.
        ENDIF
      ENDIF
      CALL PUOPEN
      CALL JPINTR(0)
C-  DRAWING THE HORIZONTAL LINES OF THE GRID
      MAX = FLOAT(TOTY)/YMOD  +1.
      DO 10 IPHI = 1, MAX
        TMPY=FLOAT(NYMIN)+(IPHI-1)*YMOD
        CALL J3MOVE(FLOAT(NX+1),TMPY,0.)
        CALL J3DRAW(FLOAT(NXMIN),TMPY,0.)
   10 CONTINUE
      IF (MAX .EQ. 11) THEN
        CALL PXCOLR('GRE')
        CALL J3MOVE(FLOAT(NX+1), 26.,0.)
        CALL J3DRAW(FLOAT(NXMIN),26.,0.)
        CALL J3MOVE(FLOAT(NX+1), 50.,0.)
        CALL J3DRAW(FLOAT(NXMIN),50.,0.)
        CALL PXCOLR('FOR')
      ENDIF
C-  DRAWING THE VERTICAL LINES OF THE GRID
      MAX = FLOAT(TOTX)/XMOD +1.
      DO 20 IETA=1, MAX
        TMPX=FLOAT(NXMIN)+(IETA-1)*XMOD
        CALL J3MOVE(TMPX,FLOAT(NYMIN),0.)
        CALL J3DRAW(TMPX,FLOAT(NY+1),0.)
   20 CONTINUE
      IF (MAX .EQ. 9) THEN
        CALL PXCOLR('YEL')
        CALL J3MOVE(17.6,FLOAT(NYMIN),0.)
        CALL J3DRAW(17.6,FLOAT(NY+1),0.)
        CALL PXCOLR('FOR')
      ENDIF
C-
      CALL JRCLOS
  999 RETURN
      END
