      SUBROUTINE PFTWIR_HORIZ ( HALF, QUAD, SECTOR,
     &                    XCEN, YCEN, DX, DY, XSCALE, YSCALE )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Draw the wires in a given theta sector, 
C-     drift direction is horizontal.
C-
C-   Inputs  : HALF, QUAD, SECTOR = FDC sector identifier
C-             XCEN, YCEN = x,y location of center of displayed sector
C-             DX, DY     = half-thickness of cell in x and y
C-   Outputs : draws wires for sector using a "+"
C-   Controls:
C-
C-   Created  29-APR-1991   Robert E. Avery, based on pftwir, by
C-      Jeffrey Bantly
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:PXPARA.INC'
      INTEGER HALF, QUAD, SECTOR, WIRE
      INTEGER LFDTQ, GZFDTA
      REAL    XCEN, YCEN, DX, DY, XSCALE, YSCALE
      REAL    XPOS, YPOS, XNOM, STAGGR, FSTAGR
      REAL    CSIZE,XSIZE,YSIZE
      PARAMETER( CSIZE = 0.3 )         ! Symbol size in world coords.
      CHARACTER*4 SECCLR
      INTEGER IER
C----------------------------------------------------------------------
      LFDTQ = GZFDTA()
      IF( LFDTQ .LE. 5 ) GO TO 999
      XNOM  = C ( LFDTQ + 5 + 25 + SECTOR )
C
      CALL PUGETA('FDC COLR SECTOR',SECCLR)
      CALL PXCOLR(SECCLR)
      CALL JSIZE(0.5,0.75)
      DO 10 WIRE = 0, 7
        STAGGR = FSTAGR(HALF,0,QUAD,SECTOR,WIRE)
        XPOS = XCEN + XSCALE * (XNOM + STAGGR) 
        YPOS = YCEN + YSCALE * C( LFDTQ+6+WIRE ) 
C   Draw a cross:
        XSIZE = YDVMAG*CSIZE 
        YSIZE = XDVMAG*CSIZE 
        CALL JMOVE(XPOS-XSIZE,YPOS)
        CALL JDRAW(XPOS+XSIZE,YPOS)
        CALL JMOVE(XPOS,YPOS-YSIZE)
        CALL JDRAW(XPOS,YPOS+YSIZE)
        IF ( WIRE.EQ.0 ) THEN
          CALL JMOVE(XPOS,YPOS-0.5)
          CALL JJUST(2,3)
          CALL J2STRG('W0')
        ENDIF
   10 CONTINUE
C----------------------------------------------------------------------
  999 RETURN
      END
