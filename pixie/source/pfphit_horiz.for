      SUBROUTINE PFPHIT_HORIZ( HALF, SECTOR, 
     &                    XCEN, YCEN, DX, DY, XSCALE, YSCALE ) 
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Draw the hits in a given phi sector,
C-     drift direction is horizontal.
C-
C-   Inputs  : HALF, SECTOR = FDC sector identifiers
C-             XCEN, YCEN = x,y location of center of displayed sector
C-             DX, DY     = half-thickness of cell in x and y
C-             XSCALE, YSCALE = converts centimeters to world coordinates
C-   Outputs : draws hits for sector using an "." for real hits and
C-             a "." for any reflected images. If hit is on a segment,
C-             draw an "X".
C-   Controls: 
C-
C-   Created  29-APR-1991   Robert E. Avery, based on pftwir, by
C-      Jeffrey Bantly
C-   Updated  11-OCT-1991   Robert E. Avery   Use hits from FHIT if necessary.
C-   Updated  30-MAR-1992   Robert E. Avery  Change colors, use different
C-      color for hits on tracks.
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$PARAMS:FDPARA.PARAMS'
C
      INTEGER HALF, SECTOR, WIRE
      INTEGER LFDPH, IHIT
      INTEGER GZFDPH
      INTEGER N_HITS(0:MXWIRP)
C
      REAL    DR_DIST(0:1, MX_HIT_WIRE, 0:MXWIRP)
      REAL    XHITP, XHITN, DRIFTP, DRIFTN
      REAL    XCEN, YCEN, DX, DY, XSCALE, YSCALE
      REAL    XPOS, YPOS
C
      LOGICAL DL_OK(MX_HIT_WIRE, 0:MXWIRP)
      LOGICAL ON_SEG(MX_HIT_WIRE, 0:MXWIRP)
      LOGICAL OK, PFGETHITS
C
      REAL    CSIZE
      PARAMETER( CSIZE = 0.16 )         ! Symbol size in CM
      CHARACTER*4 HITCLR,TKHITCLR
C----------------------------------------------------------------------
      CALL JPINTR(1)
      CALL EZPICK('PX_FDCDIS_RCP')
      CALL PUGETA('FDC COLR HITS',HITCLR)
      CALL PUGETA('FDC COLR TKHITS',TKHITCLR)
      CALL EZRSET
C
      OK = PFGETHITS(HALF,1,0,SECTOR,N_HITS,DR_DIST,DL_OK,ON_SEG)
      IF( .NOT. OK) GOTO 999
      LFDPH = GZFDPH()
      DO 10 WIRE = 0, MXWIRP
        XPOS = XCEN 
        YPOS = YCEN + YSCALE * (-1) * C( LFDPH+6+WIRE ) 
        DO 20 IHIT = 1, N_HITS(WIRE)
          DRIFTP = DR_DIST(0,IHIT,WIRE)
          DRIFTN = DR_DIST(1,IHIT,WIRE)
          XHITP  = XPOS + DRIFTP * XSCALE 
          XHITN  = XPOS + DRIFTN * XSCALE 
          CALL PXCOLR(HITCLR)
          CALL JCIRCL(XHITP , YPOS, 0.0 , 0.05 , 0)
          CALL JCIRCL(XHITN , YPOS, 0.0 , 0.05 , 0)
          IF ( ON_SEG(IHIT,WIRE) ) THEN
            CALL PXCOLR(TKHITCLR)
            CALL JSIZE(XSCALE*CSIZE,YSCALE*CSIZE)
            CALL JJUST(2,2)
            CALL JMOVE(XHITP , YPOS)
            CALL J3STRG('X')
            CALL JMOVE(XHITN , YPOS)
            CALL J3STRG('X')
          END IF
   20   CONTINUE
   10 CONTINUE
      CALL JPINTR(0)
C----------------------------------------------------------------------
  999 RETURN
      END
