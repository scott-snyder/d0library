      SUBROUTINE PFPHIT ( HALF, SECTOR,
     &                    XCEN, YCEN, DX, DY, XSCALE, YSCALE ) 
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Draw the hits in a given phi sector
C-
C-   Inputs  : HALF, SECTOR = FDC Half, Sector being displayed
C-             XCEN, YCEN = x,y location of center of displayed sector
C-             DX, DY     = half-thickness of cell in x and y
C-             XSCALE, YSCALE = converts centimeters to world coordinates
C-   Outputs : draws hits for sector using an "." for real hits and
C-             also "." for any reflected images
C-   Controls: 
C-
C-   Created  21-OCT-1988   Jeffrey Bantly
C-   Updated  14-MAY-1991   Susan K. Blessing  Use North and South halves, 
C-                                    Add EZPICK, EZRSET for PUxETV calls.
C-   Updated  11-OCT-1991   Robert E. Avery   Use hits from FHIT if necessary.
C-   Updated  19-JUN-1992   Robert E. Avery   Mark hits on track segments
C-                              and use different color.
C-     
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:FDPARA.PARAMS'
C
      INTEGER IER
      INTEGER HALF, SECTOR, WIRE
      INTEGER LFDPH, GZFDPH
      INTEGER IHIT
      INTEGER N_HITS(0:MXWIRP)
C
      REAL    DR_DIST(0:1, MX_HIT_WIRE, 0:MXWIRP)
      REAL    YHITP, YHITN, DRIFTP, DRIFTN
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
C
C----------------------------------------------------------------------
C
      CALL EZPICK('PX_FDCDIS_RCP')
      CALL PUGETA('FDC COLR HITS',HITCLR)
      CALL PUGETA('FDC COLR TKHITS',TKHITCLR)
      CALL EZRSET
C
      OK = PFGETHITS(HALF,1,0,SECTOR,N_HITS,DR_DIST,DL_OK,ON_SEG)
      IF( .NOT. OK) GOTO 999
      LFDPH = GZFDPH()
      DO 10 WIRE = 0, MXWIRP
        XPOS = XCEN + XSCALE * C( LFDPH+6+WIRE ) 
        YPOS = YCEN 
        DO 20 IHIT = 1, N_HITS(WIRE)
          DRIFTP = DR_DIST(0,IHIT,WIRE)
          DRIFTN = DR_DIST(1,IHIT,WIRE)
          YHITP  = YPOS - DRIFTP * YSCALE 
          YHITN  = YPOS - DRIFTN * YSCALE 
C
          IF ( ON_SEG(IHIT,WIRE) ) THEN
            CALL PXCOLR(TKHITCLR)
            CALL JSIZE(XSCALE*CSIZE,YSCALE*CSIZE)
            CALL JJUST(2,2)
            CALL JMOVE(XPOS, YHITP)
            CALL J3STRG('X')
            CALL JMOVE(XPOS, YHITN)
            CALL J3STRG('X')
          ELSE
            CALL PXCOLR(HITCLR)
            CALL JCIRCL(XPOS, YHITP, 0.0 , 0.05 , 0)
            CALL JCIRCL(XPOS, YHITN, 0.0 , 0.05 , 0)
          END IF
   20   CONTINUE
   10 CONTINUE
C----------------------------------------------------------------------
  999 RETURN
      END
