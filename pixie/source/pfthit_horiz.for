      SUBROUTINE PFTHIT_HORIZ ( HALF, QUAD, SECTOR, 
     &                    XCEN, YCEN, DX, DY, XSCALE, YSCALE ) 
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Draw the hits in a given theta sector,
C-     drift direction is horizontal.
C-
C-   Inputs  : HALF, QUAD, SECTOR = FDC sector identifiers
C-             XCEN, YCEN = x,y location of center of displayed sector
C-             DX, DY     = half-thickness of cell in x and y
C-             XSCALE, YSCALE = converts centimeters to world coordinates
C-   Outputs : draws hits for sector using an "." for real hits and
C-             a "." for any reflected images
C-   Controls: 
C-
C-   Created  29-APR-1991   Robert E. Avery, based on pftwir, by
C-      Jeffrey Bantly
C-   Updated  11-OCT-1991   Robert E. Avery   Use hits from FHIT if necessary.
C-   Updated  30-MAR-1992   Robert E. Avery  Change colors, use different
C-      color for hits on tracks.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$PARAMS:FDPARA.PARAMS'
C
      INTEGER HALF, QUAD, SECTOR, WIRE
      INTEGER LFDTQ, IHIT
      INTEGER GZFDTA
      INTEGER N_HITS(0:MXWIRP)
C
      REAL    DR_DIST(0:1, MX_HIT_WIRE, 0:MXWIRP)
      REAL    XHITP, XHITN, DRIFTP, DRIFTN
      REAL    XCEN, YCEN, DX, DY, XSCALE, YSCALE
      REAL    XPOS, YPOS, XNOM
C
      LOGICAL DL_OK(MX_HIT_WIRE, 0:MXWIRP)
      LOGICAL ON_SEG(MX_HIT_WIRE, 0:MXWIRP)
      LOGICAL OK, PFGETHITS
C
      REAL    CSIZE
      PARAMETER( CSIZE = 0.08 )         ! Symbol size in CM
      CHARACTER*4 HITCLR,TKHITCLR
C----------------------------------------------------------------------
      CALL EZPICK('PX_FDCDIS_RCP')
      CALL PUGETA('FDC COLR HITS',HITCLR)
      CALL PUGETA('FDC COLR TKHITS',TKHITCLR)
      CALL EZRSET
C
      OK = PFGETHITS(HALF,0,QUAD,SECTOR,N_HITS,DR_DIST,DL_OK,ON_SEG)
      IF( .NOT. OK) GOTO 999
      LFDTQ = GZFDTA()
      XNOM  = C ( LFDTQ + 5 + 25 + SECTOR )
C
      DO 10 WIRE = 0, MXWIRT
        XPOS = XCEN + XSCALE * XNOM 
        YPOS = YCEN + YSCALE * C( LFDTQ+6+WIRE ) 
        DO 20 IHIT = 1, N_HITS(WIRE)
          DRIFTP = DR_DIST(0,IHIT,WIRE)
          DRIFTN = DR_DIST(1,IHIT,WIRE)
          IF( SECTOR .EQ. 1 ) THEN
            DRIFTP = DRIFTP * (-1.)
            DRIFTN = DRIFTN * (-1.)
          ENDIF
          XHITP  = XPOS + DRIFTP * XSCALE 
          IF( SECTOR .GT. 2 .OR. DRIFTN.NE.0.0) THEN
             XHITN  = XPOS + DRIFTN * XSCALE 
          ELSE
             XHITN  = XHITP
          ENDIF
          IF ( DL_OK(IHIT,WIRE) ) THEN
            CALL PXCOLR(HITCLR)
            CALL JPINTR(0)
            CALL JCIRCL(XHITP , YPOS, 0.0 , 0.18, 0)
            CALL JCIRCL(XHITN , YPOS, 0.0 , 0.18, 0)
          ELSE
            CALL PXCOLR(HITCLR)
            CALL JPINTR(1)
            CALL JCIRCL(XHITP , YPOS, 0.0 , 0.05 , 0)
            CALL JCIRCL(XHITN , YPOS, 0.0 , 0.05 , 0)
          END IF
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
