      SUBROUTINE PFPWIR ( HALF, SECTOR, XCEN, YCEN, DX, DY, 
     &                    XSCALE, YSCALE ) 
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Draw the wires in a given phi sector
C-
C-   Inputs  : HALF, SECTOR = FDC Half, Sector
C-             XCEN, YCEN = x,y location of center of displayed sector
C-             DX, DY     = hlaf-thickness of cell in x and y
C-             XSCALE, YSCALE = world coordinates from centimeters conversion
C-   Outputs : draws wires for sector using a "+"
C-   Controls: 
C-
C-   Created  21-OCT-1988   Jeffrey Bantly
C-   Updated  20-FEB-1991   Lupe Howell  Implementing PIXIE using COMPACK 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INTEGER HALF, SECTOR, WIRE
      INTEGER LFDPH, GZFDPH, IER
      REAL    XCEN, YCEN, DX, DY, XSCALE, YSCALE
      REAL    XPOS, YPOS, YNOM, STAGGR, FSTAGR
      LOGICAL EZERROR
      CHARACTER*4 TMPCLR
C----------------------------------------------------------------------
      LFDPH = GZFDPH()
      IF( LFDPH .LE. 5 ) GO TO 999
      YNOM  = 0.
      CALL JSIZE(.5,1.)
      CALL JJUST(2,2)
      CALL EZPICK('PX_FDCDIS_RCP')
      IF ( EZERROR(IER) ) THEN
        CALL ERRMSG('PIXIE','PFPWIR','Cannot find PX_FDCDIS_RCP','W')
        GOTO 999
      ENDIF
      CALL PUGETA('FDC COLR SECTOR',TMPCLR)
      CALL EZRSET
      CALL PXCOLR(TMPCLR)
      DO 10 WIRE = 0, IC(LFDPH+2)-1                      ! 16 WIRES
        XPOS = XCEN + C( LFDPH + 6 + WIRE ) * XSCALE
        STAGGR = FSTAGR(HALF,1,0,SECTOR,WIRE)
        YPOS = YCEN + (YNOM - STAGGR) * YSCALE
        CALL JMOVE(XPOS, YPOS)
        CALL J3STRG('+')
   10 CONTINUE
C----------------------------------------------------------------------
  999 RETURN
      END
