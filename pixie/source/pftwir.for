      SUBROUTINE PFTWIR ( HALF, QUAD, SECTOR,
     &                    XCEN, YCEN, DX, DY, XSCALE, YSCALE )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Draw the wires in a given theta sector
C-
C-   Inputs  : HALF, QUAD, SECTOR = FDC sector identifier
C-             XCEN, YCEN = x,y location of center of displayed sector
C-             DX, DY     = half-thickness of cell in x and y
C-   Outputs : draws wires for sector using a "+"
C-   Controls:
C-
C-   Created  21-OCT-1988   Jeffrey Bantly
C-   Updated  10-JAN-1990   Lupe Howell  Implemented Color Table
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INTEGER HALF, QUAD, SECTOR, WIRE, DUM, LAYER
      INTEGER LKFDTQ, GZFDTA, GZFDTB
      REAL    XCEN, YCEN, DX, DY, XSCALE, YSCALE
      REAL    XPOS, YPOS, YNOM, STAGGR, FSTAGR
      CHARACTER*4 SECCLR
      CHARACTER*4 REM
      INTEGER TYP,IVAL,IER
      LOGICAL EZERROR
C----------------------------------------------------------------------
      IF(QUAD.EQ.0 .OR. QUAD.EQ.2 .OR. QUAD.EQ.4 .OR. QUAD.EQ.6) THEN
        LKFDTQ = GZFDTA()
      ELSE
        LKFDTQ = GZFDTB()
      ENDIF
      IF( LKFDTQ .LE. 5 ) GO TO 999
      LKFDTQ = LKFDTQ + 5
      YNOM  = C ( LKFDTQ + 25 + SECTOR )
      CALL JSIZE(.5,1.)
      CALL JJUST(2,2)
C
C ****  Pick correct RCP bank
C
      CALL EZPICK('PX_FDCDIS_RCP')
      IF ( EZERROR(IER) ) THEN
        CALL ERRMSG('PIXIE','PFTWIR','Cannot find PX_FDCDIS_RCP','W')
        GOTO 999
      ENDIF
      CALL EZ_GET_ARRAY('PXPARAMS','FDC COLR SECTOR',1,IVAL,
     &       SECCLR,TYP,REM,IER)
      CALL PXCOLR(SECCLR)
      LAYER = 0
      IF(QUAD.GT.3) LAYER = 1
      DO 10 WIRE = 0, 7
        XPOS = XCEN + C( LKFDTQ+1+WIRE ) * XSCALE * (-1)**LAYER
        STAGGR = 0.
        IF( SECTOR .GE. 3 ) STAGGR = FSTAGR(HALF,0,QUAD,SECTOR,WIRE)
        YPOS = YCEN - (YNOM + STAGGR) * YSCALE
        CALL JMOVE(XPOS, YPOS)
        CALL J3STRG('+')
   10 CONTINUE
C----------------------------------------------------------------------
  990 CONTINUE
C
C ****  Reset RCP bank
C
      CALL EZRSET
  999 RETURN
      END
