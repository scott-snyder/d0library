      SUBROUTINE PFTTRK_ROAD( HALF, QUAD, SECTOR,
     &                    XCEN, YCEN, DX, DY, XSCALE, YSCALE) 
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Draw the ROADS in a given theta sector.
C-
C-   Inputs  : HALF, QUAD, SECTOR = FDC sector identifier
C-             XCEN, YCEN = x,y location of center of displayed sector
C-             DX, DY     = half-thickness of cell in x and y
C-             XSCALE, YSCALE = converts centimeters to world coordinates
C-   Outputs : draws road for sector using a graphic line
C-
C-   Created   2-OCT-1992   Robert E. Avery
C-   Updated  16-AUG-1993   Robert E. Avery  Remove use of LUSER bank. 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:FLOCAL.INC/LIST'
      INCLUDE 'D0$PARAMS:FDPARA.PARAMS'
C INPUT:
      INTEGER HALF, QUAD, SECTOR
      REAL    XCEN, YCEN, DX, DY, XSCALE, YSCALE
C LOCAL:
      INTEGER LFDTQ
      INTEGER LAYER,MODULE
      INTEGER TRK_HALF 
      INTEGER DRAW_PERP
      INTEGER NPARAM
      INTEGER OFFSET 
      INTEGER LEN,II,JJ
      INTEGER QD_B,QD_E,SECT_B,SECT_E
      INTEGER CROAD
      INTEGER LFTRH 
C
      LOGICAL OK
      LOGICAL FIRST
C
      REAL    Y_WIRE_B ,Y_WIRE_E
      REAL    X_WORLD_B, Y_WORLD_B 
      REAL    X_WORLD_E, Y_WORLD_E 
      REAL    X_REAL_B, Y_REAL_B, Z_REAL_B 
      REAL    X_REAL_E, Y_REAL_E, Z_REAL_E
      REAL    XNOM
      REAL    X_TRK,Y_TRK               ! Position of track
      REAL    DX_TRK,DY_TRK             ! Slope of track
      REAL    X_ROT,Y_ROT               ! Position of track in cell coords.
      REAL    DX_ROT,DY_ROT             ! Slope of track in cell coords.
      REAL    X_SLOPE,X_DRIFT
      REAL    STAGGER
      REAL    X_DIR,Z_DIR
      REAL    Z_VTX 
      REAL    Z0(0:1)
      REAL    ZW_B(0:1),ZW_E(0:1)
      REAL    XPOS,YPOS
C
      REAL    PERP_POSITION 
      PARAMETER( PERP_POSITION  = 5.0 )
C
      REAL    TSIZE
      PARAMETER( TSIZE = 0.2 )          ! WORLD, Size of tick for 
C                                       ! Segment on Track marker
      REAL    Y_EXT_TRK
      PARAMETER( Y_EXT_TRK =  0.5 )       ! CM, Extension For Full Track
C
      CHARACTER*4 ROADCLR
      CHARACTER*4 TEXT
C
C
C  FUNCTIONS:
      REAL    FSTAGR
      INTEGER FDC_QUADTYPE
      INTEGER GZFDTA,GZFDTB
      INTEGER GZFTRH
C
      SAVE FIRST
      DATA FIRST /.TRUE./
      DATA ROADCLR /'RED'/
C----------------------------------------------------------------------
C
      IF ( FIRST ) THEN
        LFTRH = GZFTRH()
        IF ( LFTRH .LE. 0 ) GOTO 999
        Z0(0)= Q(LFTRH+3)
        Z0(1)= Q(LFTRH+4)
        CALL GTFALH(
     &              1,0,0,0,0,
     &              XPOS,YPOS,ZW_B(0))
        CALL GTFALH(
     &              1,0,0,0,7,
     &              XPOS,YPOS,ZW_E(0))
        CALL GTFALH(
     &              1,0,4,0,0,
     &              XPOS,YPOS,ZW_B(1))
        CALL GTFALH(
     &              1,0,4,0,7,
     &              XPOS,YPOS,ZW_E(1))
        CALL UCTOH('ROAD',CROAD,4,4)
        FIRST = .FALSE.
      ENDIF
C
      IF ( LFLOC.LE.0 ) GOTO 999
      IF ( IQ(LFLOC-4) .NE. CROAD ) GOTO 999
C
      CALL PUGETV('FDC DRAW PERP',DRAW_PERP)
C
      LAYER = QUAD/4
      MODULE = 3*HALF+LAYER
C
      IF ( FDC_QUADTYPE(QUAD,HALF) .EQ. 1 ) THEN
        LFDTQ = GZFDTA()
        OFFSET = 45
      ELSE
        LFDTQ = GZFDTB()
        OFFSET = 46
      ENDIF
      XNOM  = C ( LFDTQ + 30 + SECTOR )
      Y_WIRE_B = C( LFDTQ+6+0)          ! Wire 0 at top of plot
      Y_WIRE_E = C( LFDTQ+6+7)          ! Wire 7 at bottom of plot
      NPARAM = IC( LFDTQ+3)          
      STAGGER=FSTAGR(HALF, 0, QUAD, SECTOR, 0)
      Z_DIR = -((-1)**LAYER) * ((-1)**HALF)
C
C ****  Draw road
C
C      CALL PUGETA('FDC COLR ROAD',ROADCLR)
      ROADCLR = 'RED'
      CALL PXCOLR(ROADCLR)
      CALL JLSTYL(1)
      CALL JSIZE(0.75,1.0)
C
      TRK_HALF = Q(LFLOC+1)
      IF (  HALF .NE. TRK_HALF ) GOTO 999
C          
      Z_VTX  = Q(LFLOC+2) 
      DX_TRK = Q(LFLOC+3) 
      DY_TRK = Q(LFLOC+4) 
      X_TRK = ( Z0(HALF) - Z_VTX ) * DX_TRK 
      Y_TRK = ( Z0(HALF) - Z_VTX ) * DY_TRK 
C
C Check that ROAD is in cell:
      XPOS = X_TRK + DX_TRK * (Z0(1)-ZW_B(LAYER)) * (-1)**HALF
      YPOS = Y_TRK + DY_TRK * (Z0(1)-ZW_B(LAYER)) * (-1)**HALF
      CALL FGET_SECTOR(XPOS,YPOS,HALF,LAYER,QD_B,SECT_B)
      XPOS = X_TRK + DX_TRK * (Z0(1)-ZW_E(LAYER)) * (-1)**HALF
      YPOS = Y_TRK + DY_TRK * (Z0(1)-ZW_E(LAYER)) * (-1)**HALF
      CALL FGET_SECTOR(XPOS,YPOS,HALF,LAYER,QD_E,SECT_E)
C
      IF (    ((QUAD.NE.QD_B).OR.(SECTOR.NE.SECT_B)) 
     &  .AND. ((QUAD.NE.QD_E).OR.(SECTOR.NE.SECT_E))  ) GOTO 999
C
      CALL FDC_CELL(
     &              HALF, 0, QUAD, SECTOR, 0,
     &              X_TRK,Y_TRK ,DX_TRK ,DY_TRK,
     &              X_ROT,Y_ROT ,DX_ROT ,DY_ROT)
      X_DRIFT = X_ROT + STAGGER
      X_SLOPE = DX_ROT * Z_DIR 
C
      Y_REAL_B = (Y_WIRE_B + Y_EXT_TRK)
      Y_REAL_E = (Y_WIRE_E - Y_EXT_TRK)
      X_REAL_B = X_DRIFT + X_SLOPE * (Y_REAL_B - Y_WIRE_B)  
      X_REAL_E = X_DRIFT + X_SLOPE * (Y_REAL_E - Y_WIRE_B)  
C
      X_WORLD_B = XCEN + XSCALE * (XNOM + X_REAL_B)
      X_WORLD_E = XCEN + XSCALE * (XNOM + X_REAL_E)
      Y_WORLD_B = YCEN + YSCALE * Y_REAL_B 
      Y_WORLD_E = YCEN + YSCALE * Y_REAL_E 
C
      CALL JMOVE(X_WORLD_B, Y_WORLD_B)
      CALL JDRAW(X_WORLD_E, Y_WORLD_E)
C
      IF ( DRAW_PERP .GT. 0) THEN
        X_WORLD_B = XCEN + (XSCALE/10.)*Y_ROT
        Y_WORLD_B = YCEN - DY - PERP_POSITION - 2.0
        CALL JMOVE(X_WORLD_B, Y_WORLD_B)
        CALL J2STRG('R')
      ENDIF
C----------------------------------------------------------------------
  999 CONTINUE
      CALL JLSTYL(0)
      RETURN
      END
