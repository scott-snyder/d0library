      SUBROUTINE PFPTRK_ROAD( HALF, SECTOR,
     &                    XCEN, YCEN, DX, DY, XSCALE, YSCALE) 
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Draw the ROADS in a given phi sector.
C-
C-   Inputs  : HALF, SECTOR = FDC sector identifier
C-             XCEN, YCEN = x,y location of center of displayed sector
C-             DX, DY     = half-thickness of cell in x and y
C-             XSCALE, YSCALE = converts centimeters to world coordinates
C-   Outputs : draws tracks for sector using a graphic line
C-   Controls:
C-
C-   Created   4-OCT-1992   Robert E. Avery
C-   Updated  16-AUG-1993   Robert E. Avery  Remove use of LUSER bank. 
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:FLOCAL.INC/LIST'
      INCLUDE 'D0$PARAMS:FDPARA.PARAMS'
C INPUT:
      INTEGER HALF, QUAD, SECTOR
      REAL    XCEN, YCEN, DX, DY, XSCALE, YSCALE
C LOCAL:
      INTEGER LFDPH
      INTEGER LFTRH
      INTEGER NTRACK,TRACK
      INTEGER LAYER,MODULE
      INTEGER TRK_HALF 
      INTEGER NPARAM
      INTEGER OFFSET 
      INTEGER QD,SECT0,SECT15
      INTEGER CROAD
C
      LOGICAL OK
      LOGICAL FIRST
C
      REAL    Y_WIRE_B ,Y_WIRE_E
      REAL    X_WORLD_B, Y_WORLD_B 
      REAL    X_WORLD_E, Y_WORLD_E 
      REAL    X_REAL_B, Y_REAL_B
      REAL    X_REAL_E, Y_REAL_E
      REAL    X_TRK,Y_TRK               ! Position of track
      REAL    DX_TRK,DY_TRK             ! Slope of track
      REAL    X_ROT,Y_ROT               ! Position of track in cell coords.
      REAL    DX_ROT,DY_ROT             ! Slope of track in cell coords.
      REAL    X_SLOPE,X_DRIFT
      REAL    STAGGER
      REAL    X_DIR,Z_DIR
      REAL    Z_VTX 
      REAL    Z0(0:1)
      REAL    ZW0,ZW15
      REAL    XPOS,YPOS
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
      INTEGER GZFDPH 
      INTEGER GZFTRH
C
      SAVE Z0,FIRST
      DATA FIRST /.TRUE./
      DATA ROADCLR /'RED '/
C----------------------------------------------------------------------
C
      IF (FIRST) THEN
        LFTRH = GZFTRH()
        IF ( LFTRH .LE. 0 ) GOTO 999
        Z0(0)= Q(LFTRH+3)
        Z0(1)= Q(LFTRH+4)
        CALL GTFALH(
     &              1,1,0,0,0,
     &              XPOS,YPOS,ZW0)
        CALL GTFALH(
     &              1,1,0,0,15,
     &              XPOS,YPOS,ZW15)
        CALL UCTOH('ROAD',CROAD,4,4)
        FIRST = .FALSE.
      ENDIF
C
      IF ( LFLOC.LE.0 ) GOTO 999
      IF ( IQ(LFLOC-4) .NE. CROAD ) GOTO 999
C
      LAYER = 2
      MODULE = 3*HALF+LAYER
C
      LFDPH = GZFDPH()
      Y_WIRE_B = -C( LFDPH+6+0)          ! Wire 0 at top of plot
      Y_WIRE_E = -C( LFDPH+6+15)          ! Wire 15 at bottom of plot
      STAGGER=FSTAGR(HALF, 1, 0, SECTOR, 0)
      Z_DIR = (-1)**HALF
C
C ****  Draw Road
C
C      CALL PUGETA('FDC COLR ROAD',ROADCLR)
      ROADCLR = 'RED'
      CALL PXCOLR(ROADCLR)
      CALL JLSTYL(1)
      CALL JSIZE(0.75,1.0)
C
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
C Check that Road is in cell:
      XPOS = X_TRK + DX_TRK * Z_DIR * (Z0(1)-ZW0)
      YPOS = Y_TRK + DY_TRK * Z_DIR * (Z0(1)-ZW0)
      CALL FGET_SECTOR(XPOS,YPOS,HALF,LAYER,QD,SECT0)
      XPOS = X_TRK + DX_TRK * Z_DIR * (Z0(1)-ZW15)
      YPOS = Y_TRK + DY_TRK * Z_DIR * (Z0(1)-ZW15)
      CALL FGET_SECTOR(XPOS,YPOS,HALF,LAYER,QD,SECT15)
      IF ( (SECTOR.NE.SECT0) .AND. (SECTOR.NE.SECT15) ) GOTO 999
C
      CALL FDC_CELL(
     &              HALF, 1, 0, SECTOR, 0,
     &              X_TRK,Y_TRK ,DX_TRK ,DY_TRK,
     &              X_ROT,Y_ROT ,DX_ROT ,DY_ROT)
      X_DRIFT = (X_ROT + STAGGER)
      X_SLOPE = DX_ROT * Z_DIR 
C
      Y_REAL_B = (Y_WIRE_B + Y_EXT_TRK)
      Y_REAL_E = (Y_WIRE_E - Y_EXT_TRK)
      X_REAL_B = X_DRIFT + X_SLOPE * (Y_REAL_B - Y_WIRE_B)  
      X_REAL_E = X_DRIFT + X_SLOPE * (Y_REAL_E - Y_WIRE_B)  
C
      X_WORLD_B = XCEN + XSCALE * X_REAL_B
      X_WORLD_E = XCEN + XSCALE * X_REAL_E
      Y_WORLD_B = YCEN + YSCALE * Y_REAL_B 
      Y_WORLD_E = YCEN + YSCALE * Y_REAL_E 
C
      CALL JMOVE(X_WORLD_B, Y_WORLD_B)
      CALL JDRAW(X_WORLD_E, Y_WORLD_E)
C
C----------------------------------------------------------------------
      CALL JLSTYL(0)
  999 RETURN
      END
