      SUBROUTINE PFPTRK_ISA( HALF, SECTOR,
     &                    XCEN, YCEN, DX, DY, XSCALE, YSCALE) 
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Draw the ISAJET tracks in a given phi sector.
C-
C-   Inputs  : HALF, QUAD, SECTOR = FDC sector identifier
C-             XCEN, YCEN = x,y location of center of displayed sector
C-             DX, DY     = half-thickness of cell in x and y
C-             XSCALE, YSCALE = converts centimeters to world coordinates
C-   Outputs : draws tracks for sector using a graphic line
C-   Controls:
C-
C-   Created  26-NOV-1991   Robert E. Avery
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZUSER.LINK/LIST'
      INCLUDE 'D0$PARAMS:FDPARA.PARAMS'
      INCLUDE 'D0$INC:PI.DEF'
C INPUT:
      INTEGER HALF, QUAD, SECTOR
      REAL    XCEN, YCEN, DX, DY, XSCALE, YSCALE
C LOCAL:
      INTEGER LFDPH
      INTEGER LFITR,LFTRH
      INTEGER NTRACK,TRACK
      INTEGER LAYER,MODULE
      INTEGER TRK_HALF 
      INTEGER DRAW_TRACK 
      INTEGER DRAW_PERP
      INTEGER NPARAM
      INTEGER OFFSET 
      INTEGER LEN,II,JJ
      INTEGER QD,SECT0,SECT15
C
      LOGICAL OK
      LOGICAL DRAWN
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
      REAL    LENGTH
      REAL    TRKDAT(9)
      REAL    TAN_THETA, Z_VTX 
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
      CHARACTER*4 TRKCLR
      CHARACTER*4 LABEL
      CHARACTER*4 TEXT
C
C
C  FUNCTIONS:
      REAL    FSTAGR
      INTEGER GZFDPH 
      INTEGER GZFITR,GZFTRH
C
      SAVE Z0,FIRST
      DATA FIRST /.TRUE./
      DATA TRKCLR /'CYA '/
C----------------------------------------------------------------------
C
      CALL PUGETV('FDC DRAW PERP',DRAW_PERP)
      CALL PUGETV('FDC DRAW ISATRK',DRAW_TRACK)
      IF ( DRAW_TRACK .LE. 0) GOTO 999
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
        FIRST = .FALSE.
      ENDIF
C
      LFITR = GZFITR()
      IF ( LFITR .LE. 0 ) GOTO 999
C
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
C ****  Draw Tracks
C
C
      CALL PUGETA('FDC COLR ISAJET',TRKCLR)
      CALL PXCOLR(TRKCLR)
      CALL JLSTYL(1)
      CALL JSIZE(0.75,1.0)
      DRAWN = .FALSE.
C
      NTRACK = IQ( LFITR + 1 )
      DO TRACK =  1, NTRACK
        CALL GTFITR(TRACK, TRKDAT)
        TAN_THETA = TAN(TRKDAT(5)) 
        IF ( TAN_THETA .GT. 0 ) THEN
          TRK_HALF = 1
        ELSE
          TRK_HALF = 0
        ENDIF
        IF (  HALF .EQ. TRK_HALF ) THEN
          DX_TRK = TAN_THETA * COS(TRKDAT(4))
          DY_TRK = TAN_THETA * SIN(TRKDAT(4))
          Z_VTX = TRKDAT(3) 
          X_TRK = TRKDAT(1) - (Z_VTX - Z0(HALF) ) * DX_TRK 
          Y_TRK = TRKDAT(2) - (Z_VTX - Z0(HALF) ) * DY_TRK 
C
C Check that track is in cell:
          XPOS = X_TRK + DX_TRK * Z_DIR * (Z0(1)-ZW0)
          YPOS = Y_TRK + DY_TRK * Z_DIR * (Z0(1)-ZW0)
          CALL FGET_SECTOR(XPOS,YPOS,HALF,LAYER,QD,SECT0)
          XPOS = X_TRK + DX_TRK * Z_DIR * (Z0(1)-ZW15)
          YPOS = Y_TRK + DY_TRK * Z_DIR * (Z0(1)-ZW15)
          CALL FGET_SECTOR(XPOS,YPOS,HALF,LAYER,QD,SECT15)
          IF ( (SECTOR.EQ.SECT0) .OR. (SECTOR.EQ.SECT15) ) THEN
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
            IF(TRKDAT(8) .LE. 9999.) THEN
              WRITE(LABEL,100) INT(TRKDAT(8))
  100         FORMAT(I4)
              CALL SWORDS(LABEL,II,JJ,LEN)
              CALL JJUST(2,2)
              CALL JMOVE(X_WORLD_B, Y_WORLD_B+1.75)
              CALL J2STRG(LABEL(II:JJ))
              CALL JMOVE(X_WORLD_E, Y_WORLD_E-1.75)
              CALL J2STRG(LABEL(II:JJ))
            ENDIF
C
            DRAWN = .TRUE.
          ENDIF
        ENDIF
      END DO
      IF ( DRAWN  ) THEN
        X_WORLD_E = XCEN - DX - 0.75
        CALL JJUST(3,2)
        CALL JMOVE( X_WORLD_E, Y_WORLD_E-1.75)
        CALL J2STRG('Isa Track')
      ENDIF
C----------------------------------------------------------------------
      CALL JLSTYL(0)
  999 RETURN
      END
