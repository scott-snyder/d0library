      SUBROUTINE PFTTRK ( HALF, QUAD, SECTOR, 
     &                    XCEN, YCEN, DX, DY, XSCALE, YSCALE ) 
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Draw the track segments in a given theta sector.
C-                      Based on PFTTRK_HORIZ.FOR
C-
C-   Inputs  : HALF, QUAD, SECTOR = FDC sector identifier
C-             XCEN, YCEN = x,y location of center of displayed sector
C-             DX, DY     = hlaf-thickness of cell in x and y
C-             XSCALE, YSCALE = converts centimeters to world coordinates
C-   Outputs : draws tracks for sector using a graphic line
C-   
C-   Created  19-JUN-1992   Robert E. Avery
C-   Updated   9-JUL-1992   Robert E. Avery  Change line styles.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZUSER.LINK/LIST'
      INCLUDE 'D0$PARAMS:FDPARA.PARAMS'
C INPUT:
      INTEGER HALF, QUAD, SECTOR
      REAL    XCEN, YCEN, DX, DY, XSCALE, YSCALE
C LOCAL:
      INTEGER GZFDTA,GZFDTB,LFDTQ
      INTEGER GZFSEG,LFSEG
      INTEGER LSEGM 
      INTEGER NSEG,SEG
      INTEGER LAYER,MODULE
      INTEGER DRAW_TRACK 
      INTEGER H,U,QD,S,W,UB
      INTEGER STATUS 
      INTEGER IADD 
      INTEGER SECT_XSECT 
      INTEGER HIT_XSECT
C
      LOGICAL OK
      LOGICAL DL_FOUND
C
      REAL    X_WIRE_B ,X_WIRE_E
      REAL    X_WORLD_B, Y_WORLD_B 
      REAL    X_WORLD_E, Y_WORLD_E 
      REAL    X_REAL_B, Y_REAL_B
      REAL    X_REAL_E, Y_REAL_E
      REAL    YNOM
      REAL    X_SLOPE,X_DRIFT,Y_DL
      REAL    STAGGER
      REAL    Z_DIR

C
      REAL    X_EXT_SEG
      PARAMETER( X_EXT_SEG =  0.1 )       ! CM, Extension For Segment
C
      CHARACTER*4 SEGCLR,TRKCLR
C
C  FUNCTIONS:
      INTEGER NZBANK,LZFIND
      REAL    FSTAGR
      INTEGER FDC_QUADTYPE
C
C----------------------------------------------------------------------
C
C
      IF (QUAD.GE.4) THEN
        LAYER = 1
      ELSE
        LAYER = 0
      ENDIF
      MODULE = 3*HALF+LAYER
C
      LFDTQ = GZFDTA()
      YNOM  = C ( LFDTQ + 30 + SECTOR )
      X_WIRE_B = C( LFDTQ+6+0)          ! Wire 0 
      X_WIRE_E = C( LFDTQ+6+7)          ! Wire 7 
      STAGGER=FSTAGR(HALF, 0, QUAD, SECTOR, 0)
      Z_DIR = -((-1)**LAYER) * ((-1)**HALF)
C
C ****  Draw Segments
C
      CALL PUGETV('FDC DRAW TSEG',DRAW_TRACK)
      IF ( DRAW_TRACK .GT. 0) THEN
        CALL PUGETA('FDC COLR SEGMENT',SEGCLR)
        CALL PUGETA('FDC COLR TRACK',TRKCLR)
C
        LFSEG = GZFSEG(HALF,LAYER)
        IF (LFSEG .GT.0) THEN
          NSEG = NZBANK(IXCOM,LFSEG)
        ELSE
          NSEG = 0
        ENDIF
C
        DO SEG = 1, NSEG
          LSEGM = LZFIND(IXCOM,LFSEG,SEG,-5)
          STATUS = IQ(LSEGM + 0)
          HIT_XSECT = IQ(LSEGM + 1)/1000 
          IADD = IQ(LSEGM + 2)
          CALL FCODER(IADD,H,U,QD,S,W,UB,1)
          IF ( ABS(HIT_XSECT) .GT. 0 ) THEN
            SECT_XSECT = S + SIGN(1,HIT_XSECT)
          ELSE 
            SECT_XSECT = -1
          ENDIF
C
          OK= .FALSE.
          IF ( QD .EQ. QUAD) THEN
            IF  ( S  .EQ. SECTOR ) THEN
              CALL FDC_SEGMENT(MODULE,SEG,
     &                 X_SLOPE,X_DRIFT,Y_DL,DL_FOUND,OK)
            ELSEIF ( SECT_XSECT  .EQ. SECTOR ) THEN
              CALL FDC_SEG_XSECT(MODULE,SEG,
     &                 X_SLOPE,X_DRIFT,Y_DL,DL_FOUND,OK)
            ENDIF
          ENDIF
C
          IF ( OK ) THEN
            X_DRIFT = X_DRIFT + STAGGER
            X_SLOPE = X_SLOPE * Z_DIR 
C
            X_REAL_B = (X_WIRE_B + X_EXT_SEG)
            X_REAL_E = (X_WIRE_E - X_EXT_SEG)
            Y_REAL_B = X_DRIFT + X_SLOPE * (X_REAL_B - X_WIRE_B )  
            Y_REAL_E = X_DRIFT + X_SLOPE * (X_REAL_E - X_WIRE_B )  
C
            Y_WORLD_B = YCEN - YSCALE * (YNOM + Y_REAL_B)
            Y_WORLD_E = YCEN - YSCALE * (YNOM + Y_REAL_E)
            X_WORLD_B = XCEN + XSCALE * X_REAL_B * (-1)**LAYER 
            X_WORLD_E = XCEN + XSCALE * X_REAL_E * (-1)**LAYER 
C
            IF (BTEST(STATUS,IUSED)) THEN
              CALL PXCOLR(TRKCLR)
              CALL JLSTYL(0)
            ELSE
              CALL PXCOLR(SEGCLR)
              CALL JLSTYL(2)
            ENDIF
            CALL JMOVE(X_WORLD_B, Y_WORLD_B)
            CALL JDRAW(X_WORLD_E, Y_WORLD_E)
C
          ENDIF
        END DO
      ENDIF
      CALL JLSTYL(0)
C----------------------------------------------------------------------
  999 RETURN
      END
