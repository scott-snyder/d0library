      SUBROUTINE PFPTRK_HORIZ( HALF, SECTOR,
     &                    XCEN, YCEN, DX, DY, XSCALE, YSCALE)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Draw the tracks in a given phi sector
C-
C-   Inputs  : HALF, SECTOR = FDC sector identifier
C-             XCEN, YCEN = x,y location of center of displayed sector
C-             DX, DY     = hlaf-thickness of cell in x and y
C-             XSCALE, YSCALE = converts centimeters to world coordinates
C-   Outputs : draws tracks for sector using a graphic line
C-   Controls:
C-
C-   Created  26-APR-1991   Robert E. Avery
C-   Updated   9-SEP-1991   Robert E. Avery  Correction in use of
C-                              status word of FDCT track (bit 0 = half).
C-   Updated  17-SEP-1991   Susan K. Blessing  Change size of (I)QTRAK to
C-     26 (two errors and two spares).
C-   Updated   7-NOV-1991   Robert E. Avery  Increase maximum number of
C-     labels to  61 (by using lower case).
C-   Updated  25-JAN-1992   Robert E. Avery  Implement flip segment option
C-      (flipped segment is stored in USER zebra bank. Use FGET_SECTOR for
C-      fiducial checks, so corresponds to what is used in analysis.
C-   Updated   7-FEB-1992   Robert E. Avery  Use track number for label,
C-      if gt  max.
C-   Updated   7-MAY-1993   Robert E. Avery  Add call to FDC_ALIGNCHK
C-   Updated  16-AUG-1993   Robert E. Avery  Remove use of LUSER bank. 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:FLOCAL.INC/LIST'
      INCLUDE 'D0$LINKS:IZFDCT.LINK'
      INCLUDE 'D0$PARAMS:FDPARA.PARAMS'
      INCLUDE 'D0$INC:PI.DEF'
C INPUT:
      INTEGER HALF, SECTOR
      REAL    XCEN, YCEN, DX, DY, XSCALE, YSCALE
C LOCAL:
      INTEGER GZFDPH,LFDPH
      INTEGER GZFSEG,LFSEG
      INTEGER GZFTRH,LFTRH
      INTEGER GZFDCT,LFDCT
      INTEGER LSEGM
      INTEGER NSEG,SEG
      INTEGER TRACK
      INTEGER LAYER,MODULE
      INTEGER LADDER(0:2)
      INTEGER DRAW_TRACK
      INTEGER DRAW_TMEAN
      INTEGER IER
      INTEGER H,U,QD,S,W,UB
      INTEGER SECT0,SECT15
      INTEGER STATUS
      INTEGER IADD
      INTEGER IBANK
      INTEGER SECT_XSECT
      INTEGER HIT_XSECT
      INTEGER LEN,II,JJ
      INTEGER MAX_LABEL
      PARAMETER( MAX_LABEL  =  61)
C
      LOGICAL OK
      LOGICAL DL_FOUND
      LOGICAL DRAWN
      LOGICAL USER_TRACK
      LOGICAL FALH_CHANGED
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
      REAL    X_SLOPE,X_DRIFT,Y_DL
      REAL    STAGGER
      REAL    Z_DIR
      REAL    TMEAN
      REAL    Z0,ZW0,ZW15
      REAL    XPOS,YPOS
C
      REAL    TSIZE
      PARAMETER( TSIZE = 0.2 )          ! WORLD, Size of tick for
C                                       ! Segment on Track marker
      REAL    Y_EXT_SEG
      PARAMETER( Y_EXT_SEG =  0.3 )       ! CM, Extension For Segment
C
      REAL    Y_EXT_TRK
      PARAMETER( Y_EXT_TRK =  1.3 )       ! CM, Extension For Full Track
C
      LOGICAL FIRST
C
      CHARACTER*4 SEGCLR
      CHARACTER*4 TRKCLR
      CHARACTER*4 LABEL
      CHARACTER*1 LABEL_KEY(0:MAX_LABEL)
      CHARACTER*4 TEXT
C
      REAL QTRAK(26),QHTRK(3,34)        ! TRACK INFORMATION
      INTEGER IQTRAK(26)
      EQUIVALENCE (QTRAK,IQTRAK)
C
C  FUNCTIONS:
      INTEGER NZBANK,LZFIND
      REAL    FSTAGR
      LOGICAL FDC_ALIGNCHK
C
      DATA FIRST /.TRUE./
      DATA LABEL_KEY /'0','1','2','3','4','5','6','7','8','9',
     &                'A','B','C','D','E','F','G','H','I','J',
     &                'K','L','M','N','O','P','Q','R','S','T',
     &                'U','V','W','X','Y','Z',
     &                'a','b','c','d','e','f','g','h','i','j',
     &                'k','l','m','n','o','p','q','r','s','t',
     &                'u','v','w','x','y','z'/
C----------------------------------------------------------------------
      FALH_CHANGED = FDC_ALIGNCHK()
      IF ( FIRST .OR. FALH_CHANGED ) THEN
        LFTRH = GZFTRH()
        IF ( LFTRH .LE. 0 ) GOTO 999
        FIRST = .FALSE.
        LFDPH = GZFDPH()
        Y_WIRE_B = -C( LFDPH+6+0)          ! Wire 0 at top of plot
        Y_WIRE_E = -C( LFDPH+6+15)          ! Wire 15 at bottom of plot
        CALL GTFALH(
     &              1,1,0,0,0,
     &              XPOS,YPOS,ZW0)
        CALL GTFALH(
     &              1,1,0,0,15,
     &              XPOS,YPOS,ZW15)
        LFTRH = GZFTRH()
        Z0 = ABS(Q(LFTRH+3))
      ENDIF

C
      CALL JSIZE(0.75,1.0)
      LAYER = 2
      MODULE = 3*HALF+LAYER
C
      STAGGER=FSTAGR(HALF, 1, 0, SECTOR, 0)
      Z_DIR = (-1)**HALF
C
C ****  Draw Segments
C
      CALL PUGETV('FDC DRAW TSEG',DRAW_TRACK)
      IF ( DRAW_TRACK .GT. 0) THEN
        CALL PUGETA('FDC COLR SEGMENT',SEGCLR)
        CALL PXCOLR(SEGCLR)
C
        DRAWN = .FALSE.
        LFSEG = GZFSEG(HALF,LAYER)
        IF (LFSEG .GT.0) THEN
          NSEG = NZBANK(IXCOM,LFSEG)
        ELSE
          NSEG = 0
        ENDIF
C
        USER_TRACK = .FALSE.
        CALL UCTOH('FSEG',IBANK,4,4)
        IF ( LFLOC.GT.0 ) THEN
          IF ( IQ(LFLOC-4) .EQ. IBANK ) THEN        ! Draw segment in user bank
            NSEG = NSEG + 1
            USER_TRACK = .TRUE.
          ENDIF
        ENDIF
C
        DO SEG = 1, NSEG
          IF ( USER_TRACK.AND.(SEG.EQ.NSEG) ) THEN
            X_DRIFT = Q(LFLOC+1)
            X_SLOPE = Q(LFLOC+2)
            STATUS = 0
            OK = .TRUE.
            LABEL = 'F'
            CALL JLSTYL(1)
          ELSE
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
            OK= .FALSE.
            IF  ( S  .EQ. SECTOR ) THEN
              CALL FDC_SEGMENT(MODULE,SEG,
     &                 X_SLOPE,X_DRIFT,Y_DL,DL_FOUND,OK)
            ELSEIF ( SECT_XSECT  .EQ. SECTOR ) THEN
              CALL FDC_SEG_XSECT(MODULE,SEG,
     &                 X_SLOPE,X_DRIFT,Y_DL,DL_FOUND,OK)
            ENDIF
C
            IF ( SEG .LE. MAX_LABEL ) THEN
              LABEL = LABEL_KEY(SEG)
            ELSE
              WRITE(LABEL,100) SEG
  100         FORMAT(I4)
            ENDIF
          ENDIF
          CALL SWORDS(LABEL,II,JJ,LEN)
C
          IF ( OK ) THEN
C
            X_DRIFT = (X_DRIFT + STAGGER)
            X_SLOPE = X_SLOPE * Z_DIR
C
            Y_REAL_B = (Y_WIRE_B + Y_EXT_SEG)
            Y_REAL_E = (Y_WIRE_E - Y_EXT_SEG)
            X_REAL_B = X_DRIFT + X_SLOPE * (Y_REAL_B - Y_WIRE_B )
            X_REAL_E = X_DRIFT + X_SLOPE * (Y_REAL_E - Y_WIRE_B )
C
            X_WORLD_B = XCEN + XSCALE * X_REAL_B
            X_WORLD_E = XCEN + XSCALE * X_REAL_E
            Y_WORLD_B = YCEN + YSCALE * Y_REAL_B
            Y_WORLD_E = YCEN + YSCALE * Y_REAL_E
C
            IF (BTEST(STATUS,IUSED)) THEN
              CALL JMOVE(X_WORLD_B-TSIZE, Y_WORLD_B)
              CALL JDRAW(X_WORLD_B+TSIZE, Y_WORLD_B)
            ENDIF
            CALL JMOVE(X_WORLD_B, Y_WORLD_B)
            CALL JDRAW(X_WORLD_E, Y_WORLD_E)
            IF (BTEST(STATUS,IUSED)) THEN
              CALL JMOVE(X_WORLD_E-TSIZE, Y_WORLD_E)
              CALL JDRAW(X_WORLD_E+TSIZE, Y_WORLD_E)
            ENDIF
C
            CALL JJUST(2,2)
            CALL JMOVE(X_WORLD_B, Y_WORLD_B+1.0)
            CALL J2STRG(LABEL(II:JJ))
            CALL JMOVE(X_WORLD_E, Y_WORLD_E-1.0)
            CALL J2STRG(LABEL(II:JJ))
            DRAWN = .TRUE.
          ENDIF
        ENDDO
        IF ( DRAWN ) THEN
          X_WORLD_B = XCEN - DX - 0.75
          CALL JJUST(3,2)
          CALL JMOVE( X_WORLD_B, Y_WORLD_E-1.0)
          CALL J2STRG('Segment')
        ENDIF
      ENDIF
C
C ****  Draw Tracks
C
      CALL PUGETV('FDC DRAW TRACK',DRAW_TRACK)
      CALL PUGETV('FDC DRAW TMEAN',DRAW_TMEAN)
      IF ( DRAW_TRACK .GT. 0) THEN
        CALL PUGETA('FDC COLR TRACK',TRKCLR)
        CALL PXCOLR(TRKCLR)
        CALL JLSTYL(2)
C
        DRAWN = .FALSE.
        LFTRH = GZFTRH()
        LFDCT = LQ(LFTRH-IZFDCT)
C
        CALL UCTOH('FDCT',IBANK,4,4)
        IF ( LFLOC.GT.0 ) THEN
          IF ( IQ(LFLOC-4) .EQ. IBANK ) THEN        ! Draw track in user bank
            LFDCT = LFLOC
            IQ(LFDCT-5) = 1
            LQ(LFDCT) = 0
          ENDIF
        ENDIF
C
        DO WHILE ( LFDCT.NE.0 )
          CALL GTFDCT_LINK(LFDCT,QTRAK,QHTRK,LADDER)
          TRACK = IQ(LFDCT-5)
C
          IF (  HALF .EQ. IAND(1,IQTRAK(1))) THEN
            X_TRK = QTRAK(4)
            Y_TRK = QTRAK(5)
            DX_TRK = QTRAK(7)
            DY_TRK = QTRAK(8)
            TMEAN = QTRAK(20)
C Check that track is in cell:
            XPOS = X_TRK + DX_TRK * Z_DIR * (Z0-ZW0)
            YPOS = Y_TRK + DY_TRK * Z_DIR * (Z0-ZW0)
            CALL FGET_SECTOR(XPOS,YPOS,HALF,LAYER,QD,SECT0)
            XPOS = X_TRK + DX_TRK * Z_DIR * (Z0-ZW15)
            YPOS = Y_TRK + DY_TRK * Z_DIR * (Z0-ZW15)
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
              IF ( TRACK .LE. MAX_LABEL ) THEN
                LABEL = LABEL_KEY(TRACK)
              ELSE
                WRITE(LABEL,100) TRACK
              ENDIF
              CALL SWORDS(LABEL,II,JJ,LEN)
              CALL JJUST(2,2)
              CALL JMOVE(X_WORLD_B, Y_WORLD_B+0.75)
              CALL J2STRG(LABEL(II:JJ))
              CALL JMOVE(X_WORLD_E, Y_WORLD_E-0.75)
              CALL J2STRG(LABEL(II:JJ))
C
              IF ( DRAW_TMEAN .GT. 0) THEN
                IF ( TMEAN .LT. 10.0 ) THEN
                  WRITE(TEXT,200) ABS(TMEAN)
  200             FORMAT(F4.1)
                ELSE
                  TEXT = '*'
                ENDIF
                CALL JMOVE(X_WORLD_E, Y_WORLD_E-2.0)
                CALL J2STRG(TEXT)
              ENDIF
C
              IF (LADDER(0).GT.0) THEN
                CALL JMOVE(X_WORLD_B-TSIZE, Y_WORLD_B)
                CALL JDRAW(X_WORLD_B+TSIZE, Y_WORLD_B)
              ENDIF
              IF (LADDER(1).GT.0) THEN
                CALL JMOVE(X_WORLD_E-TSIZE, Y_WORLD_E)
                CALL JDRAW(X_WORLD_E+TSIZE, Y_WORLD_E)
              ENDIF
C
              DRAWN = .TRUE.
            ENDIF
          ENDIF
          LFDCT = LQ(LFDCT)
        END DO
        IF ( DRAWN  ) THEN
          X_WORLD_E = XCEN - DX - 0.75
          CALL JJUST(3,2)
          CALL JMOVE( X_WORLD_E, Y_WORLD_E-0.75)
          CALL J2STRG('Track')
          IF ( DRAW_TMEAN .GT. 0) THEN
            CALL JMOVE( X_WORLD_E, Y_WORLD_E-2.0)
            CALL J2STRG('Tr Mean')
          ENDIF
        ENDIF
      ENDIF
      CALL JLSTYL(0)
C----------------------------------------------------------------------
  999 RETURN
      END
