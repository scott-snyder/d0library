      SUBROUTINE PF_THETA_BOUND_VIEW(HALF,QUAD,SECTOR_0)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Routine to display the hits, segments
C-                         and tracks in two FDC sectors (cells),
C-                         at boundary.
C-   Inputs  : HALF,QUAD,SECTOR_0
C-   Outputs : displays
C-
C-   Created  10-JUN-1991   Robert E. Avery
C-   Updated   6-AUG-1991   Robert E. Avery  Leave out direction arrow.
C-   Updated  25-JAN-1992   Robert E. Avery  Add call to new  routine for
C-      drawing isajet tracks in this view.
C-   Updated   2-APR-1993   Robert E. Avery  Draw EM road if requested. 
C-   Updated  16-AUG-1993   Robert E. Avery  Remove use of LUSER bank. 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:PXPARA.INC'
      INCLUDE 'D0$INC:FLOCAL.INC/LIST'
C
      INTEGER HALF,UNIT,QUAD,SECTOR_0
      INTEGER SECTOR_1
      INTEGER ISTRQU, II, JJ
      INTEGER IER
      INTEGER LEN
      INTEGER GZISAE,LISAE 
      INTEGER DRAWIR,DRAHIT,DRAISA
      INTEGER CROAD
C
      CHARACTER*4 ROADCLR
      CHARACTER*4 TITCLR,SECCLR
      CHARACTER*8 LABELS(0:9)
      CHARACTER*50 TEXTE
C
      REAL    XC, YC, DX, DY
      REAL    XSCALE, YSCALE
      REAL    ZWIDTH 
      REAL    HALF_WIDTH ,FULL_WIDTH 
      REAL    HALF_SHIFT ,FULL_SHIFT 
      REAL    SIZE
      REAL    DIMENS(6)
C
      LOGICAL EZERROR
      LOGICAL FIRST 
C
      SAVE SECCLR,TITCLR,FIRST
      DATA SECCLR,TITCLR/'    ','    '/
      DATA FIRST /.TRUE./
C----------------------------------------------------------------------
C
      IF ( FIRST ) THEN
        FIRST= .FALSE.
C
C  Get cell size parameters from STP banks
C
        CALL GTFDTX(4,0,DIMENS)
        HALF_WIDTH = DIMENS(2)
        ZWIDTH = DIMENS(3)
        CALL GTFDTX(4,3,DIMENS)
        FULL_WIDTH = DIMENS(2)
C
        CALL GTFWTX(0,4,0,DIMENS)
        HALF_SHIFT = DIMENS(4)
        CALL GTFWTX(0,4,1,DIMENS)
        HALF_SHIFT = ( DIMENS(4) - HALF_SHIFT )/2.
C
        CALL GTFWTX(0,4,3,DIMENS)
        FULL_SHIFT = DIMENS(4)
        CALL GTFWTX(0,4,4,DIMENS)
        FULL_SHIFT = ( DIMENS(4) - FULL_SHIFT )/2.
C
        CALL UCTOH('ROAD',CROAD,4,4)
      ENDIF
      IF   ( ( HALF_WIDTH .EQ. 0. )
     &  .OR. ( FULL_WIDTH .EQ. 0. )
     &  .OR. ( ZWIDTH     .EQ. 0. ) ) GOTO 998
C
C ****  Pick correct RCP bank
C
      CALL EZPICK('PX_FDCDIS_RCP')
      IF ( EZERROR(IER) ) THEN
        CALL ERRMSG('PIXIE','PF_SECTOR_HITS',
     &    'Cannot find PX_FDCDIS_RCP','W')
        GOTO 999
      ENDIF
      CALL PUGETV( 'FDC DRAW WIRE', DRAWIR)
      CALL PUGETV( 'FDC DRAW HITS', DRAHIT)
      CALL PUGETV( 'FDC DRAW ISATRK ', DRAISA)
      CALL PUGETA( 'FDC COLR LABELS', TITCLR )
      CALL PUGETA( 'FDC COLR SECTOR',SECCLR )
      CALL PUGETV( 'STRING QUALITY', ISTRQU)
      IF (ISTRQU .LT. 3) CALL PUSETV( 'STRING QUALITY', 3)
C
      IF(TITCLR.EQ. '    ') TITCLR='CYA '
      IF(SECCLR.EQ. '    ') SECCLR='GRE '
C
      CALL PUSETV( 'FDC HALF', HALF)
      CALL PUSETV( 'FDC QUAD', QUAD)
      CALL PUSETV( 'FDC SECT', SECTOR_0)
      SECTOR_1 = SECTOR_0+1
C
      CALL PUOPEN
      IF(HALF.EQ.0) THEN
        WRITE( TEXTE, 1000 ) QUAD,SECTOR_0,SECTOR_1
 1000   FORMAT(' North FDC, Theta Unit, Quadrant',I2,
     &       ', Sector',I2,' &',I2)
      ELSEIF(HALF.EQ.1) THEN
        WRITE( TEXTE, 1001 ) QUAD,SECTOR_0,SECTOR_1
 1001   FORMAT(' South FDC, Theta Unit, Quadrant',I2,
     &       ' Sector',I2,' &',I2)
      ENDIF
      CALL PXCOLR(TITCLR)
      CALL JJUST( 2, 2)
      CALL PUVSTR( 0., YWIND2*.90, 1.5, 1.5, TEXTE )
C
C      CALL JMOVE( XWIND2*0.85, YWIND1*0.85)
C      CALL JSIZE(0.75,1.0)
C      CALL JJUST(3,1)
C      CALL JFONT(3)
C      IF ( QUAD .LE. 3) THEN
C        CALL JHSTRG('IR [FONT=23]d')
C      ELSE
C        CALL JHSTRG('IR [FONT=23]c')
C      ENDIF
      CALL JRCLOS
C
C
      SIZE=YWIND2*0.60
      YSCALE = SIZE / ZWIDTH !  world/cm
      XSCALE = YDVMAG/XDVMAG  * (YSCALE)
C
C Draw lower sector first:
C
      IF ( SECTOR_0 .LE. 2 ) THEN
        XC = -XSCALE * HALF_SHIFT
        DX =  XSCALE * HALF_WIDTH 
      ELSE
        XC = -XSCALE * FULL_SHIFT
        DX =  XSCALE * FULL_WIDTH
      ENDIF
      YC = 0
      DY = YSCALE * ZWIDTH
C
      CALL PUOPEN
      CALL PXRECT( SECCLR, XC, YC, 0., DX, DY )
C
      IF( DRAWIR .GE. 1 ) CALL PFTWIR_HORIZ( HALF, QUAD, SECTOR_0,
     &            XC, YC, DX, DY, XSCALE, YSCALE )
      IF( DRAHIT .GE. 1 ) CALL PFTHIT_HORIZ( HALF, QUAD, SECTOR_0,
     &            XC, YC, DX, DY, XSCALE, YSCALE )
      CALL PFTTRK_HORIZ( HALF, QUAD, SECTOR_0,
     &            XC, YC, DX, DY, XSCALE, YSCALE )
      LISAE = GZISAE()
      IF ( (DRAISA.GE.1) .AND. (LISAE .GT. 0) ) THEN
        CALL FDCISA
        CALL PFTTRK_ISA( HALF, QUAD, SECTOR_0,
     &            XC, YC, DX, DY, XSCALE, YSCALE )
      ENDIF
C
C Draw EM road if needed:
C
      IF ( LFLOC.GT.0 ) THEN
        IF ( IQ(LFLOC-4) .EQ. CROAD ) THEN        
          CALL PFTTRK_ROAD( HALF, QUAD, SECTOR_0,
     &            XC, YC, DX, DY, XSCALE, YSCALE )
        ENDIF
      ENDIF
C
      CALL JRCLOS
C
C Then higher sector:
C
      IF ( SECTOR_1 .GT. 5 ) THEN
        GOTO 998
      ELSEIF ( SECTOR_1 .LE. 2 ) THEN
        XC = XSCALE * HALF_SHIFT
        DX = XSCALE * HALF_WIDTH 
      ELSE
        XC = XSCALE * FULL_SHIFT
        DX = XSCALE * FULL_WIDTH
      ENDIF
C
      CALL PUOPEN
      CALL PXRECT( SECCLR, XC, YC, 0., DX, DY )
C
      IF( DRAWIR .GE. 1 ) CALL PFTWIR_HORIZ( HALF, QUAD, SECTOR_1,
     &            XC, YC, DX, DY, XSCALE, YSCALE )
      IF( DRAHIT .GE. 1 ) CALL PFTHIT_HORIZ( HALF, QUAD, SECTOR_1,
     &            XC, YC, DX, DY, XSCALE, YSCALE )
      CALL PFTTRK_HORIZ( HALF, QUAD, SECTOR_1,
     &            XC, YC, DX, DY, XSCALE, YSCALE )
      IF ( (DRAISA.GE.1) .AND. (LISAE .GT. 0) ) THEN
        CALL PFTTRK_ISA( HALF, QUAD, SECTOR_1,
     &            XC, YC, DX, DY, XSCALE, YSCALE )
      ENDIF
C 
      CALL JRCLOS
C----------------------------------------------------------------------
  998 CONTINUE
      CALL PUSETV( 'STRING QUALITY', ISTRQU)
C
C ****  Reset RCP bank
C
      CALL EZRSET
  999 RETURN
      END
