      SUBROUTINE PF_THETA_SECT_VIEW(HALF,QUAD,SECTOR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Routine to display the hits, segments
C-                         and tracks in an FDC sector (cell).
C-
C-   Inputs  : HALF,QUAD,SECTOR
C-   Outputs : displays
C-
C-   Created  25-APR-1991   Robert E. Avery
C-   Updated   6-AUG-1991   Robert E. Avery  Leave out direction arrow.
C-   Updated  25-JAN-1992   Robert E. Avery  Add call to new  routine for
C-      drawing isajet tracks in this view.
C-   Updated  14-OCT-1992   Robert E. Avery  Add call to new  routine for 
C-      drawing ZTRAKS road in this view.
C-   Updated  16-AUG-1993   Robert E. Avery  Remove use of LUSER bank. 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:PXPARA.INC'
      INCLUDE 'D0$INC:FLOCAL.INC/LIST'
C
      INTEGER HALF,UNIT,QUAD,SECTOR
      INTEGER ISTRQU, II, JJ
      INTEGER IER
      INTEGER LEN
      INTEGER GZFDTA,LFDTA 
      INTEGER GZISAE,LISAE 
      INTEGER DRAWIR,DRATRK,DRAISA,DRAHIT 
      INTEGER NUM_PARAMS 
      INTEGER CROAD
C
      CHARACTER*4 TITCLR,SECCLR
      CHARACTER*8 LABELS(0:9)
      CHARACTER*50 TEXTE
C
      REAL    XC, YC, DX, DY
      REAL    XSCALE, YSCALE
      REAL    YWIDTH, ZWIDTH 
      REAL    SIZE
C
      LOGICAL FIRST
      LOGICAL EZERROR
C
      DATA FIRST /.TRUE./
      DATA SECCLR,TITCLR/'    ','    '/
C----------------------------------------------------------------------
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        CALL UCTOH('ROAD',CROAD,4,4)
      ENDIF
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
      CALL PUGETV( 'FDC DRAW TRACK', DRATRK)
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
      CALL PUSETV( 'FDC SECT', SECTOR)
C
      CALL PUOPEN
      IF(HALF.EQ.0) THEN
        WRITE( TEXTE, 1000 ) QUAD,SECTOR
 1000   FORMAT(' North FDC, Theta Unit, Quadrant',I2,
     &       ', Sector',I2)
      ELSEIF(HALF.EQ.1) THEN
        WRITE( TEXTE, 1001 ) QUAD,SECTOR
 1001   FORMAT(' South FDC, Theta Unit, Quadrant',I2,
     &       ' Sector',I2)
      ENDIF
      CALL PXCOLR(TITCLR)
      CALL JJUST( 2, 2)
      CALL PUVSTR( 0., YWIND2*.90, 1.5, 1.5, TEXTE )
      CALL JRCLOS
C
      LFDTA = GZFDTA()
      IF( LFDTA .LE. 0 ) GOTO 998
      NUM_PARAMS = IC(LFDTA+3)
      YWIDTH = C(LFDTA + 46 + SECTOR * NUM_PARAMS )
      ZWIDTH = C(LFDTA + 47 + SECTOR * NUM_PARAMS )
      IF ( YWIDTH .EQ.0. .OR. ZWIDTH .EQ.0. ) GOTO 998
C
      SIZE=YWIND2*0.60
      YSCALE = SIZE / ZWIDTH !  world/cm
      XSCALE = YDVMAG/XDVMAG  * (YSCALE)
C
      XC = 0
      YC = 0
      DX = XSCALE * YWIDTH
      DY = YSCALE * ZWIDTH
C
      CALL PUOPEN
      CALL PXRECT( SECCLR, XC, YC, 0., DX, DY )
C
      IF( DRAWIR .GE. 1 ) CALL PFTWIR_HORIZ( HALF, QUAD, SECTOR,
     &            XC, YC, DX, DY, XSCALE, YSCALE )
      IF( DRAHIT .GE. 1 ) CALL PFTHIT_HORIZ( HALF, QUAD, SECTOR,
     &            XC, YC, DX, DY, XSCALE, YSCALE )
      CALL PFTTRK_HORIZ( HALF, QUAD, SECTOR,
     &            XC, YC, DX, DY, XSCALE, YSCALE )
      LISAE = GZISAE()
      IF ( (DRAISA.GE.1) .AND. (LISAE .GT. 0) ) THEN
        CALL FDCISA
        CALL PFTTRK_ISA( HALF, QUAD, SECTOR,
     &            XC, YC, DX, DY, XSCALE, YSCALE )
      ENDIF
C
      IF ( LFLOC.GT.0 ) THEN
        IF ( IQ(LFLOC-4) .EQ. CROAD ) THEN        ! Draw segment in user bank
          CALL PFTTRK_ROAD( HALF, QUAD, SECTOR,
     &            XC, YC, DX, DY, XSCALE, YSCALE )
        ENDIF
      ENDIF

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
