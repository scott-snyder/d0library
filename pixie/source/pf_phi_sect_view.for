      SUBROUTINE PF_PHI_SECT_VIEW(HALF,SECTOR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Routine to display the hits, segments
C-                         and tracks in an FDC sector (cell).
C-
C-   Inputs  : HALF,SECTOR
C-   Outputs : displays
C-
C-   Created  13-MAY-1991   Robert E. Avery
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
      INCLUDE 'D0$INC:PI.DEF'
      INCLUDE 'D0$INC:FLOCAL.INC/LIST'
C
      INTEGER HALF,UNIT,SECTOR
      INTEGER ISTRQU, II, JJ
      INTEGER IER
      INTEGER LEN
      INTEGER GZISAE,LISAE 
      INTEGER GZFDPH,LFDPH 
      INTEGER DRAWIR,DRAHIT,DRAISA
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
      LOGICAL EZERROR
      LOGICAL FIRST
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
      CALL PUSETV( 'FDC SECT', SECTOR)
C
      CALL PUOPEN
      IF(HALF.EQ.0) THEN
        WRITE( TEXTE, 1000 ) SECTOR
 1000   FORMAT(5X,' North FDC, Phi Unit, Sector',I3)
      ELSEIF(HALF.EQ.1) THEN
        WRITE( TEXTE, 1001 ) SECTOR
 1001   FORMAT(5X,' South FDC, Phi Unit, Sector',I3)
      ENDIF
      CALL PXCOLR(TITCLR)
      CALL JJUST( 2, 2)
      CALL PUVSTR( 0., YWIND2*.90, 1.5, 1.5, TEXTE )
C
C      CALL JMOVE( XWIND2*0.90,  YWIND1*0.90)
C      CALL JSIZE(0.75,1.0)
C      CALL JJUST(3,1)
C      CALL JFONT(3)
C      CALL JHSTRG('IR [FONT=23]c')
      CALL JRCLOS
C
      LFDPH = GZFDPH()
      IF( LFDPH .LE. 0 ) GOTO 998
      YWIDTH = C( LFDPH + 69 + 3 ) * PI /36
      ZWIDTH = C( LFDPH + 69 + 4 )             
      IF ( YWIDTH .EQ.0. .OR. ZWIDTH .EQ.0. ) GOTO 998
C
      SIZE=YWIND2*0.80
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
      IF( DRAWIR .GE. 1 ) CALL PFPWIR_HORIZ( HALF, SECTOR,
     &            XC, YC, DX, DY, XSCALE, YSCALE )
      IF( DRAHIT .GE. 1 ) CALL PFPHIT_HORIZ( HALF, SECTOR,
     &            XC, YC, DX, DY, XSCALE, YSCALE )
      CALL PFPTRK_HORIZ( HALF, SECTOR,
     &            XC, YC, DX, DY, XSCALE, YSCALE )
      LISAE = GZISAE()
      IF ( (DRAISA.GE.1) .AND. (LISAE .GT. 0) ) THEN
        CALL FDCISA
        CALL PFPTRK_ISA( HALF, SECTOR,
     &            XC, YC, DX, DY, XSCALE, YSCALE )
      ENDIF
C
C  Draw road, if it is stored in user bank.
C
      IF ( LFLOC.GT.0 ) THEN
        IF ( IQ(LFLOC-4) .EQ. CROAD ) THEN        
          CALL PFPTRK_ROAD( HALF, SECTOR,
     &            XC, YC, DX, DY, XSCALE, YSCALE )
        ENDIF
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
