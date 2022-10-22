      SUBROUTINE PF1ADC(TRACE_TYPE)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Shows the raw FADC data from a single FDC
C-                         sense wire or delay line and also displays
C-                         the first differences.
C-
C-   Inputs  : TRACE_TYPE  1=orig,2=left,3=subtr,4=1&2,5=1&3,6=1&2&3
C-   Outputs : none
C-   Controls: none
C-
C-   Created  15-FEB-1989   Jeffrey Bantly
C-   Updated  19-MAR-1990   Jeffrey Bantly  use logical format
C-   Updated   4-MAY-1990   Jeffrey Bantly  add markers to hits
C-   Updated   5-NOV-1990   Jeffrey Bantly  convert bilinear conv to map
C-   Updated  20-FEB-1991   Lupe Howell  Convert to PIXIE using COMPACK
C-   Updated   8-SEP-1991   Jeffrey Bantly  update calls to GTFxHT
C-   Updated   8-SEP-1991   Jeffrey Bantly  add pulse shape subtraction,
C-                                            FDFADC call for crosstalk
C-                                            corrections, additional options
C-   Updated  11-OCT-1991   Robert E. Avery  Clean up error handling.
C-   Updated   2-MAR-1992   Robert E. Avery  add  TRACE_TYPE as input.
C-   Updated  13-MAR-1992   Jeffrey Bantly  Add second difference display,
C-           only display first 'FDC FADC BINS', 100 by default.
C-   Updated  25-MAY-1992   Robert E. Avery   Clean up, avoid losing
C-                              early bins, label first bin.
C-   Updated  19-JUN-1992   Robert E. Avery  Fix up some more.
C-   Updated   9-JUL-1992   Robert E. Avery  Allow user to choose scale. 
C-   Updated  18-AUG-1992   Robert E. Avery  Fix up pulse subtraction 
C-   Updated  17-DEC-1992   Robert E. Avery  Fix difference plots. 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:PXPARA.INC'
      INCLUDE 'D0$PARAMS:FDPARA.PARAMS'
C INPUT:
      INTEGER TRACE_TYPE
C LOCAL:
      INTEGER HALF,UNIT,QUAD,SECTOR,WIRE,LOGCHA
      INTEGER I, IZ
      INTEGER IMAX, TZ(MX_HIT_WIRE), NZ
      INTEGER EXPDAT(0:LFADC-1), IPEV
      INTEGER ICALL, IDX, LENGTH, IER, TMPADC, ISTRQU
      INTEGER IBIN,PSHLEV,RUNTYPE,FIRSTBIN
      INTEGER NUM_BINS 
      INTEGER II,JJ,LEN
      INTEGER VSCALE
C
      INTEGER CLUS_NUM, NUMCLU
      INTEGER LOCCLU(MAX_CLUS)
      INTEGER LENCLU(MAX_CLUS)
C
      REAL    Y(LFADC), DY(LFADC), DDY(LFADC), YWID
      REAL    Z(MX_HIT_WIRE), PK(MX_HIT_WIRE), RT(MX_HIT_WIRE)
      REAL    Y_SUB(LFADC),Y_PK(LFADC),LEADING_EDGE,FBIN,NEW_FADC
      REAL    PED(2),PULFAC
      DOUBLE PRECISION FVGAUSS_TAIL
C
      CHARACTER*4 PATH,FPATH
      CHARACTER*4 TITCLR,HITCLR,HITCLR2,HITCLR3
      CHARACTER*60 TEXTE
C
      LOGICAL CHACHK, IOK, BILCON,PULSHP
      LOGICAL EZERROR
C
      DATA Y,DY,DDY /LFADC*0.,LFADC*0.,LFADC*0./
      DATA NUM_BINS /100/
      DATA EXPDAT / LFADC*0/
      DATA ICALL /0/
C-----------------------------------------------------------------------
C
      IF(ICALL.EQ.0) THEN
        ICALL = 1
        CALL EZPICK('FTRAKS_RCP')
        IF( EZERROR(IER) ) THEN
          CALL ERRMSG('PIXIE','PF1ADC','Can not find FTRAKS_RCP','W')
        ELSE
          CALL EZGETS('FPATH',IDX,FPATH,LENGTH,IER)
          CALL EZGET_i('RUNTYPE',RUNTYPE,IER)
          CALL EZGET_i('PULSE_SHAPE_LEVEL',PSHLEV,IER)
          CALL EZGET('PULFAC',PULFAC,IER)
          CALL EZRSET
          PATH=FPATH
        ENDIF
      ENDIF
      CALL PATHST(PATH)
C
C ****  Pick correct RCP bank
C
      CALL EZPICK('PX_FDCDIS_RCP')
      IF ( EZERROR(IER) ) THEN
        CALL ERRMSG('PIXIE','PF1ADC','Cannot find PX_FDCDIS_RCP','W')
        GOTO 999
      ENDIF
      CALL PUGET_i( 'FDC HALF', HALF)
      CALL PUGET_i( 'FDC UNIT', UNIT)
      CALL PUGET_i( 'FDC QUAD', QUAD)
      CALL PUGET_i( 'FDC SECT', SECTOR)
      CALL PUGET_i( 'FDC WIRE', WIRE)
      CALL PUGET_l( 'FDC PULSHP', PULSHP)
      CALL PUGETV( 'STRING QUALITY', ISTRQU)
      CALL PUGETV( 'FDC FADC BINS', NUM_BINS )
      CALL PUGETA( 'FDC COLR LABELS', TITCLR )
      CALL PUGETA( 'FDC COLR HITS', HITCLR )
      CALL PUGETV( 'ELECT VERT SCALE', VSCALE)
      IF (TITCLR.EQ. '    ') TITCLR='CYA '
      IF (HITCLR.EQ. '    ') HITCLR='RED '
      HITCLR2='GRE '
      HITCLR3='YEL '
      IF (NUM_BINS.GE.LFADC) NUM_BINS = LFADC
      IF (NUM_BINS.LE.10) NUM_BINS = 10
      IF (ISTRQU .LT. 3) CALL PUSETV( 'STRING QUALITY', 3)
      IF (VSCALE.NE.0) CALL FLGSET('PF_SET_SCALE',.TRUE.)
C
      CALL PUOPEN
      IF ( UNIT.EQ.0 ) THEN
        IF(HALF.EQ.0) THEN
          WRITE( TEXTE, 1000 ) QUAD,SECTOR,WIRE
 1000     FORMAT(' North FDC, Theta Unit, Quad',I2,
     &       ', Sector',I2,', Wire',I2)
        ELSEIF(HALF.EQ.1) THEN
          WRITE( TEXTE, 1001 ) QUAD,SECTOR,WIRE
 1001     FORMAT(' South FDC, Theta Unit, Quad',I2,
     &       ', Sector',I2,', Wire',I2)
        ENDIF
      ELSE
        IF(HALF.EQ.0) THEN
          WRITE( TEXTE, 1002 ) SECTOR,WIRE
 1002     FORMAT(' North FDC, Phi Unit, Sector',I2,', Wire',I2)
        ELSEIF(HALF.EQ.1) THEN
          WRITE( TEXTE, 1003 ) SECTOR,WIRE
 1003     FORMAT(' South FDC, Phi Unit, Sector',I2,', Wire',I2)
        ENDIF
      ENDIF
C
      CALL JJUST( 2, 1)
      CALL PXCOLR( TITCLR )
      CALL SWORDS(TEXTE,II,JJ,LEN)
      CALL PUVSTR( 0., YWIND2*(0.90), 2., 1.5, TEXTE(II:JJ) )
      CALL PUVSTR( 0., YWIND2*(0.70), 2., 1.5, ' FADC Trace ')
      CALL PUVSTR( 0., YWIND2*(-.15), 2., 1.5, ' First Difference ')
      CALL PUVSTR( 0., YWIND2*(-.55), 2., 1.5, ' Second Difference ')
      CALL JRCLOS
C
      CALL VZERO( Y, LFADC)
      CALL VZERO( Y_SUB, LFADC)
      CALL VZERO( Y_PK, LFADC)
      CALL VZERO( DY, LFADC)
      CALL VZERO( DDY, LFADC)
C
      IF ( WIRE.NE.0 ) THEN
        CALL FDFADC( HALF,UNIT,QUAD,SECTOR,0,
     &               EXPDAT,NUMCLU,LOCCLU,LENCLU)
      ENDIF
      CALL FDFADC( HALF,UNIT,QUAD,SECTOR,WIRE,
     &               EXPDAT,NUMCLU,LOCCLU,LENCLU)
      DO CLUS_NUM=1,NUMCLU
        IBIN=LOCCLU(CLUS_NUM)-1
        DO I=1,LENCLU(CLUS_NUM)
          IBIN=IBIN+1
          Y(IBIN+1)=FLOAT(EXPDAT(IBIN))
        ENDDO
      ENDDO
C
C  Start at LAST (earliest) cluster
C
      FIRSTBIN = LOCCLU(NUMCLU)
      IF ( FIRSTBIN+NUM_BINS .GT. LFADC ) FIRSTBIN = LFADC - NUM_BINS 
C
      DO I=1,NUM_BINS
        Y(I) = Y(FIRSTBIN+I)
        Y_SUB(I) = Y(I)
      ENDDO
      DO I=1,NUM_BINS-1
        IF ( (Y(I).EQ.0).OR.(Y(I+1).EQ.0) ) THEN
          DY(I) = 0.0
        ELSE
          DY(I) = Y(I+1) - Y(I)
        ENDIF
      ENDDO
      DY(NUM_BINS) = 0.0
      DO I=1,NUM_BINS-1
        IF ( (DY(I).EQ.0).OR.(DY(I+1).EQ.0) ) THEN
          DDY(I) = 0.0
        ELSE
          DDY(I) = DY(I+1) - DY(I)
        ENDIF
      ENDDO
      DDY(NUM_BINS) = 0.0
C
      IF(UNIT.EQ.0) THEN
        CALL GTFTHT(HALF,QUAD,SECTOR,WIRE,Z,PK,RT,TZ,NZ)
      ELSEIF(UNIT.EQ.1) THEN
        CALL GTFPHT(HALF,SECTOR,WIRE,Z,PK,RT,TZ,NZ)
      ENDIF
      DO I=1,NZ
        Z(I)= Z(I)-FIRSTBIN
      ENDDO
C
      CALL PUVPRT( -.8, .8, .20, .70 )
      IF (TRACE_TYPE.NE.2 .AND. TRACE_TYPE.NE.3) THEN
        CALL PFUHIS( NUM_BINS, Y, TITCLR, HITCLR, Z, TZ, NZ )
      ENDIF
C
C Label first bin
C
      CALL JWINDO(XWIND1,XWIND2,YWIND1,YWIND2)
      CALL JVPORT(XVPRT1, XVPRT2, YVPRT1, YVPRT2) 
      CALL PUOPEN
      CALL JJUST(1,1)
      CALL PXCOLR( TITCLR )
      WRITE( TEXTE, 1010 ) FIRSTBIN
 1010 FORMAT(' First FADC Bin ',I3)
      CALL PUVSTR( XWIND1*(0.80),YWIND2*(0.20), 1., 1.5, TEXTE )
      CALL JRCLOS
C
C  Pulse shape subtraction, if requested
C
      IF (PSHLEV.GE.2) THEN
        IMAX = 30
        IF(RUNTYPE.EQ.0) IMAX = 20
        DO 40 IZ=1,NZ
          LEADING_EDGE = Z(IZ)
          IPEV = NINT(LEADING_EDGE) - 3
          IF(IPEV.LT.1) IPEV=1
          IF(IPEV.GT.(LFADC-30)) IPEV=LFADC-30
          DO 50 IBIN=IPEV,IPEV+IMAX
            FBIN = FLOAT(IBIN) - LEADING_EDGE
            NEW_FADC = FVGAUSS_TAIL(FBIN,RT(IZ))*PK(IZ)*PULFAC
            Y_SUB(IBIN) = Y_SUB(IBIN) - NEW_FADC
            Y_PK(IBIN) = Y_PK(IBIN) + NEW_FADC
            IF( Y_SUB(IBIN) .LT. 0.0 ) Y_SUB(IBIN) = 0.0
   50     CONTINUE
   40   CONTINUE
        CALL FGTLPD(HALF,UNIT,QUAD,SECTOR,WIRE,PED(1),PED(2))
        DO IBIN=1,LFADC
          IF(Y_PK(IBIN).NE.0.0) Y_PK(IBIN)=Y_PK(IBIN)+PED(1)
        ENDDO
        CALL PUVPRT( -.8, .8, .20, .70 )
        IF(TRACE_TYPE.EQ.2) THEN
          CALL PFUHIS( NUM_BINS, Y_SUB, TITCLR, HITCLR2, Z, TZ, NZ )
        ELSEIF (TRACE_TYPE.EQ.4) THEN
          CALL PFUHIS( NUM_BINS, Y_SUB, TITCLR, HITCLR3, Z, TZ, NZ+100 )
        ELSEIF (TRACE_TYPE.EQ.6) THEN
          CALL PFUHIS( NUM_BINS, Y_SUB, TITCLR, HITCLR2, Z, TZ, NZ+100 )
        ENDIF
        IF(TRACE_TYPE.EQ.3) THEN
          CALL PFUHIS( NUM_BINS, Y_PK, TITCLR, HITCLR2, Z, TZ, NZ )
        ELSEIF (TRACE_TYPE.EQ.5) THEN
          CALL PFUHIS( NUM_BINS, Y_PK, TITCLR, HITCLR3, Z, TZ, NZ+100 )
        ELSEIF (TRACE_TYPE.EQ.6) THEN
          CALL PFUHIS( NUM_BINS, Y_PK, TITCLR, HITCLR3, Z, TZ, NZ+100 )
        ENDIF
C
      ENDIF
      CALL FLGSET('PF_SET_SCALE',.FALSE.)
C
C  Draw in first differences.
C
      DO I=1,NZ
        Z(I)= Z(I)-0.5
      ENDDO
      CALL PUVPRT( -.8, .8, -.50, -.15 )
      CALL PFUHIS( NUM_BINS, DY, TITCLR, HITCLR, Z, TZ, NZ )
C
C  Draw in second differences.
C
      DO I=1,NZ
        Z(I)= Z(I)-0.5
      ENDDO
      CALL PUVPRT( -.8, .8, -.90, -.55 )
      CALL PFUHIS( NUM_BINS, DDY, TITCLR, HITCLR, Z, TZ, NZ )
C
C-----------------------------------------------------------------------
  999 CONTINUE
      CALL PUSETV( 'STRING QUALITY', ISTRQU)
C
C ****  Reset RCP bank
C
      CALL EZRSET
      RETURN
      END
