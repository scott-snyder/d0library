      SUBROUTINE PF8ADC
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Shows the raw FADC data from a FDC Theta
C-                         sector's sense wires and delay lines.
C-
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created  15-FEB-1989   Jeffrey Bantly  modified from PDFADC.FOR
C-   Updated  19-MAR-1990   Jeffrey Bantly  use logical format
C-   Updated   4-MAY-1990   Jeffrey Bantly  add markers to hits
C-   Updated   7-AUG-1990   Jeffrey Bantly  use color params
C-   Updated   5-NOV-1990   Jeffrey Bantly  convert bilinear conv to map
C-   Updated  23-JAN-1991   Jeffrey Bantly  check for CDD3 bank
C-   Updated  20-FEB-1991   Lupe Howell     Implementing PIXIE using COMPACK
C-   Updated  30-APR-1991   Jeffrey Bantly  make better use of Compack
C-   Updated  14-MAY-1991   Susan K. Blessing  Use North and South halves
C-   Updated   5-JUL-1991   Jeffrey Bantly  add in crosstalk correction
C-                                          and additional options
C-   Updated   8-SEP-1991   Jeffrey Bantly  remove corrections to FDFADC
C-   Updated  11-OCT-1991   Robert E. Avery  Clean up error handling. 
C-   Updated  19-FEB-1992   Robert E. Avery  Put dialogue into 
C-                              PFPICK_SECTOR, reorganize.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:PXPARA.INC'
      INCLUDE 'D0$LINKS:IZCDD2.LINK'
      INCLUDE 'D0$LINKS:IZCDD3.LINK'
      INCLUDE 'D0$PARAMS:FDPARA.PARAMS'
C
      INTEGER HALF,UNIT,QUAD,SECTOR,WIRE,LOGCHA
      INTEGER HALF1,QUAD1,SECTOR1
      INTEGER LKCDD2,LKCDD3
      INTEGER I, IPEV, IBIN
      INTEGER NDELAY, ISTRQU, TZ(MX_HIT_WIRE), NZ, IZ, II, JJ
      INTEGER EXPDAT(0:LFADC-1), LEN
      INTEGER ICALL, NPULSE, IANS
      INTEGER MXWIRE,PSHLEV,IBEG,ISTART,MXCHAN,RUNTYPE
      INTEGER IDX,LENGTH,IER,TRACE_TYPE,IMAX
C
      INTEGER CLUS_NUM, NUMCLU
      INTEGER LOCCLU(MAX_CLUS)
      INTEGER LENCLU(MAX_CLUS)
C
      REAL    Y(LFADC), YWID, TOP, BOT, TOPTOB, UPSIDN
      REAL    XW1, XW2, YW1, YW2, YXRAT
      REAL    Z(MX_HIT_WIRE), PK(MX_HIT_WIRE), RT(MX_HIT_WIRE)
      REAL    PED(2), Y_SUB(LFADC), FBIN, LEADING_EDGE
      REAL    MINY,MAXY,Y_PK(LFADC),PULFAC, NEW_FADC
      DOUBLE PRECISION GAUSS_TAIL
      DOUBLE PRECISION FVGAUSS_TAIL
C
      CHARACTER*4 PATH,FPATH
      CHARACTER*4 TITCLR,HITCLR,HITCLR2,HITCLR3
      CHARACTER*8 LABELS(0:9)
      CHARACTER*50 TEXTE
      CHARACTER*60 ANSWER
      CHARACTER*80 STRING,KEYTEXT
      CHARACTER*80 PROM
C
      LOGICAL FLGVAL,HARDCOPY 
      LOGICAL ASK_QUESTIONS 
      LOGICAL DOIT,DONE
      LOGICAL FADCHK(0:NBPSEN-1)
      LOGICAL BILCON,IOK,PULSHP
      LOGICAL EZERROR
C
      SAVE ICALL,PATH,PSHLEV
      DATA Y /LFADC*0./
      DATA EXPDAT / LFADC*0/
      DATA LKCDD2,LKCDD3/0,0/
      DATA TRACE_TYPE /1/
      DATA HALF,UNIT,QUAD,SECTOR/ 0,0,0,0/
      DATA TITCLR,HITCLR/2*'    '/
      DATA PROM /
     &  ' Trace type?(1-5,1=orig,2=left,3=subtr,4=1&2,5=1&3,6=1&2&3)'/
      DATA LABELS /' Wire  0',' Wire  1',' Wire  2',' Wire  3',
     &  ' Wire  4',' Wire  5',' Wire  6',' Wire  7',' DL HV 0',
     &  ' DL SG 1'/
      DATA ICALL /0/
C-----------------------------------------------------------------------
C
      ASK_QUESTIONS = .TRUE.
      GOTO 100

C
C   Entry point for multiple displays - requires knowledge of Half and Sector.
C
      ENTRY PF8ADC_MID(HALF1,QUAD1,SECTOR1)
C
      ASK_QUESTIONS = .FALSE.
      QUAD=QUAD1
      HALF=HALF1
      SECTOR=SECTOR1
      TRACE_TYPE=1
C
  100 CONTINUE
C
C ****  Pick correct RCP bank
C
      CALL EZPICK('PX_FDCDIS_RCP')
      IF ( EZERROR(IER) ) THEN
        CALL ERRMSG('PIXIE','PF8ADC','Cannot find PX_FDCDIS_RCP','W')
        GOTO 999
      ENDIF
C
      IF(ICALL.EQ.0) THEN
        ICALL = 1
        CALL EZPICK('FTRAKS_RCP')
        IF( EZERROR(IER) ) THEN
          CALL ERRMSG('PIXIE','PF8ADC','Can not find FTRAKS_RCP','W')
        ELSE
          CALL EZGETS('FPATH',IDX,FPATH,LENGTH,IER)
          CALL EZGET('RUNTYPE',RUNTYPE,IER)
          CALL EZGET('PULSE_SHAPE_LEVEL',PSHLEV,IER)
          CALL EZGET('PULFAC',PULFAC,IER)
          CALL EZRSET
          MXWIRE=MXWIRT
          PATH=FPATH
        ENDIF
      ENDIF
      CALL PATHST(PATH)
C
      LKCDD2=LQ(LHEAD-IZCDD2)
      LKCDD3=LQ(LHEAD-IZCDD3)
      IF(LKCDD2.LE.5.AND.LKCDD3.LE.5) THEN
        CALL INTMSG(' No FADC CDD3 bank present')
        GOTO 999
      ENDIF
C
      CALL PUGETV( 'FDC PULSHP', PULSHP)
      IF ( ASK_QUESTIONS ) THEN
        CALL PFPICK_SECTOR(0,HALF,QUAD,SECTOR)
        IF (HALF .EQ. 2) GOTO 999
C
        HARDCOPY = FLGVAL('HARDCOPY')
        IF ( .NOT.HARDCOPY ) THEN         
          IF( PULSHP .AND. (PSHLEV.GT.1) ) THEN
            ANSWER=' '
            LEN=0
            CALL GETPAR(1,PROM,'U',ANSWER)
            CALL SWORDS(ANSWER,II,JJ,LEN)
            IF(LEN.NE.0) READ(ANSWER(1:LEN),*,ERR=200) TRACE_TYPE
            IF(TRACE_TYPE .LT. 1 .OR. TRACE_TYPE .GT. 6 ) THEN
  200         TRACE_TYPE = 1
            ENDIF
          ENDIF
        ENDIF
      ENDIF
      CALL PUSETV( 'FDC HALF', HALF)
      CALL PUSETV( 'FDC QUAD', QUAD)
      CALL PUSETV( 'FDC SECT', SECTOR)
      CALL PUGETA( 'FDC COLR LABELS', TITCLR )
      CALL PUGETA( 'FDC COLR HITS', HITCLR )
      IF(TITCLR.EQ. '    ') TITCLR='FOR '
      IF(HITCLR.EQ. '    ') HITCLR='RED '
      HITCLR2='GRE '
      HITCLR3='YEL '
C
      CALL PUGETV( 'STRING QUALITY', ISTRQU)
      IF (ISTRQU .LT. 3) CALL PUSETV( 'STRING QUALITY', 3)
C
C   Label top of display
C
      CALL PUOPEN
      IF(HALF.EQ.0) THEN
        WRITE( TEXTE, 1000 ) QUAD,SECTOR
 1000   FORMAT(' North FDC, Theta Unit, Quadrant',I2,
     &       ', Sector',I2)
      ELSEIF(HALF.EQ.1) THEN
        WRITE( TEXTE, 1001 ) QUAD,SECTOR
 1001   FORMAT(' South FDC, Theta Unit, Quadrant',I2,
     &       ', Sector',I2)
      ENDIF
      CALL PXCOLR(TITCLR)
      CALL JJUST( 2, 2)
      CALL PUVSTR( 0., YWIND2*.90, 1.5, 1.5, TEXTE )
      CALL JRCLOS
C
C   Loop over wires and dl in sector (theta) or full sector (phi)
C
      NDELAY = 2
      MXCHAN = MXWIRT+NDELAY
      MXWIRE = MXWIRT
      UNIT=0
C
C
      DO 10 WIRE=0,MXCHAN
        UPSIDN = 1.
        IF ( WIRE .GT. 7+1 ) UPSIDN = -1.
        CALL VZERO(  Y, LFADC)
        CALL VZERO(  Y_SUB, LFADC)
        CALL VZERO(  Y_PK, LFADC)
C
        CALL FDFADC( HALF,UNIT,QUAD,SECTOR,WIRE,
     &               EXPDAT,NUMCLU,LOCCLU,LENCLU)
        DO CLUS_NUM=1,NUMCLU
          IBIN=LOCCLU(CLUS_NUM)-1
          DO I=1,LENCLU(CLUS_NUM)
            IBIN=IBIN+1
            Y(IBIN+1)=FLOAT(EXPDAT(IBIN))*UPSIDN
            Y_SUB(IBIN+1)=Y(IBIN+1)
          ENDDO
        ENDDO
C
        CALL GTFTHT(HALF,QUAD,SECTOR,WIRE,Z,PK,RT,TZ,NZ)
C
        CALL PUVPRT(-1.,1.,YVPRT1,YVPRT2)
        CALL J4RGET(1,XW1,XW2,YW1,YW2)
        CALL JWINDO(-1.,1.,-0.8337,0.767)
        CALL J4RGET(1,XW1,XW2,YW1,YW2)
        TOPTOB = 1.6007
        IF ( WIRE .LE. 7 ) THEN
          TOP = 0.767 - (WIRE+2)*(TOPTOB/10.)
          BOT = TOP - (TOPTOB/12.)
        ELSEIF ( WIRE .EQ. 8 ) THEN
          TOP = 0.767
          BOT = TOP - (TOPTOB/12.) + (TOPTOB/100.)
        ELSE
          TOP = 0.767 - (TOPTOB/12.) - (TOPTOB/100.)
          BOT = TOP - (TOPTOB/12.)
        ENDIF
        CALL PUVPRT( -1.0, 1.0, BOT, TOP )
        CALL PUOPEN
        CALL PXCOLR( TITCLR )
        CALL JJUST(2,2)
        YXRAT=8.0*6.0/ABS(YVPRT2-YVPRT1)
        CALL PUVSTR( -.9,0.0,1.0,YXRAT,LABELS(WIRE))
        CALL JRCLOS
        CALL PUVPRT( -.8, .8, BOT, TOP )
        IF(TRACE_TYPE.NE.2 .AND. TRACE_TYPE.NE.3) THEN
          CALL PFUHIS( LFADC, Y, TITCLR, HITCLR, Z, TZ, NZ )
        ENDIF
C
        IF(PSHLEV.LT.2) GOTO 10
        IMAX = 30
        IF(RUNTYPE.EQ.0) IMAX = 20
        DO 40 IZ=1,NZ
          LEADING_EDGE = Z(IZ)
          IPEV = NINT(LEADING_EDGE) - 3
          IF(IPEV.LT.1) IPEV=1
          IF(IPEV.GT.(LFADC-30)) IPEV=LFADC-30
          DO 50 IBIN=IPEV,IPEV+IMAX
            FBIN = FLOAT(IBIN) - LEADING_EDGE
            NEW_FADC = FVGAUSS_TAIL(FBIN,RT(IZ))*PK(IZ)*UPSIDN*PULFAC
            Y_SUB(IBIN) = Y_SUB(IBIN) - NEW_FADC
            Y_PK(IBIN) = Y_PK(IBIN) + NEW_FADC
            IF( Y_SUB(IBIN)*UPSIDN .LT. 0.0 ) Y_SUB(IBIN) = 0.0
   50     CONTINUE
   40   CONTINUE
        CALL FGTLPD(HALF,UNIT,QUAD,SECTOR,WIRE,PED(1),PED(2))
        DO IBIN=1,LFADC
          IF(Y_PK(IBIN).NE.0.0) Y_PK(IBIN)=Y_PK(IBIN)+PED(1)*UPSIDN
        ENDDO
        CALL PUVPRT( -.8, .8, BOT, TOP )
        IF(TRACE_TYPE.EQ.2) THEN
          CALL PFUHIS( LFADC, Y_SUB, TITCLR, HITCLR2, Z, TZ, NZ+50 )
        ELSEIF (TRACE_TYPE.EQ.4) THEN
          CALL PFUHIS( LFADC, Y_SUB, TITCLR, HITCLR3, Z, TZ, NZ+100 )
        ELSEIF (TRACE_TYPE.EQ.6) THEN
          CALL PFUHIS( LFADC, Y_SUB, TITCLR, HITCLR2, Z, TZ, NZ+100 )
        ENDIF
        IF(TRACE_TYPE.EQ.3) THEN
          CALL PFUHIS( LFADC, Y_PK, TITCLR, HITCLR2, Z, TZ, NZ+50 )
        ELSEIF (TRACE_TYPE.EQ.5) THEN
          CALL PFUHIS( LFADC, Y_PK, TITCLR, HITCLR3, Z, TZ, NZ+100 )
        ELSEIF (TRACE_TYPE.EQ.6) THEN
          CALL PFUHIS( LFADC, Y_PK, TITCLR, HITCLR3, Z, TZ, NZ+100 )
        ENDIF
C
   10 CONTINUE
C
C ****  Reset RCP bank
C
  999 CONTINUE
      CALL PUSETV( 'STRING QUALITY', ISTRQU)
      CALL EZRSET
      CALL PATHRS()
C-----------------------------------------------------------------------
      RETURN
      END
