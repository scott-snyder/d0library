      SUBROUTINE PZVCRT
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Shows the raw FADC data from 16 channels
C-
C-   Inputs  : none
C-   Outputs : drawn traces
C-
C-   Created  27-SEP-1990   Jeffrey Bantly
C-   Updated  12-OCT-1990   Susan K. Blessing   
C-   Updated  23-JAN-1991   Jeffrey Bantly  add return if monte carlo data 
C-   Updated   4-MAR-1991   Lupe Howell  Implementing PIXIE using COMPACK
C-   Updated  30-APR-1991   Jeffrey Bantly  cleanup using new Compack 
C-   Updated  18-JUL-1991   Lupe Howell  Check for errors 
C-   Updated  22-JUL-1991   Robert E. Avery  More cleanup 
C-   Updated  25-JUN-1992   Robert E. Avery  Use flag for "SET SCALE" 
C-   Updated  23-MAR-2004   compile with g77.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:PXPARA.INC'
      INCLUDE 'D0$PARAMS:FDPARA.PARAMS'
C
      INTEGER I, J, K
      INTEGER ISTRQU, TZ(10), NZ, ICDD, FCRATE, FCARD
      INTEGER SCRATE, SCARD, UPRLWR, SCRATE1, SCRATE2
      INTEGER LCHN(0:15), FIXSCALE, VSCALE
      INTEGER EVDATA(0:LFADC-1), LEN, II, JJ
      INTEGER IER, ID
C
      REAL PEDESTAL
      PARAMETER( PEDESTAL =  8.0 )      ! typical pedestal
      REAL Y(LFADC), YWID, TOP, BOT, TOPTOB
      REAL XW1, XW2, YW1, YW2, Z(10)                
C
      LOGICAL EZERROR
      LOGICAL FLGVAL,HARDCOPY
C
      CHARACTER*4 TITCLR,HITCLR,GENCLR,CUPRLWR(0:1)
      CHARACTER*9 LABEL
      CHARACTER*50 TEXTE
      CHARACTER*60 ANSWER
      CHARACTER*80 STRING
      CHARACTER*80 PROM
C
      DATA Y /LFADC*0./
      DATA EVDATA / LFADC*0/
      DATA Z/10*0./
      DATA TZ/10*0/
      DATA CUPRLWR/' UPR',' LWR'/
      DATA PROM /
     &  ' Enter vertical scale (1-1000,return=prev or 1,0=prefixed>'/ 
C
C-----------------------------------------------------------------------
C
C ****  Selecting the PIXIE RCP file
C
      CALL EZPICK('PX_CD_ELECTRONICSDIS_RCP')
      IF ( EZERROR(IER) ) THEN
        CALL EZPICK('PX_FDCDIS_RCP')
        IF ( EZERROR(IER) ) THEN
          CALL ERRMSG('PIXIE','PZVCRT',
     &    'Cannot find PX_FDCDIS_RCP','W')
          GOTO 999
        ELSE
          CALL INTMSG(
     &    '   PX_FDCDIS_RCP bank used instead')
        ENDIF
      ENDIF
C
      IF(IQ(LHEAD+1) .GT. 1000) THEN
        CALL INTMSG(' Monte Carlo data is not in electronic crates')
        GOTO 990
      ENDIF
C
      CALL PZCRATE_INFO(ICDD,FCRATE,FCARD,LCHN)
C
      IF (ICDD.EQ.-1) GO TO 990
C
      IER = 0
      CALL ZCRATE_CODER(FCRATE,FCARD,SCRATE,SCARD,UPRLWR,IER,1) ! F->S
      IF(IER.LT.0) GOTO 990
C
      CALL PUGETA( 'ELECT COLR LABELS', TITCLR )
      CALL PUGETA( 'ELECT COLR HITS', HITCLR )
      CALL PUGETV( 'STRING QUALITY', ISTRQU)
      IF (ISTRQU .LT. 3) CALL PUSETV( 'STRING QUALITY', 3)
C
      HARDCOPY = FLGVAL('HARDCOPY')
      IF ( .NOT.HARDCOPY ) THEN
        CALL PUGETV( 'ELECT FIX SCALE', FIXSCALE)
        IF (FIXSCALE.LE.0 .OR. FIXSCALE.GT.1000) FIXSCALE = 400.
        CALL PUGETV( 'ELECT VERT SCALE', VSCALE)
        ANSWER=' '
        CALL GETPAR(1,PROM,'U',ANSWER)
        CALL SWORDS(ANSWER,II,JJ,LEN)
        vscale = fixscale
        IF(LEN.NE.0) READ(ANSWER(1:LEN),*,ERR=100) VSCALE
 100    continue
        IF (VSCALE.LE.0 .OR. VSCALE.GT.1000) THEN
          VSCALE = FIXSCALE
        ENDIF
        CALL PUSETV( 'ELECT FIX SCALE', FIXSCALE)
        CALL PUSETV( 'ELECT VERT SCALE', VSCALE)
      ENDIF
      CALL FLGSET( 'PF_SET_SCALE',.TRUE.)
C
C   Label top of display
C
      CALL PUOPEN
      CALL PXCOLR( TITCLR )
      CALL JJUST( 2, 2)
      WRITE( TEXTE, 1006 ) FCRATE, FCARD
 1006 FORMAT(' FADC Crate',I4,'        Card ',I3)
      CALL PUVSTR( 0., YWIND2*.95, 1.5, 1.5, TEXTE )
      SCRATE1 = SCRATE/10
      SCRATE2 = SCRATE - 10*SCRATE1
      IF(SCRATE2.LT.0 .OR. SCRATE2.GT.2) SCRATE2 = 9
      WRITE( TEXTE, 1007 ) SCRATE1, SCRATE2, SCARD, CUPRLWR(UPRLWR)
 1007 FORMAT(' Shaper Crate',I3,'-',I1,'     Card ',I3,'   ',A4,' Half')
      CALL PUVSTR( 0., YWIND2*.88, 1.5, 1.5, TEXTE )
      CALL JRCLOS
C
C   Loop over channels in full FADC card
C
      DO 50 ID = 0, 15
        CALL VZERO(Y,LFADC)
        CALL VZERO(EVDATA(0),LFADC)
C Unpack all CDD banks
        CALL ZDEXPD(0,LCHN(ID),EVDATA)
C "Unzerosuppress" data 
        CALL FUNSUP(PEDESTAL,EVDATA,Y)
C
        CALL PUVPRT(-1.,1.,YVPRT1,YVPRT2)
        CALL J4RGET(1,XW1,XW2,YW1,YW2)
        CALL JWINDO(-1.,1.,YVPRT1,YVPRT2)
        CALL J4RGET(1,XW1,XW2,YW1,YW2)
        CALL PUOPEN
        CALL JJUST(2,2)
        TOPTOB = ABS(YVPRT2 - YVPRT1)
        CALL PXCOLR( TITCLR )
        TOP = YVPRT2 - (ID)*(TOPTOB/16.)
        BOT = TOP - (TOPTOB/17.)
        WRITE(LABEL,101) (FCARD*16+ID)
  101   FORMAT(' Chan',I4)
        CALL PUVSTR(-.9,(TOP+BOT)/2.,1.0,1.5,LABEL)
        WRITE(LABEL,102) ID
  102   FORMAT(I4)
        CALL PUVSTR(+.9,(TOP+BOT)/2.,1.0,1.5,LABEL)
        CALL JRCLOS
        CALL PUVPRT( -.8, .8, BOT, TOP )
        NZ = 0
        CALL PFUHIS( LFADC, Y, TITCLR, HITCLR, Z, TZ, NZ )
C
   50 CONTINUE
C
C-----------------------------------------------------------------------
  990 CONTINUE
      CALL FLGSET('PF_SET_SCALE',.FALSE.)
      CALL PUSETV( 'STRING QUALITY', ISTRQU)
      CALL EZRSET
  999 RETURN
      END
