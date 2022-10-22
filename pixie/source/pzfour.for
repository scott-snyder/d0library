      SUBROUTINE PZFOUR
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Shows the raw FADC data and its Fourier
C-                         transform for one channel
C-
C-   Inputs  : none
C-   Outputs : drawn traces
C-
C-   Created  12-OCT-1990   Susan K. Blessing
C-   Updated  02-APR-1992   Susan K. Blessing   Use INTMSG and GETPAR
C-    rather than PFUMES and PFRSTR.
C-   Updated  25-JUN-1992   Robert E. Avery  Eliminate GEN color 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:PXPARA.INC'
      INCLUDE 'D0$PARAMS:FDPARA.PARAMS'
C
      INTEGER I, J, K, M
      INTEGER ISTRQU, TZ(10), NZ, ICDD, FCRATE, FCARD, FCHAN
      INTEGER SCRATE, SCARD, UPRLWR, SCRATE1, SCRATE2
      INTEGER LCHN(0:15)
      INTEGER EVDATA(0:LFADC-1)
      INTEGER IER
      INTEGER NBIN
C
      REAL OFFSET(2)
      REAL Y(LFADC), TOP, BOT, TOPTOB
      REAL Y1(256)
      REAL XW1, XW2, YW1, YW2, Z(10)
      REAL FT(256)
      REAL YMIN
C
      COMPLEX C(128)
      EQUIVALENCE(C,Y1)
C
      LOGICAL FIRST
C
      CHARACTER*4 TITCLR,HITCLR,CUPRLWR(0:1)
      CHARACTER*9 LABEL
      CHARACTER*50 TEXTE
      CHARACTER*60 PROM
      CHARACTER*80 STRING
C
      DATA Y /LFADC*0./
      DATA EVDATA / LFADC*0/
      DATA FIRST/.TRUE./
      DATA CUPRLWR/' UPR',' LWR'/
C
      DATA M/-8/
C
C-----------------------------------------------------------------------
C
      IF(FIRST) THEN
        CALL EZPICK('CD_ELECTRONICS_RCP')
        CALL EZGET('OFFSET',OFFSET,IER)
        CALL EZRSET
        FIRST = .FALSE.
      ENDIF
C
      CALL PUOPEN
      CALL INTMSG('/')
C
      CALL CRATE_INFO(ICDD,FCRATE,FCARD,LCHN)
C
      IF (ICDD.EQ.-1) GO TO 900
C
C ASK WHICH CHANNEL IN THIS CARD
      PROM = ' Display which channel? (0-15) (99=Back to Menu)>'
      CALL GETPAR(1,PROM,'I',FCHAN) 
      IF(FCHAN .EQ. 99) GOTO 900
      IF(FCHAN .LT. 0 .OR. FCHAN .GT. 15 ) GOTO 800
C
C FADC TO SHAPER
      IER = 0
      CALL ZCRATE_CODER(FCRATE,FCARD,SCRATE,SCARD,UPRLWR,IER,1)
      IF(IER.LT.0) GOTO 900
C
      CALL PUGETA( 'ELECT COLR LABELS', TITCLR )
      CALL PUGETA( 'ELECT COLR HITS', HITCLR )
      CALL PUGETV( 'STRING QUALITY', ISTRQU)
      IF (ISTRQU .LT. 3) CALL PUSETV( 'STRING QUALITY', 3)
C
      CALL JRCLOS
      CALL JPURGE(MAXSEG)
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
      IF (SCRATE2.LT.0 .OR. SCRATE2.GT.2) SCRATE2 = 9
      WRITE( TEXTE, 1007 ) SCRATE1, SCRATE2, SCARD, CUPRLWR(UPRLWR)
 1007 FORMAT(' Shaper Crate',I3,'-',I1,'     Card ',I3,'   ',A4,' Half')
      CALL PUVSTR( 0., YWIND2*.88, 1.5, 1.5, TEXTE )
      CALL JRCLOS
C
C DRAW DATA
      CALL VZERO(Y,LFADC)
      CALL VZERO_i(EVDATA(0),LFADC)
C Unpack all CDD banks
      CALL ZDEXPD(0,LCHN(FCHAN),EVDATA)
C "Unzerosuppress" data
      CALL FUNSUP(OFFSET(1),EVDATA,Y)
C
C USE FIRST 256 BINS ONLY
C
      DO I = 1, 256
        Y1(I) = Y(I)
      END DO
C
      CALL PUVPRT(-1.,1.,YVPRT1,YVPRT2)
      CALL J4RGET(1,XW1,XW2,YW1,YW2)
      CALL JWINDO(-1.,1.,YVPRT1,YVPRT2)
      CALL J4RGET(1,XW1,XW2,YW1,YW2)
      CALL PUOPEN
      CALL JJUST(2,2)
      TOPTOB = ABS(YVPRT2 - YVPRT1)
      CALL PXCOLR( TITCLR )
      TOP = YVPRT2 - 0.25*(TOPTOB/4.)
      BOT = TOP - (TOPTOB/5.)
      WRITE(LABEL,101) (FCARD*16+FCHAN)
  101 FORMAT(' Chan',I4)
      CALL PUVSTR(-.9,(TOP+BOT)/2.,1.0,1.5,LABEL)
      WRITE(LABEL,102) FCHAN
  102 FORMAT(I4)
      CALL PUVSTR(+.9,(TOP+BOT)/2.,1.0,1.5,LABEL)
      CALL JRCLOS
      CALL PUVPRT( -.8, .8, BOT, TOP )
      NZ = 0
      CALL PFUHIS( 256, Y1, TITCLR, HITCLR, Z, TZ, NZ )
C
C DO FOURIER TRANSFORM
      CALL RFFT(C,M)
C
      DO J = 1, 128
        FT(J) = REAL(C(J))*REAL(C(J)) + AIMAG(C(J))*AIMAG(C(J))
      END DO
C
C DRAW FOURIER TRANSFORM ON LOG SCALE
C
      CALL PUVPRT(-1.,1.,YVPRT1,YVPRT2)
      CALL J4RGET(1,XW1,XW2,YW1,YW2)
      CALL JWINDO(-1.,1.,YVPRT1,YVPRT2)
      CALL J4RGET(1,XW1,XW2,YW1,YW2)
      CALL PUOPEN
      CALL JJUST(2,2)
      TOPTOB = ABS(YVPRT2 - YVPRT1)
      CALL PXCOLR( TITCLR )
      TOP = YVPRT2 - 1.75*(TOPTOB/4.)
      BOT = TOP - (TOPTOB/5.)
      CALL PUVSTR(-.88,(TOP+BOT)*(-.22),1.0,1.5,'FOURIER')
      CALL PUVSTR(-.88,(TOP+BOT)*.01,1.0,1.5,'TRANSFORM')
      CALL PUVSTR(-.88,(TOP+BOT)*.75,1.0,1.5,'LOG SCALE')
      CALL PUVSTR(-.88,(TOP+BOT)*1.37,1.0,1.5,'10**-5')
      CALL PUVSTR(-.58,(TOP+BOT)*1.55,1.0,1.5,'0')
      CALL PUVSTR(-.33,(TOP+BOT)*1.55,1.0,1.5,'10')
      CALL PUVSTR(-.07,(TOP+BOT)*1.55,1.0,1.5,'20')
      CALL PUVSTR(.19,(TOP+BOT)*1.55,1.0,1.5,'30')
      CALL PUVSTR(.45,(TOP+BOT)*1.55,1.0,1.5,'40')
      CALL PUVSTR(.71,(TOP+BOT)*1.55,1.0,1.5,'50')
      CALL JRCLOS
      CALL PUVPRT( -.8, .8, BOT, TOP )
      NZ = 5
      DO I = 1, 5
        TZ(I) = 2
        Z(I) = 128./53. * 10. * FLOAT(I)
      END DO
      NBIN = 128
      YMIN = -5.
      CALL PFULOG(NBIN,FT,TITCLR,HITCLR,Z,TZ,NZ,YMIN)
C
C DRAW FOURIER TRANSFORM ON LINEAR SCALE
C
      CALL PUVPRT(-1.,1.,YVPRT1,YVPRT2)
      CALL J4RGET(1,XW1,XW2,YW1,YW2)
      CALL JWINDO(-1.,1.,YVPRT1,YVPRT2)
      CALL J4RGET(1,XW1,XW2,YW1,YW2)
      CALL PUOPEN
      CALL JJUST(2,2)
      TOPTOB = ABS(YVPRT2 - YVPRT1)
      CALL PXCOLR( TITCLR )
      TOP = YVPRT2 - 3*(TOPTOB/4.)
      BOT = TOP - (TOPTOB/5.)
      CALL PUVSTR(-.88,(TOP+BOT)*.4,1.0,1.5,'FOURIER')
      CALL PUVSTR(-.88,(TOP+BOT)*.43,1.0,1.5,'TRANSFORM')
      CALL PUVSTR(-.86,(TOP+BOT)*.55,1.0,1.5,'LINEAR SCALE')
      CALL PUVSTR(-.58,(TOP+BOT)*.67,1.0,1.5,'0')
      CALL PUVSTR(-.33,(TOP+BOT)*.67,1.0,1.5,'10')
      CALL PUVSTR(-.07,(TOP+BOT)*.67,1.0,1.5,'20')
      CALL PUVSTR(.19,(TOP+BOT)*.67,1.0,1.5,'30')
      CALL PUVSTR(.45,(TOP+BOT)*.67,1.0,1.5,'40')
      CALL PUVSTR(.71,(TOP+BOT)*.67,1.0,1.5,'50')
      CALL JRCLOS
      CALL PUVPRT( -.8, .8, BOT, TOP )
      NBIN = 128
      DO I = 1, 5
        TZ(I) = 999
      END DO
      CALL PFUHIS(NBIN,FT,TITCLR,HITCLR,Z,TZ,NZ)
C
      GOTO 999
  800 CONTINUE
      CALL INTMSG(' Improper value.')
  900 CONTINUE
      CALL JRCLOS
C
C-----------------------------------------------------------------------
  999 CONTINUE
      CALL PUSETV( 'STRING QUALITY', ISTRQU)
      RETURN
      END
