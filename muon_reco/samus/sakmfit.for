      SUBROUTINE SAKMFIT(IMUOT,IMUON,OK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : SAMUS Kalman fit routine.
C-                         Places output to MUKF bank
C-
C-   Inputs  :
C-                IMUON     --  Index of MUON bank (input)
C-   Outputs :
C-              (implicit)  --  MUKF bank
C-              OK          --  Error status (<=0 error, 1 OK, >1 warning)
C-   Controls:
C-
C-   Created  29-Dec-1994   Igor V. Mandrichenko
C-   Updated  23-Jan-1995   Igor V. Mandrichenko Add B*dL
C-   Updated  29-Jan-1995   I.M. Create 3 segments MUKF bank.
C-                          MUKF seg. no.       What ?
C-                          -------------       ------------
C-                          1                   No-vertex SAMUS fit (A+B+C)
C-                          2                   Vertex SAMUS fit (#1+vertex)
C-                          3                   Global fit (see SAGLOBAL)
C-   Updated  04-FEB-1995   I.M. New format of STTH
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
c      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:SAKFIT.INC'
      INCLUDE 'D0$LINKS:IZSTTH.LINK'
C
C------------------------------------ Parameters ----------------------
      INTEGER IMUON,IMUOT,OK
C
C------------------------------------ Constants -----------------------
      REAL    CL
      PARAMETER   (CL=0.29979)        ! For 10**6 Gauss
      INTEGER HITLEN
      PARAMETER   (HITLEN=8)
C
C------------------------------------ Local variables -----------------
      REAL    COV(5,5),FITPAR(5),INF(5,5),
     +          ZPNT(MUKA_MAXPNT),SIPNT(MUKA_MAXPNT),
     +          COPNT(MUKA_MAXPNT),UPNT(MUKA_MAXPNT),
     +          THPNT(MUKA_MAXPNT),SIGPNT(MUKA_MAXPNT),
     +          ELOS(MUKA_MAXPNT), PMOM,ZSTART,ZEND,
     +          VERTEX(3),VTXMEAS(5),VTXINF(5,5),
     +          SQBUF(8,20),PREFITIN(6),PREFITOUT(6),
     +          FITA(5),INFA(5,5),FITBC(5),INFBC(5,5),ZA,ZBC,
     +          ERRMTX(5,5,2),FITSAV(5,2),ZSAV(2),CHI2SAV(2),
     +          ELCAL,ELTOR
      INTEGER I,J,N,IDEB,IADDR,
     +          NH,IST,ISC,SKHST,
     +          IOK,
     +          IDIR,LMUON,GZMUON,LSTTH,LMUKF,
     +          IFIRST,LMUOT,GZMUOT,NHITSAV(2),SKFIT
      REAL    CHI2A,CHI2BC,DCHI2A,DCHI2V,
     +          BDL3(3),BDLSAV(2)

C<<
      DATA    IFIRST/1/
C<<
      IF(IFIRST.NE.0) THEN
        CALL SAKMFIT_INIT(0)
        CALL    EZPICK('SAMUS_UTIL_PARAM')
        CALL EZGET('SAKFIT',SKFIT,IOK)
        IF(IOK.NE.0) SKFIT = 2
        CALL EZGET('SAKHST',SKHST,IOK)
        IF(IOK.NE.0) SKHST = 0
        IF( SKHST .GT. 0 ) THEN
C<<
          IDEB = 14000
C<<
          CALL    HCDIR('//PAWC',' ')
          CALL    HMDIR('SAKFIT','S')
          CALL    HCDIR('//PAWC/SAKFIT',' ')
C<<
          CALL HBOOK1( IDEB+10, 'LOG COORDINATE ERR (BC FIT)',
     +                           100, -3., 1., 0.)
C<<
          CALL HBOOK1( IDEB+11, 'LOG SLOPE ERR (BC FIT)',
     +                           100, -5., -1., 0.)
C<<
          CALL HBOOK1( IDEB+12, 'LOG 1/P ERR (BC FIT, estimated)',
     +                           100, -4., 0., 0.)
C<<
          CALL HBOOK1( IDEB+15, 'Chi**2 (BC FIT)',
     +                           100, 0., 50., 0.)
C<<
          CALL HBOOK1( IDEB+20, 'LOG COORDINATE ERR (A FIT)',
     +                           100, -3., 1., 0.)
C<<
          CALL HBOOK1( IDEB+21, 'LOG SLOPE ERR (A FIT)',
     +                           100, -3., 1., 0.)
C<<
          CALL HBOOK1( IDEB+25, 'Chi**2 (A FIT)',
     +                           100, 0., 50., 0.)
C<<
          CALL HBOOK1( IDEB+30, 'LOG COORDINATE ERR (After magnet)',
     +                           100, -3., 1., 0.)
C<<
          CALL HBOOK1( IDEB+31, 'LOG SLOPE ERR (After magnet)',
     +                           100, -3., 1., 0.)
C<<
          CALL HBOOK1( IDEB+32, 'LOG 1/P ERR (After magnet)',
     +                           100, -4., 0., 0.)
C<<
          CALL HBOOK1( IDEB+40, 'LOG COORDINATE ERR (A fit added)',
     +                           100, -3., 1., 0.)
C<<
          CALL HBOOK1( IDEB+41, 'LOG SLOPE ERR (A fit added)',
     +                           100, -4., 0., 0.)
C<<
          CALL HBOOK1( IDEB+42, 'LOG 1/P ERR (A fit added)',
     +                           100, -4., 0., 0.)
C<<
          CALL HBOOK1( IDEB+45, 'Delta Chi**2 (+A FIT)',
     +                           100, 0., 50., 0.)
C<<
          CALL HBOOK1( IDEB+46, 'Chi**2 (A fit added)',
     +                           100, 0., 50., 0.)
C<<
          CALL HBOOK1( IDEB+50, 'LOG COORDINATE ERR (After cal.)',
     +                           100, -3., 1., 0.)
C<<
          CALL HBOOK1( IDEB+51, 'LOG SLOPE ERR (After cal.)',
     +                           100, -3., 1., 0.)
C<<
          CALL HBOOK1( IDEB+52, 'LOG 1/P ERR (After cal.)',
     +                           100, -4., 0., 0.)
C<<
          CALL HBOOK1( IDEB+60, 'LOG COORDINATE ERR (Vertex)',
     +                           100, -3., 1., 0.)
C<<
          CALL HBOOK1( IDEB+61, 'LOG SLOPE ERR (Vertex)',
     +                           100, -3., 1., 0.)
C<<
          CALL HBOOK1( IDEB+62, 'LOG 1/P ERR (Vertex)',
     +                           100, -4., 0., 0.)
C<<
          CALL HBOOK1( IDEB+65, 'Delta Chi**2 (+Vertex)',
     +                           100, 0., 50., 0.)
C<<
          CALL HBOOK1( IDEB+66, 'Chi**2 (Vertex)',
     +                           100, 0., 100., 0.)
        END IF
        CALL EZRSET
        IFIRST = 0
      ENDIF
C<<
      OK = 0
      IF( SKHST.GT.0 ) CALL   HCDIR('//PAWC/SAKFIT',' ')
C----------------------------------------------------------------------
C       Extract info from MUON,STTH
C----------------------------------------------------------------------
      LMUON = GZMUON(IMUON)
      IF( LMUON.LE.0 ) GOTO 999

      CALL BKMUKF(LMUON,3,1,LMUKF)
      IF( LMUKF.LE.0 ) GOTO 999
C<<
      LMUON = GZMUON(IMUON)
      IF( LMUON.LE.0 ) GOTO 999
      LMUOT = GZMUOT(IMUOT)
      IF( LMUOT.LE.0 ) GOTO 999
      LSTTH = LQ(LMUOT-IZSTTH)
      IF( LSTTH.LE.0 ) GOTO 999
C<<
      IDIR  = IQ(LMUOT + 3) - 12
      PMOM  = Q(LMUOT + 23)
      IF( ABS(PMOM).LT.10. ) PMOM = SIGN(10.,PMOM)
      ELCAL = Q(LMUOT + 25)
      ELTOR = Q(LMUOT + 26)
      IF( ABS(PMOM).LT.ELCAL+ELTOR ) PMOM = SIGN(10.,ELCAL+ELTOR+1.)
C-Reco 2 straight line fits
      CALL UCOPY(Q(LMUOT+8),PREFITIN(1),3)
      CALL UCOPY(Q(LMUOT+14),PREFITIN(4),3)
      CALL UCOPY(Q(LMUOT+11),PREFITOUT(1),3)
      CALL UCOPY(Q(LMUOT+17),PREFITOUT(4),3)
C-Hits
      NH = 0
      CALL    GTSTTH(IMUOT,LSTTH,NH,IST,ISC,IADDR,
     +                SQBUF,SQBUF)
      IF(NH.LE.0) GOTO 999
      IF(MOD(NH,10).LT.4) GOTO 999
      IF(NH/100+MOD(NH/10,10).LT.4) GOTO 999
      NH = NH/100 + MOD(NH/10,10) + MOD(NH,10)
c---------------------------------------------------------
c       MINSQ fit through B&C stations
c---------------------------------------------------------
      J = 1
      DO I = 1,NH
        CALL    GTSTTH(IMUOT,LSTTH,I,IST,ISC,IADDR,
     +                    SQBUF(1,J),SQBUF(7,J))
        IF(IST.EQ.2 .OR. IST.EQ.3 .OR.
     +                    IST.EQ.5 .OR. IST.EQ.6) THEN
          SQBUF(8,J) = SAMUS_SIGMA            ! Meas. error
          J = J + 1
        END IF
      END DO

      N = J-1
      IF( N.LT.4 ) GOTO 999
      CALL SKQFIT(PREFITOUT,SQBUF,N,ZBC,FITBC,INFBC,CHI2BC,IOK)
      IF(IOK.NE.0) GOTO 999
      FITBC(5)   =    1./SIGN(ABS(PMOM)-ELTOR-ELCAL,PMOM)
      INFBC(5,5) =    1./((FITBC(5)*0.4)**2+1.e-9)
c-----------------------------------------------------------
c DEBUG
c-----------------------------------------------------------
      CALL HF1(IDEB+15, CHI2BC, 1.)
      CALL UCOPY(INFBC,COV,25)
      CALL RSINV(5,COV,5,IOK)
      IF( IOK.NE.0 ) GOTO 999
      DO J=1,5
        CALL HF1(IDEB+10+(J-1)/2, 0.5*ALOG10(COV(J,J)), 1.)
      END DO
c---------------------------------------------------------
c       MINSQ fit through A station
c---------------------------------------------------------
      J = 1
      DO I = 1,NH
        CALL    GTSTTH(IMUOT,LSTTH,I,IST,ISC,IADDR,
     +                    SQBUF(1,J),SQBUF(7,J))
        IF(IST.EQ.1 .OR. IST.EQ.4 ) THEN
          SQBUF(8,J) = SAMUS_SIGMA            ! Meas. error
          J = J + 1
        END IF
      END DO

      N = J-1
      IF( N.LT.4 ) GOTO 999
      CALL SKQFIT(PREFITIN,SQBUF,N,ZA,FITA,INFA,CHI2A,IOK)
      IF(IOK.NE.0) GOTO 999
c-----------------------------------------------------------
c DEBUG
c-----------------------------------------------------------
      CALL HF1(IDEB+25, CHI2A, 1.)
      CALL UCOPY(INFA,COV,25)
      COV(5,5) = 1.
      CALL RSINV(5,COV,5,IOK)
      IF( IOK.NE.0 ) GOTO 999
      DO J=1,4
        CALL HF1(IDEB+20+(J-1)/2, 0.5*ALOG10(COV(J,J)), 1.)
      END DO

C-----------------------------------------------------------
C       Estimate momentum at the middle of the toroid
C-----------------------------------------------------------
C ???
C-----------------------------------------------------------
C       Set up MAGNET data
C-----------------------------------------------------------
      J = 1
      N = 0
      CALL SAKMAG(ABS(PMOM)-ELCAL,
     +        PREFITIN,PREFITOUT,ELTOR,
     +        IDIR,J-1,N,
     +        ZPNT,UPNT,SIPNT,COPNT,SIGPNT,THPNT,ELOS)
      J = N + 1
C<<
      CALL SKF_SORT_HITS(N,IDIR,ZPNT,UPNT,SIPNT,COPNT,
     +          SIGPNT,THPNT,ELOS)
C-----------------------------------------------------------
C       Go through magnet
C-----------------------------------------------------------
      CALL    ucopy(fitbc,fitpar,5)
      CALL    UCOPY(INFBC,INF,25)
      ZSTART = ZBC    ! FIT parameters are given at ...
      ZEND = ZA
C<<
      BDL3(1) = 0
      BDL3(2) = 0
      BDL3(3) = 0
C<<
      CALL    SAKLM3(ZSTART,ZEND,N,ZPNT,UPNT,SIPNT,COPNT,SIGPNT,
     +                THPNT,ELOS,
     +                FITPAR,INF,BDL3,IOK)
      IF(IOK.NE.1) GOTO 999
c-----------------------------------------------------------
c DEBUG
c-----------------------------------------------------------
      CALL UCOPY(INF,COV,25)
      CALL RSINV(5,COV,5,IOK)
      IF( IOK.NE.0 ) GOTO 999
      DO J=1,5
        CALL HF1(IDEB+30+(J-1)/2, 0.5*ALOG10(COV(J,J)), 1.)
      END DO
c-----------------------------------------------------------
c       Add A station
c-----------------------------------------------------------
      CALL    KFMRG2(FITPAR,INF, FITA,INFA, FITPAR,INF,
     +                DCHI2A, IOK)
      IF(IOK.NE.1) GOTO 999
c-----------------------------------------------------------
c DEBUG
c-----------------------------------------------------------
      CALL HF1(IDEB+45, DCHI2A, 1.)
      CALL HF1(IDEB+46, CHI2BC+CHI2A+DCHI2A, 1.)
      CALL UCOPY(INF,COV,25)
      CALL RSINV(5,COV,5,IOK)
      IF( IOK.NE.0 ) GOTO 999
      DO J=1,5
        CALL HF1(IDEB+40+(J-1)/2, 0.5*ALOG10(COV(J,J)), 1.)
      END DO
C-----------------------------------------------------------
C       Set up CALOR data
C-----------------------------------------------------------
      J = 1
      N = 0
      CALL SAKEC(ABS(PMOM)-ELCAL/2,
     +                    PREFITIN,ELCAL,
     +                    IDIR,J-1,N,
     +                    ZPNT,UPNT,SIPNT,COPNT,SIGPNT,
     +                    THPNT,ELOS)
      J = N + 1
C<<
      CALL SKF_SORT_HITS(N,IDIR,ZPNT,UPNT,SIPNT,COPNT,
     +          SIGPNT,THPNT,ELOS)
C-----------------------------------------------------------
C       Go through calorimeter
C-----------------------------------------------------------
      CALL SKF_GET_VTX(VERTEX)
C<<
      ZSTART = ZA    ! FIT parameters are given at ...
      ZEND = VERTEX(3)
C<<
      CALL    SAKLM3(ZSTART,ZEND,N,ZPNT,UPNT,SIPNT,COPNT,SIGPNT,
     +                THPNT,ELOS,
     +                FITPAR,INF,BDL3,IOK)
      IF(IOK.NE.1) GOTO 999
C<<
c-----------------------------------------------------------
c DEBUG
c-----------------------------------------------------------
      CALL UCOPY(INF,COV,25)
      CALL RSINV(5,COV,5,IOK)
      IF( IOK.NE.0 ) GOTO 999
      DO J=1,5
        CALL HF1(IDEB+50+(J-1)/2, 0.5*ALOG10(COV(J,J)), 1.)
      END DO
C<<
C-----------------------------------------------------------
C       Save no-vertex fit for MUKF
C-----------------------------------------------------------
      CALL UCOPY(INF,ERRMTX(1,1,1),25)
      CALL RSINV(5,ERRMTX(1,1,1),5,IOK)
      IF( IOK.NE.0 ) GOTO 999
      CALL UCOPY(FITPAR,FITSAV(1,1),5)
      ZSAV(1) = ZEND
      CHI2SAV(1) = CHI2BC + CHI2A + DCHI2A
      BDLSAV(1) = SQRT(BDL3(1)**2 + BDL3(2)**2 + BDL3(3)**2)*1.E-3
      NHITSAV(1) = NH
      CALL MUKFFL(LMUKF, 1, 5, NH, ZEND, CHI2BC+CHI2A+DCHI2A,
     +                BDLSAV(1), FITPAR, ERRMTX(1,1,1))
C<<
C--------------------------------------------------
C       Add vertex measurement
C--------------------------------------------------
      VTXMEAS(1) = VERTEX(1)
      VTXMEAS(2) = VERTEX(2)
C<<
      CALL    VZERO(VTXINF,25)
      VTXINF(1,1) = 1./VTX_SIGMA**2
      VTXINF(2,2) = 1./VTX_SIGMA**2
C<<
      CALL    KFMRG2(VTXMEAS,VTXINF,FITPAR,INF,FITPAR,INF,
     +            DCHI2V,IOK)
      IF(IOK.NE.1) GOTO 999
C<<
c-----------------------------------------------------------
c DEBUG
c-----------------------------------------------------------
      CALL HF1(IDEB+65, DCHI2V, 1.)
      CALL HF1(IDEB+66, CHI2BC+CHI2A+DCHI2A+DCHI2V, 1.)
      CALL UCOPY(INF,COV,25)
      CALL RSINV(5,COV,5,IOK)
      IF( IOK.NE.0 ) GOTO 999
      DO J=1,5
        CALL HF1(IDEB+60+(J-1)/2, 0.5*ALOG10(COV(J,J)), 1.)
      END DO
C-----------------------------------------------------------
C       Prepare vertex fit for MUKF
C-----------------------------------------------------------
      CALL UCOPY(INF,ERRMTX(1,1,2),25)
      CALL RSINV(5,ERRMTX(1,1,2),5,IOK)
      IF( IOK.NE.0 ) GOTO 999
      CALL UCOPY(FITPAR,FITSAV(1,2),5)
      ZSAV(2) = ZEND
      CHI2SAV(2) = CHI2SAV(1) + DCHI2V
C<<
      BDLSAV(2) = BDLSAV(1)
      NHITSAV(2) = NH+2
      CALL MUKFFL(LMUKF,2,5,NH+2,ZEND, CHI2BC+CHI2A+DCHI2A+DCHI2V,
     +                BDLSAV(1),FITPAR,ERRMTX(1,1,2))
C<<
      OK = 1  ! Finished without error
C<<
  999 RETURN
      END
