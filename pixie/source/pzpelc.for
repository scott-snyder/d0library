      SUBROUTINE PZPELC(LPELC)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : to draw tracks according to PELC bank
C-
C-   Inputs  : LPELC: PELC bank address (LPELC=0 means to draw all PELC
C-                    tracks)
C-   Outputs : none
C-
C-   Created  20-JUL-1990   Qizhong Li-Demarteau
C-   Updated  20-NOV-1990   Lupe Howell  PIXIE Compack implementation 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:GRAPHF77.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:ZTRLNK.INC'
      INCLUDE 'D0$LINKS:IZPELC.LINK'
      INCLUDE 'D0$LINKS:IZVERT.LINK'
      INCLUDE 'D0$LINKS:IZZFIT.LINK'
      INTEGER LPELC, ZLINKR(250), NZTR
      INTEGER IFZTRK, IFZVTX, IFZCDC, IFZTRD
      INTEGER GZPELC, GZVERH
      INTEGER PLPELC, LVERH, LVERT, LVTXT, LDTRK, LZFIT, KP, I
      INTEGER LDRFT, LVRFT, IZTRK, PLZTRK, PLCALC, LTRDT, NPELC
      INTEGER IER,ITYP
      REAL    ZVTX, PT, RMIN, RMAX, DRMIN, DRMAX, XHPOS, YHPOS, RMXVTX
      REAL    PHI, DLTPHI, PHIMIN, PHIMAX, THETA, DLTTHE, THEMIN, THEMAX
      REAL    FITVTX(3), ERRVTX(3)
      CHARACTER*4 REM,CVAL
      LOGICAL EZERROR
      DATA    DLTPHI/0.15/, DLTTHE/0.5/
C----------------------------------------------------------------------
C
C ****  Pick correct RCP bank
C
      CALL EZPICK('PX_CALDIS_RCP')
      IF ( EZERROR(IER) ) THEN
        CALL ERRMSG('PIXIE','PZPELC','Cannot find PX_CALDIS_RCP','W')
        GOTO 999
      ENDIF
C
C ****  Get some CALDIS constants
C
      CALL EZ_GET_ARRAY('CALDIS_PXPARAMS','PELC ZTRK TRACK',1,IFZTRK,
     &     CVAL,ITYP,REM,IER)
      CALL EZ_GET_ARRAY('CALDIS_PXPARAMS','PELC VTX TRACK',1,IFZVTX,
     &     CVAL,ITYP,REM,IER)
      CALL EZ_GET_ARRAY('CALDIS_PXPARAMS','PELC TRD TRACK',1,IFZTRD,
     &     CVAL,ITYP,REM,IER)
      CALL EZ_GET_ARRAY('CALDIS_PXPARAMS','PELC CDC TRACK',1,IFZCDC,
     &     CVAL,ITYP,REM,IER)
C
C ****  Initialize a temporary LINK area
C
      IF (ZTRLNK(1) .EQ. 0) 
     &  CALL MZLINT(IXCOM,'/ZTRLNK/',ZTRLNK,ZLINKS(250),ZTRLNK)
C
C  draw VTX, TRD and CDC geometry
C      
      CALL PVTSEC_GEO(0,31)
      CALL PTRDXY_GEO
      CALL PDXYVW_GEO
C
C   get some geometry number from STP bank
C
      LDRFT = LC(LDGEH - 3)
      RMAX = C(LDRFT+17) + C(LDRFT+8)
      LVRFT = LC(LVGEH - 3)
      RMIN = C(LVRFT+7) - C(LVRFT+5)
      RMXVTX = C(LVRFT+21) + C(LVRFT+19)
C
C  draw tracks associated to PELC
C
      NPELC = 0
      IF (LPELC .EQ. 0) THEN
        PLPELC = GZPELC()
      ELSE
        PLPELC = LPELC
      ENDIF
  101 IF (PLPELC .EQ. 0) GOTO 900 
      PLCALC = LQ(PLPELC -2)
      IF (PLCALC .LE. 0) GOTO 900 
      PLZTRK = LQ(PLCALC - 5)
      IF (PLZTRK .LE. 0) GOTO 900
      LDTRK = LQ(PLZTRK-7)
      IF (LDTRK .EQ. 0) RMAX = RMXVTX
      LZFIT = LQ(PLZTRK - IZZFIT)
      IF (LZFIT .LE. 0) THEN 
        PHI = Q(PLPELC+10)
        THETA = Q(PLPELC+8)
        PHIMIN = PHI - DLTPHI
        PHIMAX = PHI + DLTPHI
        THEMIN = THETA - DLTTHE
        THEMAX = THETA + DLTTHE
        PT = Q(PLPELC+7)
        LVERH = GZVERH()
        LVERT = LQ(LVERH - IZVERT)
        IF (LVERT .GT. 0) THEN
          CALL VZERO(FITVTX,3)
          CALL VZERO(ERRVTX,3)
          CALL UCOPY(Q(LVERT+3),FITVTX,3)
          CALL UCOPY(Q(LVERT+6),ERRVTX,3)
        ELSE
          ERRVTX(1) = 9999.0
        ENDIF
        ERRVTX(1) = 9999.0  ! temporarily force the fit without the vertex 
        CALL 
     &    ZTRAKS(FITVTX(3),PHIMIN,PHIMAX,THEMIN,THEMAX,PT,NZTR,ZLINKR)
        CALL UCOPY(ZLINKR(1),ZLINKS(1),NZTR)
        DO 200 IZTRK = 1, NZTR
          IF (ZLINKS(IZTRK) .EQ. PLZTRK) THEN
            CALL ZTRKFT(ZLINKS(IZTRK),FITVTX,ERRVTX)
            LZFIT = LQ(ZLINKS(IZTRK) - IZZFIT)
            IF (LZFIT .LE. 0) GOTO 900 
            PLZTRK = ZLINKS(IZTRK)
            GOTO 201
          ENDIF
  200   CONTINUE
        GOTO 900 
      ENDIF
  201 CALL PUOPEN
      CALL PXCOLR('RED')
      KP = LZFIT
      DRMAX = RMAX - SQRT(Q(KP+11)**2 + Q(KP+12)**2)
      DRMIN = RMIN - SQRT(Q(KP+11)**2 + Q(KP+12)**2) 
      XHPOS = Q(KP+11) + DRMAX * COS(Q(KP+10))
      YHPOS = Q(KP+12) + DRMAX * SIN(Q(KP+10))
      CALL JMOVE(XHPOS, YHPOS)
      XHPOS = Q(KP+11) + DRMIN * COS(Q(KP+10))
      YHPOS = Q(KP+12) + DRMIN * SIN(Q(KP+10))
      CALL JDRAW( XHPOS, YHPOS )
      CALL JRCLOS
      IF (IFZVTX .NE. 0) THEN
        LVTXT = LQ(PLZTRK - 6)
        CALL PV1TRK(LVTXT)
      ENDIF
      IF (IFZTRD .NE. 0) THEN
        LTRDT = LQ(PLZTRK - 9)
        CALL PT1TRK(LTRDT)
      ENDIF
      IF (IFZCDC .NE. 0) THEN
        LDTRK = LQ(PLZTRK - 7)
        CALL PD1TRK(LDTRK)
      ENDIF
      IF (LPELC .EQ. 0) THEN
        NPELC = NPELC + 1
        PLPELC = GZPELC()
        DO 102 I = 1, NPELC
          PLPELC = LQ(PLPELC)
          IF (PLPELC .LE. 0) GOTO 900 
  102   CONTINUE
        GOTO 101
      ENDIF
C
C  deactivate the temporary link area
C
      ZTRLNK(1) = 0
C
  900 CONTINUE
C
C ****  Reset RCP bank
C
      CALL EZRSET
  999 RETURN
      END
