      SUBROUTINE PESUM_TRACK
C---------------------------------------------------------------------
C-
C-   Purpose and Methods :DRAWS ESUM OBJECT TRACKS
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  29-MAR-1993   Vipin Bhatnagar
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$PARAMS:ESUM.PARAMS'
      INCLUDE 'D0$INC:PI.DEF'
      INTEGER NFOUND(ID_ALL:LAST_TYPE),IFLAG,IER
      INTEGER PID
      INTEGER NVTX,NJET,NMUO,NELE,NPHO,NTAU,NMIS,NSUM
      INTEGER IETA,IP,IE,I,IOK
      REAL    PJ,PXYZ(3),THEIAP,P,PTMIN,XC(3)
      REAL    ET,ETA,THETA,PHI,LTRK,XVSV(3),ETMIN
      REAL    VERTZ(3),VERTX(3),VERTY(3),THETAP
      REAL    ARRAY(NPHIL,2*NETAL),DPT,RLEVEL
      CHARACTER*4  CLEVEL,PATHNAM
      CHARACTER*32 PLTITL
      LOGICAL EZERROR,PU_PICK_ACTIVE
      DATA LTRK /300./
C----------------------------------------------------------------------
      CALL JIQDIL(RLEVEL)
C-
C--- Check do picking...
C-
      IF ( PU_PICK_ACTIVE() ) THEN
        CALL PICK_ESUM
        IF( RLEVEL .EQ. -2. )   CALL PX_PICK_QUIT
        GO TO 999
      ENDIF

C--- Clear Array
      IOK = 0
C
C ****  Make sure we pick correct bank
C
      CALL EZPICK('PX_PHYDIS_RCP')          ! Selecting PHYDIS bank
      IF ( EZERROR(IER) ) THEN
        CALL ERRMSG('PIXIE','PESUM_TRACK','Bank PX_PHYDIS_RCP NOT 
     &    FOUND','W')
        GOTO 999
      ENDIF
C-
C--- CHECK ESUM BANK IS AVAILABLE OR NOT...
C-
      CALL PUGETV('PHYDIS ETMIN',ETMIN)
      PTMIN   = ETMIN
      CALL PUGETA('PHYDIS ESUM PATH',CLEVEL)
      PATHNAM = CLEVEL
      PLTITL= ' ESUM('//PATHNAM//') TRACKS '
      CALL GTESUM_COUNTS(CLEVEL,NFOUND,IER)
      IF (IER .NE. 0) THEN
        CALL ERRMSG('PIXIE','PU_GET_ESUM','Bank ESUM NOT FOUND','W')
        IOK = 1
        GO TO 999
      ENDIF
C-
      CALL PUOPEN
C-
C--- Draw X, Y and Z axes
      CALL PXLWID(2)
      CALL PXCOLR('YEL')
      CALL J3MOVE(0., 0., 0.)
      CALL JR3DRA(240., 0., 0.)
      CALL JSIZE(10.,10.)
      CALL J3STRG('X')
      CALL J3MOVE(0., 0., 0.)
      CALL JR3DRA(0., 240., 0.)
      CALL JSIZE(10.,10.)
      CALL J3STRG('Y')
      CALL J3MOVE(0., 0., 0.)
      CALL JR3DRA(0., 0., 380.)
      CALL JSIZE(10.,10.)
      CALL JBASE( 0., 0., 1.)
      CALL JPLANE( 0., 1., 0.)
      CALL J3STRG('Z')
C-
C=== VERTEX
C-
      NVTX = 0
      DO I = 1,NFOUND(ID_VERTEX)
        CALL GTESUM(CLEVEL,ID_VERTEX,I,ET,ETA,THETA,PHI,IFLAG,IER)
        IF (IER .NE. 0) THEN
          GO TO 100
        ENDIF
        NVTX = NVTX + 1
        IF (NVTX .LE. 3) THEN
          VERTZ(NVTX) = PHI
          VERTX(NVTX) = ETA
          VERTY(NVTX) = THETA
        ENDIF
      ENDDO
      XVSV(1) = VERTX(1)
      XVSV(2) = VERTY(1)
      XVSV(3) = VERTZ(1)
C-
C=== JET
C-
  100 NJET = 0
      DO I = 1,NFOUND(ID_JET)
        CALL GTESUM(CLEVEL,ID_JET,I,ET,ETA,THETA,PHI,IFLAG,IER)
        IF (IER .NE. 0) THEN
          GO TO 200
        ENDIF
        NJET = NJET + 1
        THETAP = 2*ATAN(1/EXP(ETA))
        PJ   = ET/SIN(THETAP)
        PXYZ(3) = PJ*COS(THETAP)
        PXYZ(1) = ET*COS(PHI)
        PXYZ(2) = ET*SIN(PHI)
        PID     = 0
        CALL VZERO(XC,3)
        CALL PLESUMTK(PTMIN,PID,DPT,LTRK,PXYZ,PJ,XVSV,XC)
      ENDDO
C-
C----Muons
C-
  200 NMUO = 0
      DO I = 1,NFOUND(ID_MUON)
        CALL GTESUM(CLEVEL,ID_MUON,I,ET,ETA,THETA,PHI,IFLAG,IER)
        IF (IER .NE. 0) THEN
          GO TO 300
        ENDIF
        NMUO = NMUO + 1
        THETAP = 2*ATAN(1/EXP(ETA))
        P      = ET/SIN(THETAP)
        PXYZ(3) = P*COS(THETAP)
        PXYZ(1) = ET*COS(PHI)
        PXYZ(2) = ET*SIN(PHI)
        PID     = 14
        CALL VZERO(XC,3)
        CALL PLESUMTK(PTMIN,PID,DPT,LTRK,PXYZ,P,XVSV,XC)
      ENDDO
C-
C----Electrons
C-
  300 NELE = 0
      DO I = 1,NFOUND(ID_ELECTRON)
        CALL GTESUM(CLEVEL,ID_ELECTRON,I,ET,ETA,THETA,PHI,IFLAG,IER)
        IF (IER .NE. 0) THEN
          GO TO 400
        ENDIF
        NELE = NELE + 1
        THETAP = 2*ATAN(1/EXP(ETA))
        P      = ET/SIN(THETAP)
        PXYZ(3) = P*COS(THETAP)
        PXYZ(1) = ET*COS(PHI)
        PXYZ(2) = ET*SIN(PHI)
        PID     = 12
        CALL VZERO(XC,3)
        CALL PLESUMTK(PTMIN,PID,DPT,LTRK,PXYZ,P,XVSV,XC)
      ENDDO
C-
C-----Photons
C-
  400 NPHO = 0
      DO I = 1,NFOUND(ID_PHOTON)
        CALL GTESUM(CLEVEL,ID_PHOTON,I,ET,ETA,THETA,PHI,IFLAG,IER)
        IF (IER .NE. 0) THEN
          GO TO 500
        ENDIF
        NPHO = NPHO + 1
        THETAP = 2*ATAN(1/EXP(ETA))
        P      = ET/SIN(THETAP)
        PXYZ(3) = P*COS(THETAP)
        PXYZ(1) = ET*COS(PHI)
        PXYZ(2) = ET*SIN(PHI)
        PID     = 10
        CALL VZERO(XC,3)
        CALL PLESUMTK(PTMIN,PID,DPT,LTRK,PXYZ,P,XVSV,XC)
      ENDDO
C-
C------Taus
C-
  500 NTAU = 0
      DO I = 1,NFOUND(ID_TAU)
        CALL GTESUM(CLEVEL,ID_TAU,I,ET,ETA,THETA,PHI,IFLAG,IER)
        IF (IER .NE. 0) THEN
          GO TO 600
        ENDIF
        NTAU = NTAU + 1
        THETAP = 2*ATAN(1/EXP(ETA))
        P      = ET/SIN(THETAP)
        PXYZ(3) = P*COS(THETAP)
        PXYZ(1) = ET*COS(PHI)
        PXYZ(2) = ET*SIN(PHI)
        PID     = 16
        CALL VZERO(XC,3)
        CALL PLESUMTK(PTMIN,PID,DPT,LTRK,PXYZ,P,XVSV,XC)
      ENDDO
C-
C-----MISSING ET
C-
  600 NMIS = 0
      DO I = 1,NFOUND(ID_ETMISS)
        CALL GTESUM(CLEVEL,ID_ETMISS,I,ET,ETA,THETA,PHI,IFLAG,IER)
        IF (IER .NE. 0) THEN
          GO TO 998
        ENDIF
        NMIS = NMIS + 1
        THETAP = 2*ATAN(1/EXP(ETA))
        P      = ET/SIN(THETAP)
        PXYZ(3) = P*COS(THETAP)
        PXYZ(1) = ET*COS(PHI)
        PXYZ(2) = ET*SIN(PHI)
        PID     = -1
        CALL VZERO(XC,3)
        CALL PLESUMTK(PTMIN,PID,DPT,LTRK,PXYZ,P,XVSV,XC)
      ENDDO
C-
  998 CONTINUE
      CALL JRCLOS
      CALL LEGENDTK
C-
      CALL EZRSET
C-
C----PRINTING TITLE
C-
      CALL PXTITL(PLTITL)
C-
  999 RETURN
      END
