      SUBROUTINE PICK_ESUM
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :Allows user to pick 3D ESUM TRACK and
C-                        displays P, Pt and ID of the track
C-
C-   Inputs  :
C-   Outputs : Particle Id, P,Pt,Eta and Phi
C-   Controls:
C-
C-   Created  27-APR-1993   Vipin Bhatnagar
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$PARAMS:ESUM.PARAMS'
      INCLUDE 'D0$INC:PI.DEF'
      INTEGER NFOUND(ID_ALL:LAST_TYPE),IFLAG,IER
      INTEGER PID
      INTEGER NVTX,NJET,NMUO,NELE,NPHO,NTAU,NMIS,NSUM
      INTEGER IETA,IP,IE,I,IOK,J
      REAL    PJ,PXYZ(3),THEIAP,P,PTMIN,XC(3)
      REAL    ET,ETA,THETA,PHI,LTRK,XVSV(3),ETMIN
      REAL    VERTZ(3),VERTX(3),VERTY(3),THETAP
      REAL    DPT,RLEVEL,XPV(2)
      REAL    PSV,PTSV,SETA,SPHI
      REAL    DMIN,DIST
      CHARACTER*4  CLEVEL,PATHNAM
      CHARACTER*6  JET,MUON,ELEC,PHOT,TAU,MET
      CHARACTER*70 LINE(2)
      DATA LTRK /300./
C-
C----------------------------------------------------------------------
C
C ****  Get window coordinates of picked point
C
      CALL PU_GET_PICKV(XPV)
      DMIN=1.E6
C
C--- CHECK ESUM BANK IS AVAILABLE OR NOT...
C-
      CALL PUGETV('PHYDIS ETMIN',ETMIN)
      PTMIN   = ETMIN
      CALL PUGETA('PHYDIS ESUM PATH',CLEVEL)
      PATHNAM = CLEVEL
      CALL GTESUM_COUNTS(CLEVEL,NFOUND,IER)
      IF (IER .NE. 0) THEN
        CALL ERRMSG('PIXIE','PU_GET_ESUM','Bank ESUM NOT FOUND','W')
        IOK = 1
        GO TO 999
      ENDIF
C-
C=== VERTEX
C-
      CALL VZERO(XVSV,3)
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
        CALL PIDIST(PTMIN,LTRK,PXYZ,PJ,XVSV,XC,XPV,DIST)
        IF (DIST.LT.DMIN) THEN
          DMIN = DIST
          PSV  = PJ
          PTSV = ET
          SETA = ETA
          SPHI = PHI
        ENDIF
        IF (DMIN .LT. .02) THEN
          WRITE(LINE(1),2000)
 2000     FORMAT('   #    ID      P       PT      ETA      PHI ')
          WRITE(LINE(2),990)NJET,'JET',PSV,PTSV,SETA,SPHI
  990     FORMAT(I4,3X,A4,3F8.2,1X,F8.2)
          DO J = 1,2
            CALL INTMSG(LINE(J))
          ENDDO
          DMIN = 1.E6
        ENDIF
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
        CALL PIDIST(PTMIN,LTRK,PXYZ,P,XVSV,XC,XPV,DIST)
        IF (DIST.LT.DMIN) THEN
          DMIN = DIST
          PSV  = P
          PTSV = ET
          SETA = ETA
          SPHI = PHI
        ENDIF
        IF (DMIN .LT. .02) THEN
          WRITE(LINE(1),2001)
 2001     FORMAT('   #    ID      P       PT      ETA      PHI ')
          WRITE(LINE(2),991)NMUO,'MUON',PSV,PTSV,SETA,SPHI
  991     FORMAT(I4,3X,A4,3F8.2,1X,F8.2)
          DO J = 1,2
            CALL INTMSG(LINE(J))
          ENDDO
          DMIN = 1.E6
        ENDIF
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
        CALL PIDIST(PTMIN,LTRK,PXYZ,P,XVSV,XC,XPV,DIST)
        IF (DIST.LT.DMIN) THEN
          DMIN = DIST
          PSV  = P
          PTSV = ET
          SETA = ETA
          SPHI = PHI
        ENDIF
        IF (DMIN .LT. .02) THEN
          WRITE(LINE(1),2002)
 2002     FORMAT('   #    ID      P       PT      ETA      PHI ')
          WRITE(LINE(2),992)NELE,'ELEC',PSV,PTSV,SETA,SPHI
  992     FORMAT(I4,3X,A4,3F8.2,1X,F8.2)
          DO J = 1,2
            CALL INTMSG(LINE(J))
          ENDDO
          DMIN = 1.E6
        ENDIF
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
        CALL PIDIST(PTMIN,LTRK,PXYZ,P,XVSV,XC,XPV,DIST)
        IF (DIST.LT.DMIN) THEN
          DMIN = DIST
          PSV  = P
          PTSV = ET
          SETA = ETA
          SPHI = PHI
        ENDIF
        IF (DMIN .LT. .02) THEN
          WRITE(LINE(1),2003)
 2003     FORMAT('   #    ID      P       PT      ETA      PHI ')
          WRITE(LINE(2),993)NPHO,'PHOT',PSV,PTSV,SETA,SPHI
  993     FORMAT(I4,3X,A4,3F8.2,1X,F8.2)
          DO J = 1,2
            CALL INTMSG(LINE(J))
          ENDDO
          DMIN = 1.E6
        ENDIF
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
        CALL PIDIST(PTMIN,LTRK,PXYZ,P,XVSV,XC,XPV,DIST)
        IF (DIST.LT.DMIN) THEN
          DMIN = DIST
          PSV  = P
          PTSV = ET
          SETA = ETA
          SPHI = PHI
        ENDIF
        IF (DMIN .LT. .02) THEN
          WRITE(LINE(1),2004)
 2004     FORMAT('   #    ID      P       PT      ETA      PHI ')
          WRITE(LINE(2),994)NTAU,'TAU',PSV,PTSV,SETA,SPHI
  994     FORMAT(I4,3X,A4,3F8.2,1X,F8.2)
          DO J = 1,2
            CALL INTMSG(LINE(J))
          ENDDO
          DMIN = 1.E6
        ENDIF
      ENDDO
C-
C-----Missing Et
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
        CALL PIDIST(PTMIN,LTRK,PXYZ,P,XVSV,XC,XPV,DIST)
        IF (DIST.LT.DMIN) THEN
          DMIN = DIST
          PSV  = P
          PTSV = ET
          SETA = ETA
          SPHI = PHI
        ENDIF
        IF (DMIN .LT. .02) THEN
          WRITE(LINE(1),2005)
 2005     FORMAT('   #    ID      P       PT      ETA      PHI ')
          WRITE(LINE(2),995)NMIS,'MET',PSV,PTSV,SETA,SPHI
  995     FORMAT(I4,3X,A4,3F8.2,1X,F8.2)
          DO J = 1,2
            CALL INTMSG(LINE(J))
          ENDDO
          DMIN = 1.E6
        ENDIF
      ENDDO
C-
  998 CONTINUE
C-
  999 RETURN
      END
