      SUBROUTINE PCEMHST
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Plot EM layer Histogram at IPHI +-1 and
C-                         IETA +-1.
C-
C-   Controls: IFLAG - 1 for Phi, 2 for Eta
C-
C-
C-   MOdified 28-MAR-1993   Nobuaki Oshima
C-   Created  15-JUL-1992   Nobuaki Oshima
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$PARAMS:BYTE_ORDER.PARAMS'
C-
      INTEGER LCAEP,LDCAEP
      INTEGER LPELC,LPPHO,LPELCN,LPPHON,LCACL,LCASH
      INTEGER GZCAEP,GZPELC,GZPPHO
      INTEGER NXBN,NYBN
      PARAMETER ( NXBN= 6 )
      PARAMETER ( NYBN= 12)
      INTEGER NX,NY,IXG,IYG
      INTEGER IETAMN,IETAMX,IPHIMN,IPHIMX
      INTEGER IETA,IPHI,IFLAG,INDX1,INDX2,INDL
      INTEGER NRP,NCH,IX,JBIT,I,J,II,JJ,IER
      INTEGER INDCES,IP,IE,IL
      INTEGER NXMIN,NYMIN,NXG,NYG,N
      INTEGER ELE_NUM,PHO_NUM,LENGTH,IELC
      INTEGER IETA_HOT(5),IPHI_HOT(5)
      REAL    EDPTH(5),PDPTH(5),ENERGY_HOT(5)
      REAL    PHIBUF(NXBN,NYBN),ETABUF(NXBN,NYBN)
      REAL    ENER,ZSCAL,ZMXPHI,ZMXETA
      REAL    XMIN,XMAX,YMIN,YMAX,ZMIN,ZMAX
      REAL    TXMIN,TXMAX,TYMIN,TYMAX,TZMIN,TZMAX,ZDEL,
     &        XDIR,YDIR,ZDIR,TEMP,UMIN,UMAX,VMIN,VMAX,
     &        WX,WY,WZ,PLZMAX,ZMED,ZDIV
      BYTE BYTES(4)
      EQUIVALENCE (BYTES(1),INDCES)
      CHARACTER*24 XLAB,YLAB,ZLAB
      CHARACTER*32 TITLE,STRING,OUTSTR1,OUTSTR2,PROM1,PROM2
      CHARACTER*3  COLOR
      CHARACTER    EP
      LOGICAL EZERROR
C-
      DATA PROM2/' Electron or Photon? [E] '/
C-
C----------------------------------------------------------------------
C-
C---
      CALL EZPICK('PX_CALDIS_RCP')
      IF ( EZERROR(IER) ) THEN
        CALL ERRMSG('PIXIE','PCEMHST',
     &    'Unable to pick RCP bank PX_CALDIS_RCP','W')
        GOTO 999
      ENDIF
C-
      CALL PUGETV('JET TYPE',IFLAG)
      CALL EZRSET
C_
C---- GET TOTAL NUMBER OF ELECTRONS AND PHOTONS
C-
      IF (IFLAG .EQ. 1) THEN
        ELE_NUM = 0
        PHO_NUM = 0
        LPELC   = GZPELC()
        LPPHO   = GZPPHO()
        IF (LPELC .GT. 0) CALL GTPELC_TOTAL(ELE_NUM,IER)
        IF(ELE_NUM .GT. 0 ) THEN
          WRITE(OUTSTR1,1100)ELE_NUM
          CALL PUTMSG(OUTSTR1)
        ENDIF
        IF (LPPHO .GT. 0) CALL GTPPHO_TOTAL(PHO_NUM,IER)
        IF(PHO_NUM .GT. 0) THEN
          WRITE(OUTSTR2,1200)PHO_NUM
          CALL PUTMSG(OUTSTR2)
        ENDIF
 1100   FORMAT(' Number of electrons = ',I3)
 1200   FORMAT(' Number of photons   = ',I3)
C-
        IF(ELE_NUM .GT. 0 .AND. PHO_NUM .GT. 0) THEN
          CALL GETPAR(1,PROM2,'U',EP)
          IF(EP(1:1) .EQ. 'P') THEN
            ELE_NUM = 0
          ELSE
            EP = 'E'
            PHO_NUM = 0
          ENDIF
        ENDIF
        IF(ELE_NUM .GT. 0) THEN
          EP = 'E'
          PROM1 = 'Enter The Electron Index:'
        ELSEIF (PHO_NUM .GT. 0)THEN
          EP = 'P'
          PROM1 = 'Enter The Photon Index:'
        ELSE
          GOTO 999
        ENDIF
C-
        CALL GETPAR(1,PROM1,'U',STRING)
        CALL SWORDS(STRING,II,JJ,LENGTH)
        IF(LENGTH .GT. 0) THEN
          READ(STRING(1:LENGTH),*,ERR=999) IELC
        ELSE
          IELC = 1
        ENDIF
C-
        IF(EP(1:1) .EQ. 'E' .AND. IELC .GT. ELE_NUM) THEN
          IELC = ELE_NUM
          WRITE(STRING,1300) IELC
        ELSEIF(EP(1:1) .EQ. 'P' .AND. IELC .GT. PHO_NUM) THEN
          IELC = PHO_NUM
          WRITE(STRING,1301) IELC
        ENDIF
        CALL PUTMSG(STRING)
 1300   FORMAT(' Electron',I3,' was selected.')
 1301   FORMAT(' Photon',I3,' was selected.')
C-
C--- Searching CACL and CASH...
C-
        IF (EP(1:1) .EQ. 'E') THEN
          IF (LPELC .LE. 0) THEN
            CALL ERRMSG('PIXIE','PCEMHST',
     &        'PELC Bank does not exist.','W')
            GOTO 999
          ENDIF
          WRITE(STRING,1300) IELC
          DO I = 1,IELC-1
            LPELCN = LPELC
            LPELC  = LQ(LPELCN)
            IF (LPELC .LE. 0) THEN
              LPELC = LPELCN
              GOTO 21
            ENDIF
          ENDDO
   21     CONTINUE
          LCACL = LQ(LPELC-2)
        ELSE
C-
          IF (LPPHO .LE. 0) THEN
            CALL ERRMSG('PIXIE','PCEMHST',
     &        'PPHO Bank does not exist.','W')
            GOTO 999
          ENDIF
          WRITE(STRING,1301) IELC
          DO I = 1,IELC-1
            LPPHON = LPPHO
            LPPHO  = LQ(LPPHON)
            IF (LPPHO .LE. 0) THEN
              LPPHO = LPPHON
              GOTO 41
            ENDIF
          ENDDO
   41     CONTINUE
          LCACL = LQ(LPPHO - 2)
        ENDIF
C-
        IF (LCACL .LE. 0) THEN
          CALL ERRMSG('PIXIE','PCEMHST',
     &      'CACL Bank does not exist.','W')
          GOTO 999
        ELSE
          LCASH = LQ(LCACL-2)
          IF (LCASH .LE. 0) THEN
            CALL ERRMSG('PIXIE','PCEMHST',
     &        'CASH Bank does not exist.','W')
            GOTO 999
          ENDIF
        ENDIF
C-
C--- Pick up Hotest IETA and IPHI at EM Layer 3
C-
        CALL CEMDPTH(LCASH,IETA_HOT,IPHI_HOT,EDPTH,PDPTH,ENERGY_HOT)
        IPHI = IPHI_HOT(3)
        IETA = IETA_HOT(3)
      ENDIF
C-
C=== Set up Lego Space
C-
      IPHIMN = IPHI - 1
      IF (IPHIMN .LE. 0)    IPHIMN = IPHIMN + NPHIL
      IPHIMX = IPHI + 1
      IF (IPHIMX .GE. 65)   IPHIMX = IPHIMX - NPHIL
C-
      IETAMN = IETA - 1
      IF (IETAMN .EQ. 0)    IETAMN = -1
      IETAMX = IETA + 1
      IF (IETAMX .EQ. 0)    IETAMX =  1
C-
C--- Pick CAEP Bank up...
C-
      IF(GZCAEP().LE.0) THEN
        CALL PUMESS(' PCEMHST - CAEP Bank DOES NOT exist!')
        GO TO 999
      ENDIF
      LCAEP = GZCAEP()
      NRP   = IQ(LCAEP+2)
      NCH   = IQ(LCAEP+3)
      IF(LCAEP.GT.0 .AND. NCH.GT.0) THEN
        IX=JBIT(IQ(LCAEP+4),6)
      ENDIF
C-
      CALL VZERO(PHIBUF,NXBN*NYBN)
      CALL VZERO(ETABUF,NXBN*NYBN)
      DO 100 I=1,NCH
        LDCAEP = LCAEP+(I-1)*NRP
        INDCES = IQ(LDCAEP+4)
        IE     = BYTES(BYTE4)
        IP     = BYTES(BYTE3)
        IL     = BYTES(BYTE2)
        ENER   = Q(LDCAEP+5)
        IF (IL.LT.MNLYEM .OR. IL.GT.MXLYEM)   GO TO 100
        IF (ENER .LE. 0.)                     GO TO 100
C--- PHI
C-
        INDX1 = 0
        INDX2 = 0
        IF (IP .EQ. IPHIMN) THEN
          INDX1 = 1
          INDX2 = 2
        ELSEIF (IP .EQ. IPHI) THEN
          INDX1 = 3
          INDX2 = 4
        ELSEIF (IP .EQ. IPHIMX) THEN
          INDX1 = 5
          INDX2 = 6
        ENDIF
        IF (IE.EQ.IETAMN .OR. IE.EQ.IETA .OR. IE.EQ.IETAMX) THEN
          IF (IL.LE.2 .OR. IL.GE.7) THEN
            INDL = 12 - (3*(IL-1))
            IF (IL .EQ. 7)   INDL = 3
            PHIBUF(INDX1,INDL) = ENER
            PHIBUF(INDX2,INDL) = ENER
          ELSE
            IF (IL.EQ.3 .OR. IL.EQ.5) THEN
              PHIBUF(INDX1,6) = PHIBUF(INDX1,6) + 2.*ENER
            ELSEIF (IL.EQ.4 .OR. IL.EQ.6) THEN
              PHIBUF(INDX2,6) = PHIBUF(INDX2,6) + 2.*ENER
            ENDIF
          ENDIF
        ENDIF
C--- ETA
C-
        INDX1 = 0
        INDX2 = 0
        IF (IE .EQ. IETAMN) THEN
          INDX1 = 1
          INDX2 = 2
        ELSEIF (IE .EQ. IETA) THEN
          INDX1 = 3
          INDX2 = 4
        ELSEIF (IE .EQ. IETAMX) THEN
          INDX1 = 5
          INDX2 = 6
        ENDIF
        IF (IP.EQ.IPHIMN .OR. IP.EQ.IPHI .OR. IE.EQ.IPHIMX) THEN
          IF (IL.LE.2 .OR. IL.GE.7) THEN
            INDL = 12 - (3*(IL-1))
            IF (IL .EQ. 7)   INDL = 3
            ETABUF(INDX1,INDL) = ENER
            ETABUF(INDX2,INDL) = ENER
          ELSE
            IF (IL.EQ.3 .OR. IL.EQ.4) THEN
              ETABUF(INDX1,6) = ETABUF(INDX1,6) + 2.*ENER
            ELSEIF (IL.EQ.5 .OR. IL.EQ.6) THEN
              ETABUF(INDX2,6) = ETABUF(INDX2,6) + 2.*ENER
            ENDIF
          ENDIF
        ENDIF
  100 CONTINUE
C-
C--- Set up Histo Frame and Label
C-
      NX    = NXBN
      NY    = NYBN
      YMIN  = 1.
      YMAX  = NYBN
      ZMIN  = 0.
      ZMAX  =-1.
      YLAB  = 'EM LAYER'
      ZLAB  = 'E'
      NXMIN = 1
      NYMIN = 1
      NXG   = 1
      NYG   = 1
      N     = NX
      ZSCAL = 1.
      IF (IFLAG .EQ. 1) THEN
        XMIN  = IPHIMN
        XMAX  = IPHIMX
        XLAB='IPHI'
        TITLE = '   IPHI - EM LAYER'
      ELSE
        XMIN  = IETAMN
        XMAX  = IETAMX
        XLAB='IETA'
        TITLE = '   IETA - EM LAYER'
      ENDIF
      ZMXPHI = PLZMAX(NXMIN,NX,NYMIN,NY,PHIBUF,PHIBUF,ZMAX,N,0)
      ZMXETA = PLZMAX(NXMIN,NX,NYMIN,NY,ETABUF,ETABUF,ZMAX,N,0)
      ZMAX = MAX(ZMXPHI,ZMXETA)
      IF (ZMAX .LE. 0.) THEN
        CALL PUMESS(' PCEMHST - No Energy inside bounds!')
        GO TO 999
      ENDIF
C-
C--- PLOT HISTO
C-
C-   Set viewing parameters
      CALL PLSETV(NX,NY,UMIN,UMAX,VMIN,VMAX)
C-   Making GRID
      CALL PLGRID(NXMIN,NX,NYMIN,NY,IXG,IYG,XMIN,XMAX,
     &            YMIN,YMAX)
C-   Draw Z-Axis
      ZMED=(XMAX+ZMIN)/2.
      CALL PLZAXS(NXMIN,NYMIN,ZMAX,ZMIN,ZMED,ZSCAL)
C-
C--- Building Blocks
      CALL PUOPEN
      DO J = 1,NY
        DO I = NX,1,-1
          TXMIN = FLOAT(I)
          TXMAX = TXMIN + 1.
          TYMIN = FLOAT(J)
          TYMAX = TYMIN + 1.
          TZMIN = 0.
          IF (IFLAG .EQ. 1) THEN
            ENER = PHIBUF(I,J)
          ELSE
            ENER = ETABUF(I,J)
          ENDIF
          TZMAX = ENER/ZMAX*ZSCAL
          IF ( J .EQ. 12 ) THEN         ! EM LAY-1
            COLOR = 'YEL'
          ELSEIF ( J .EQ. 9 ) THEN      ! EM LAY-2
            COLOR = 'GRE'
          ELSEIF ( J .EQ. 6 ) THEN      ! EM LAY-3
            COLOR = 'RED'
          ELSEIF ( J .EQ. 3 ) THEN      ! EM LAY-4
            COLOR = 'BLU'
          ELSE                          ! ???
            COLOR = ' '
          ENDIF
          IF (COLOR .NE. ' ') THEN
            CALL JPEDGE(0)
            CALL PXCOLFILL(COLOR)
            CALL PLDBAR(3,TXMIN,TXMAX,TYMIN,TYMAX,TZMIN,TZMAX,1)
          ENDIF
        ENDDO
      ENDDO
      CALL PUCLOSE
C-
C---
C   PRINTING AXIS
      CALL PLABEL(VMIN,XMIN,XMAX,YMIN,YMAX,ZMIN,ZMAX,XLAB,YLAB,ZLAB,
     &       TYMAX,TXMAX,TZMAX,NX,NXMIN,NY,NYMIN,ZSCAL)
C  PRINTING TITLE
C  NOTE: THIS ROUTINE SETS VIEWING PARAMETES TO X-Y VIEW
      CALL PXTITL(TITLE)
C-
  999 RETURN
      END
