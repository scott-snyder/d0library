      SUBROUTINE PC3DST
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Draw Jets Axes, Muons, Electrons, Photons
C-                         and Miss-E's Axes in 3D View.
C-
C-   Inputs  :
C-   Outputs :
C-   Controls: Needs JETS, PMUO, PELC, PPHO and PNUT banks
C-
C-   Created  31-JAN-1991   Nobuaki Oshima
C-   Updated  19-MAR-1991   Harrison B. Prosper
C-      Call EZPICK('PX_CALDIS_RCP')
C-   Updated  18-JUN-1991   Nobu Oshima
C-      Adapts for System picking and rotating + adds 'CAL E COLORANGE'
C-   Updated  18-AUG-1991   Nobu Oshima
C-      Use PUTEXT to draw Particles Summary List.
C-   Updated  11-OCT-1991   Nobu Oshima
C-      Adapts for PHYDIS and change warning messages.
C-   Updated  22-FEB-1992   Nobu Oshima
C-      Impose energy or momentum scale which is shown by line length
C-      to the Max E of jet, if it's .GT. Max.E of the jet.
C-   Updated  14-MAR-1992   Nobu Oshima
C-      Draw Jets cone for EM(cyn) and HAD(red).
C-   Updated  21-MAY-1992   Nobu Oshima - Taking care photons now.
C-   Updated  21-JUN-1992   Nobu Oshima - Making particles/Jets list.
C-   Updated  16-SEP-1992   Sharon Hagopian - limit write buffer size
C-   Updated  26-SEP-1992   Nobu Oshima - Use GTJETS and GTJETS_NOEP
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INTEGER LCAPH,GZCAPH
      INTEGER LJETS,GZJETS,LPMUO,GZPMUO,LPELC,GZPELC
      INTEGER LPNUT,GZPNUT,LVERT,GZVERT, LJTSH
      INTEGER LPPHO,GZPPHO,LZTRK,GZZTRK,LZFIT
      INTEGER NJET,NMUO,NELE,NPHO,NMUON,NTRK,NTAU
      INTEGER IDRTK,IJETS,MXLINE,I,ID,IER,JERR, JLINE,IPASS
      INTEGER NVTX,NCDC,NFDC, DLEGEN
      INTEGER IVERS, NUMJT,JETEP,NUM_TAU,IT
      INTEGER MAXLINE,IPRERR
      PARAMETER (MAXLINE=256)
      REAL    EBUF(7),THETA,PHI,ETA
      REAL    ETAU(4),ETTAU,THETAT,ETAT,PHIT,RMS_WIDTH
      REAL    RCONE,LENGJ,PHIJ,ETAJ,EMFRAC
      REAL    PTMIN,DPT,LTRK, SCALE,SETA,SPHI
      REAL    ETMIN,EXYZ(3),E,ET,XP(3),XV(3),XC(3)
      REAL    PXYZ(3),P,PT,PNOR,MISSET
      REAL    XMN, XMX, YMN, YMX, AORG(3)
      REAL    RLEVEL
      LOGICAL PU_SET_RCP_BANK
      LOGICAL EZERROR,PU_PICK_ACTIVE
      CHARACTER*3  CNUM(5),NMSTR
      CHARACTER*4  PATH
      CHARACTER*5  ETSTR
      CHARACTER*22 STR1,STR2
      CHARACTER*34 SUMITEM(256)
C----------------------------------------------------------------------
C-
      CALL JIQDIL(RLEVEL)
C-
C--- Check do picking...
C-
      IF ( PU_PICK_ACTIVE() ) THEN
        CALL PICK_DST
        IF( RLEVEL .EQ. -2. )   CALL PX_PICK_QUIT
        GO TO 999
      ENDIF
C
C ****  Make sure we pick correct bank
C
      CALL EZPICK('PX_PHYDIS_RCP')          ! Selecting PHYDIS bank
      IF ( EZERROR(IER) ) THEN
        CALL ERRMSG('PIXIE','PC3DST','Bank PX_PHYDIS_RCP NOT FOUND','W')
        GOTO 999
      ENDIF
C
      CALL PATHGT(PATH)
C-
C--- Get parameters for JETS axes display from RCP bank(PT=ET)
      CALL PUGETV ('TRACK PTMIN',PTMIN)
      CALL PUGETV ('TRACK DPT',DPT)
      CALL PUGETV ('TRACK LENGTH',LTRK)
      CALL PUGETV ('PHYDIS DRAW TRACKS',IDRTK)
      CALL PUGETV ('PHYDIS DRAW JETS',IJETS)
      CALL PUGETV ('PHYDIS DRAW LEGEND',DLEGEN)
      ETMIN = PTMIN
C-
C--- Find Primary Vertex
C-
      LVERT = GZVERT(1)
      IF (LVERT .GT. 0) THEN
        CALL UCOPY (Q(LVERT+3), XP(1), 3)
      ELSE
        CALL VZERO(XP,3)
      ENDIF
C-
      JLINE = 1
      IPRERR=0
      CALL PUOPEN
C-
C====== Process JETS ================================================
C-
      NJET=0
C-
      IF (PATH .EQ. 'RECO') THEN
        CALL PC_SET_CAPH('JETS',JERR)
        IF (JERR .NE. 0)      GO TO 300
      ENDIF
C-
C--- Get CONE size
      LCAPH = GZCAPH()
      IF (IQ(LCAPH+4) .EQ. 2) THEN
        RCONE = Q(LCAPH+6)
      ELSE
        RCONE = 0.
      ENDIF
      LJETS = GZJETS()
      IF (LJETS .LE. 0) THEN
        CALL ERRMSG('PIXIE','PC3DST',
     &              'No JETS in this event!','W')
        GO TO 300
      ENDIF
C-
C--- Sort JETS banks from  Max to Min by Energy
      CALL ZSORT(IXCOM,LJETS,5)
      LJETS = GZJETS()
      CALL ZTOPSY(IXCOM,LJETS)
      LJETS = GZJETS()
      PNOR  = Q(LJETS+5)
C-
C--- Start JETS loop...
C-
      WRITE(SUMITEM(JLINE),2000)
 2000 FORMAT(' JETS#  PHI  ETA  ET(GeV)  E(GeV)')
      JLINE = JLINE + 1
C-
      IER = 0
      CALL GTJETS_TOTAL(NUMJT,IER)
      IF (NUMJT.EQ.0 .OR. IER.NE.0)   GO TO 300
      JETEP = 0
      IF (IJETS .EQ. 0) THEN
        CALL GTJETS_NOEP
      ELSE
        CALL GTJETS_JNEP_RESET
      ENDIF
   10 NJET = NJET + 1
      IER  = 0
      CALL GTJETS (NJET,IVERS,EBUF,THETA,PHI,ETA,IER)
      IF (NJET.GT.NUMJT .OR. IER.EQ.-4) THEN
        NJET = NJET - JETEP - 1
        GO TO 300
      ELSEIF (IER .EQ. -5) THEN
        JETEP = JETEP + 1
        GO TO 10
      ENDIF
      CALL UCOPY (EBUF(1), EXYZ(1), 3)
      E      = EBUF(4)
      ET     = EBUF(5)
      EMFRAC = EBUF(7)
      PHIJ   = PHI
      ETAJ   = ETA
C-
      IF(JLINE.LT.MAXLINE)THEN
        WRITE(SUMITEM(JLINE),2002) NJET,PHIJ,ETAJ,ET,E
 2002   FORMAT(3X,I2,F6.2,F5.1,2F8.2)
        JLINE = JLINE + 1
      ELSEIF(IPRERR.EQ.0)THEN
        IPRERR=1
        JLINE = JLINE + 1
        WRITE(SUMITEM(MAXLINE),2003)
 2003   FORMAT(' PARTICLE LIST OVERFLOW')
      ENDIF
C-
C--- Draw JETS axes...
      IF (ET .LT. ETMIN)    GO TO 10
      CALL PLTRKS(0, PNOR, EXYZ, E, XP)
C-
      CALL PXFTOC(ET, ETSTR)
      CALL JSIZE(10., 18.)
      CALL J3STRG(ETSTR)
C---
      LENGJ = 360. * E/PNOR
      CALL PCCONE(RCONE,LENGJ,ETAJ,PHIJ,EMFRAC,XP(3))
C--- GO TO THE NEXT JETS BANK
C-
      GO TO 10
C-
C====== Process PMUO ================================================
C-
  300 NMUO = 0
C- IF THERE WAS NO JETS...
      IF (PNOR .LE. 0.) THEN
        PNOR = 40.
      ENDIF
C
      LPMUO = GZPMUO(0)
      IF (LPMUO.EQ.0) THEN
        CALL ERRMSG('PIXIE','PC3DST',
     &              'No MUON in this event!','W')
        GO TO 600
      ENDIF
C-
C--- Start PMUO loop...
C-
      IF(JLINE.LT.MAXLINE)THEN
        WRITE(SUMITEM(JLINE),2004)
 2004   FORMAT(' MUON#  PHI  ETA      PT:P(GeV/C)')
        JLINE = JLINE + 1
      ELSEIF(IPRERR.EQ.0)THEN
        IPRERR=1
        JLINE = JLINE + 1
        WRITE(SUMITEM(MAXLINE),2003)
      ENDIF
C-
   20 IF (LPMUO .LE. 0)     GO TO 600
      NMUO    = NMUO + 1
      CALL UCOPY (Q(LPMUO+10), PXYZ(1), 3)
      P       = ABS(Q(LPMUO+13))
      PT      = Q(LPMUO+14)
      SETA    = Q(LPMUO+16)
      SPHI    = Q(LPMUO+17)
      CALL UCOPY (Q(LPMUO+25), XV(1), 3)
C-
      IF(JLINE.LT.MAXLINE)THEN
        WRITE(SUMITEM(JLINE),2006) NMUO,SPHI,SETA,PT,P
 2006   FORMAT(3X,I2,F6.2,F5.1,1X,F7.1,':',F7.1)
        JLINE = JLINE + 1
      ELSEIF(IPRERR.EQ.0)THEN
        IPRERR=1
        JLINE = JLINE + 1
        WRITE(SUMITEM(MAXLINE),2003)
      ENDIF
C-
C--- Draw MUON tracks...
C-
      IF (PT .LT. PTMIN)     GO TO 40
      IF ( P .GT. PNOR ) THEN
        SCALE  = PNOR/P
        P      = PNOR
        PXYZ(1)= SCALE*PXYZ(1)
        PXYZ(2)= SCALE*PXYZ(2)
        PXYZ(3)= SCALE*PXYZ(3)
      ENDIF
      CALL PXLWID(5)
      IF (IQ(LPMUO+5) .EQ. 1) THEN ! tk vec. is defined at the vertex
        LVERT = LQ(LPMUO-4)
        IF (LVERT .GT. 0) THEN
          CALL UCOPY (Q(LVERT+3), XV(1), 3)
        ELSE                        ! protecttion for junk muon data
          CALL VZERO(XV,3)
        ENDIF
        CALL PLTRKS(14, PNOR, PXYZ, P, XV)
      ELSEIF (IQ(LPMUO+5) .EQ. 2) THEN ! tk vec. is defined at muon sys.
        CALL UCOPY (Q(LPMUO+25), XV(1), 3)
        CALL PLTRKS(14, PNOR, PXYZ, P, XV)
C-
C--- at least vertex point is used for fitting...
      ELSEIF (IQ(LPMUO+4) .GE. 1) THEN
        CALL PLTRKS(14, PNOR, PXYZ, P, XP)
C.      ELSE                         ! junk muons
C.        CALL PLTRKS(14, PNOR, PXYZ, P, XV)
      ENDIF
C-
      CALL PXFTOC(PT, ETSTR)
      CALL JSIZE(10., 18.)
      CALL J3STRG(ETSTR)
C--- GO TO THE NEXT PMUO BANK
C-
   40 LPMUO = LQ(LPMUO)
      GO TO 20
C-
C====== Process PELC ================================================
C-
  600 NELE = 0
C-
      LPELC = GZPELC()
      IF (LPELC .LE. 0) THEN
        CALL ERRMSG('PIXIE','PC3DST',
     &              'No ELECTRON in this event!','W')
        GO TO 700
      ENDIF
C-
C--- Start PELC loop...
C-
      IF(JLINE.LT.MAXLINE)THEN
        WRITE(SUMITEM(JLINE),2008)
 2008   FORMAT(' ELEC#  PHI  ETA  ET(GeV)  E(GeV)')
        JLINE = JLINE + 1
      ELSEIF(IPRERR.EQ.0)THEN
        IPRERR=1
        JLINE = JLINE + 1
        WRITE(SUMITEM(MAXLINE),2003)
      ENDIF
C-
   50 IF (LPELC .LE. 0)     GO TO 700
      NELE    = NELE + 1
      CALL UCOPY (Q(LPELC+3), EXYZ(1), 3)
      E       =  Q(LPELC+6)
      ET      =  Q(LPELC+7)
      SETA    =  Q(LPELC+9)
      SPHI    =  Q(LPELC+10)
C-
      IF(JLINE.LT.MAXLINE)THEN
        WRITE(SUMITEM(JLINE),2002) NELE,SPHI,SETA,ET,E
        JLINE = JLINE + 1
      ELSEIF(IPRERR.EQ.0)THEN
        IPRERR=1
        JLINE = JLINE + 1
        WRITE(SUMITEM(MAXLINE),2003)
      ENDIF
C-
C--- Draw Electron vectors...
      IF (ET .LT. ETMIN)    GO TO 55
      IF ( E .GT. PNOR ) THEN
        SCALE  = PNOR/E
        E      = PNOR
        EXYZ(1)= SCALE*EXYZ(1)
        EXYZ(2)= SCALE*EXYZ(2)
        EXYZ(3)= SCALE*EXYZ(3)
      ENDIF
      CALL PXLWID(5)
      CALL PLTRKS(12, PNOR, EXYZ, E, XP)
      CALL PXFTOC(ET, ETSTR)
C
      CALL JSIZE(10., 18.)
      CALL J3STRG(ETSTR)
C--- GO TO THE NEXT PELC BANK
C-
   55 LPELC = LQ(LPELC)
      GO TO 50
C-
C====== Process PTAU ================================================
C-
  700 NTAU = 0
C-
      IER = 0
      CALL GTPTAU_TOTAL(NUM_TAU,IER)
      IF (IER .EQ. 0) THEN
C-
C--- Start PTAU loop...
C-
        DO IT = 1,NUM_TAU
          CALL GTPTAU(IT,ETAU,ETTAU,THETAT,ETAT,PHIT,RMS_WIDTH,IER)
          IF(JLINE.LT.MAXLINE)THEN
            WRITE(SUMITEM(JLINE),2009)
 2009       FORMAT(' TAU #  PHI  ETA  ET(GeV)  E(GeV)')
            JLINE = JLINE + 1
          ELSEIF(IPRERR.EQ.0)THEN
            IPRERR=1
            JLINE = JLINE + 1
            WRITE(SUMITEM(MAXLINE),2003)
          ENDIF
C-
          NTAU    = NTAU + 1
          CALL UCOPY (ETAU(1), EXYZ(1), 3)
          E       =  ETAU(4)
          ET      =  ETTAU
          SETA    =  ETAT
          SPHI    =  PHIT
C-
          IF(JLINE.LT.MAXLINE)THEN
            WRITE(SUMITEM(JLINE),2002)NTAU,SPHI,SETA,ET,E
            JLINE = JLINE + 1
          ELSEIF(IPRERR.EQ.0)THEN
            IPRERR=1
            JLINE = JLINE + 1
            WRITE(SUMITEM(MAXLINE),2003)
          ENDIF
C-
C--- Draw Tau vectors...
          IF (ET .GE. ETMIN) THEN
            IF ( E .GT. PNOR ) THEN
              SCALE  = PNOR/E
              E      = PNOR
              EXYZ(1)= SCALE*EXYZ(1)
              EXYZ(2)= SCALE*EXYZ(2)
              EXYZ(3)= SCALE*EXYZ(3)
            ENDIF
            CALL PXLWID(5)
            CALL PLTRKS(16, PNOR, EXYZ, E, XP)
            CALL PXFTOC(ET, ETSTR)
C
            CALL JSIZE(10., 18.)
            CALL J3STRG(ETSTR)
          ENDIF
        ENDDO
      ELSE
        CALL ERRMSG('PIXIE','PC3DST',
     &              'No TAU in this event!','W')
      ENDIF
C-
C====== Process PPHO ================================================
C-
      NPHO = 0
C-
      LPPHO = GZPPHO()
      IF (LPPHO .LE. 0) THEN
        CALL ERRMSG('PIXIE','PC3DST',
     &              'No PHOTON in this event!','W')
        GO TO 800
      ENDIF
C-
C--- Start PPHO loop...
C-
      IF(JLINE.LT.MAXLINE)THEN
        WRITE(SUMITEM(JLINE),2010)
 2010   FORMAT(' PHOT#  PHI  ETA  ET(GeV)  E(GeV)')
        JLINE = JLINE + 1
      ELSEIF(IPRERR.EQ.0)THEN
        IPRERR=1
        JLINE = JLINE + 1
        WRITE(SUMITEM(MAXLINE),2003)
      ENDIF
C-
   52 IF (LPPHO .LE. 0)     GO TO 800
      NPHO    = NPHO + 1
      CALL UCOPY (Q(LPPHO+3), EXYZ(1), 3)
      E       =  Q(LPPHO+6)
      ET      =  Q(LPPHO+7)
      SETA    =  Q(LPPHO+9)
      SPHI    =  Q(LPPHO+10)
C-
      IF(JLINE.LT.MAXLINE)THEN
        WRITE(SUMITEM(JLINE),2002) NPHO,SPHI,SETA,ET,E
        JLINE = JLINE + 1
      ELSEIF(IPRERR.EQ.0)THEN
        IPRERR=1
        JLINE = JLINE + 1
        WRITE(SUMITEM(MAXLINE),2003)
      ENDIF
C-
C--- Draw Photon vectors...
      IF (ET .LT. ETMIN)    GO TO 57
      IF ( E .GT. PNOR ) THEN
        SCALE  = PNOR/E
        E      = PNOR
        EXYZ(1)= SCALE*EXYZ(1)
        EXYZ(2)= SCALE*EXYZ(2)
        EXYZ(3)= SCALE*EXYZ(3)
      ENDIF
      CALL PXLWID(5)
      CALL PLTRKS(10, PNOR, EXYZ, E, XP)
      CALL PXFTOC(ET, ETSTR)
C
      CALL JSIZE(10., 18.)
      CALL J3STRG(ETSTR)
C--- GO TO THE NEXT PELC BANK
C-
   57 LPPHO = LQ(LPPHO)
      GO TO 52
C-
C====== Process PNUT ================================================
C-
  800 MISSET = 0.
C-
      IF(JLINE.LT.MAXLINE)THEN
        WRITE(SUMITEM(JLINE),2020)
 2020   FORMAT(' PNUT#  PHI  ETA  ET(GeV)  E(GeV)')
        JLINE = JLINE + 1
      ELSEIF(IPRERR.EQ.0)THEN
        IPRERR=1
        JLINE = JLINE + 1
        WRITE(SUMITEM(MAXLINE),2003)
      ENDIF
C-
      DO 60 IPASS = 1,5
        LPNUT = GZPNUT(IPASS)
        IF (LPNUT .GT. 0)  THEN
          EXYZ(1) =  Q(LPNUT+3)
          EXYZ(2) =  Q(LPNUT+4)
          EXYZ(3) =  XP(3)     ! IMPOSE ETA
          E       =  Q(LPNUT+6)
          ET      =  Q(LPNUT+7)
          SETA    =  Q(LPNUT+9)
          SPHI    =  Q(LPNUT+10)
          MISSET  =  ET
          IF(JLINE.LT.MAXLINE)THEN
            WRITE(SUMITEM(JLINE),2002) IPASS,SPHI,SETA,ET,E
            JLINE = JLINE + 1
          ELSEIF(IPRERR.EQ.0)THEN
            IPRERR=1
            JLINE = JLINE + 1
            WRITE(SUMITEM(MAXLINE),2003)
          ENDIF
        ELSE
          GO TO 70
        ENDIF
   60 CONTINUE
C-
C--- Draw Miss. E vectors...
   70 IF ( E .GT. PNOR ) THEN
        SCALE  = PNOR/E
        E      = PNOR
        EXYZ(1)= SCALE*EXYZ(1)
        EXYZ(2)= SCALE*EXYZ(2)
        EXYZ(3)= SCALE*EXYZ(3)
      ENDIF
      CALL PXLWID(8)
      CALL PLTRKS(-1, PNOR, EXYZ, E, XP)
C-
C-
C====== Process ZTRK ================================================
C-
  900 NTRK = 0
      IF (IDRTK .EQ. 0) GO TO 1000
C-
      LZTRK = GZZTRK(0)
      IF (LZTRK .LE. 0) THEN
        CALL ERRMSG('PIXIE','PC3DST',
     &              'No ZTRKS in this event!','W')
        GO TO 1000
      ENDIF
C-
C--- Start ZTRK loop...
C-
C.      IF(JLINE.LT.MAXLINE)THEN
C.        WRITE(SUMITEM(JLINE),2012)
C. 2012   FORMAT(' ZTRK#   VTXH#   CDCH#   FDCH#')
C.        JLINE=JLINE+1
C.      ELSEIF(IPRERR.EQ.0)THEN
C.        IPRERR=1
C.        JLINE = JLINE + 1
C.        WRITE(SUMITEM(MAXLINE),2003)
C.      ENDIF
C-
   80 IF (LZTRK .LE. 0)     GO TO 1000
      NTRK    = NTRK + 1
      LZFIT   = LQ(LZTRK-1)
      IF (LZFIT .LE. 0)     GO TO 85
      EXYZ(1) = PNOR*SIN(Q(LZFIT+13))*COS(Q(LZFIT+10))
      EXYZ(2) = PNOR*SIN(Q(LZFIT+13))*SIN(Q(LZFIT+10))
      EXYZ(3) = PNOR*COS(Q(LZFIT+13))
      E       = PNOR
C-
      NVTX    = IQ(LZFIT+3)
      NCDC    = IQ(LZFIT+4)
      NFDC    = IQ(LZFIT+5)
C.      IF(JLINE.LT.MAXLINE)THEN
C.        WRITE(SUMITEM(JLINE),2014) NTRK,NVTX,NCDC,NFDC
C. 2014   FORMAT(3X,I2,1X,3I8)
C.        JLINE = JLINE + 1
C.      ELSEIF(IPRERR.EQ.0)THEN
C.        IPRERR=1
C.        JLINE = JLINE + 1
C.        WRITE(SUMITEM(MAXLINE),2003)
C.      ENDIF
C-
C--- Draw ZTRAKS vectors...
      CALL PXLWID(2)
      CALL PLTRKS(99, PNOR, EXYZ, E, XP)
      CALL PXITOC(NTRK, 2, NMSTR)
C
      CALL JSIZE(10., 18.)
      CALL J3STRG(NMSTR)
C--- GO TO THE NEXT PELC BANK
C-
   85 LZTRK = LQ(LZTRK)
      GO TO 80
C-
C--- Draw Labels and Beam pipe.
 1000 CONTINUE
      IF(DLEGEN .EQ. 1) THEN
        CALL PLCA3D
      ENDIF
C-
      CALL JRCLOS
C-
C-
C--- Draw Particle Summary List and a track label
C-
      MXLINE = JLINE - 1
      DO I=1,MXLINE
        CALL INTMSG( SUMITEM(I) )
      ENDDO
C-
      WRITE(STR1,201) PTMIN
  201 FORMAT('ET(PT)-MIN:',F5.1,' GeV')
      IPASS = IPASS - 1
      WRITE(STR2,202) IPASS,MISSET
  202 FORMAT('Miss. ET(',I1,'):',F5.1,' GeV')
      IF(DLEGEN .EQ. 1) THEN
        CALL PCTEXT(1,STR1)
        CALL PCTEXT(2,STR2)
        CALL LEGENDTK
      ELSE
        CALL PCTEXT(3,STR1)
        CALL PCTEXT(4,STR2)
      ENDIF
C-
C---  RESET CAPH
C-
      IF (JERR .EQ. 0) THEN
        CALL PC_RESET_CAPH
      ENDIF
C-
C--- Reset RCP-bank
C-
      CALL EZRSET
C
  999 RETURN
      END
