      SUBROUTINE PICK_DST
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Allows user to pick a 3DST TRACK and
C-                         displays P, Pt and ID of the track
C-
C-   Inputs  : None
C-   Outputs : Displays #,P,PT,ETA,PHI of the picked one
C-   Controls: None
C-
C-   MODIFIED TO DISPLAY dist.of closest approach,Truncated H mat.dim.,
C-   Chisq.for truncated H matrix,Probability for truncated H matrix for
C-   a picked ELECTRON and PHOTON( 10-23-92)
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZRECO.LINK'
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INTEGER LCAPH,GZCAPH,LRECO,LHMTE,LHMTP
      INTEGER LJETS,GZJETS,LPMUO,GZPMUO,LPELC,GZPELC
      INTEGER LVERT,GZVERT, LJTSH
      INTEGER LPPHO,GZPPHO,LZTRK,GZZTRK,LZFIT
      INTEGER NJET,NMUO,NELE,NPHO,NMUON,NTRK,NTAU,IPASS
      INTEGER IVERS, NUMJT,JETEP,NUM_TAU,IT
      INTEGER I,ID,IER,JERR, JLINE,IJETS
      INTEGER  IDBOX,TRHMTDM
      REAL    RCONE,LENGJ,PHIJ,ETAJ,EMFRAC
      REAL    EBUF(7),THETA,PHI,ETA
      REAL    ETAU(4),ETTAU,THETAT,ETAT,PHIT,RMS_WIDTH
      REAL    PTMIN,DPT,LTRK, SCALE,SETA,SPHI
      REAL    ETMIN,EXYZ(3),E,ET,XP(3),XV(3),XC(3)
      REAL    PXYZ(3),P,PT,PNOR,MISSET
c.n.  REAL    XMN, XMX, YMN, YMX, AORG(3)
      REAL     XPV(2),DIST,DMIN,PSV,PTSV,PJT,PTJT
      REAL    CHISTRHMT,PROBTRHMT,DCLAPP
      LOGICAL PU_SET_RCP_BANK
      LOGICAL EZERROR,PU_PICK_ACTIVE
      CHARACTER*4  PATH
      CHARACTER*70 LINE(5)
C----------------------------------------------------------------------
C
C ****  Get window coordinates of picked point
C
      CALL PU_GET_PICKV(XPV)
      DMIN=1.E6
C.N.
C-
      CALL PATHGT(PATH)
C--- Get parameters for track display
      CALL PUGETV('TRACK PTMIN',PTMIN)
      CALL PUGETV('TRACK LENGTH',LTRK)
      CALL PUGETV('PHYDIS DRAW JETS',IJETS)
C.N.  CALL PUGETV('PHYDIS DRAW TRACKS',IDRTK)
      ETMIN = PTMIN
      LTRK= 400.

C---- Get RECO from Header
C-
      LRECO = LQ(LHEAD-IZRECO)
      IF (LRECO .EQ. 0) GOTO 999
C----Find primary vertex
C-
      LVERT = GZVERT(1)
      IF (LVERT .GT. 0) THEN
        CALL UCOPY (Q(LVERT+3),XP(1),3)
      ELSE
        CALL VZERO(XP,3)
      ENDIF
C----------------------------------------
C----------------------------------------
C.N.
C====== Process JETS ================================================

C-
      NJET=0
C-
      IF (PATH .EQ. 'RECO') THEN
        CALL PC_SET_CAPH('JETS',JERR)
        IF (JERR .NE. 0)      GO TO 300  ! going to muon
      ENDIF
C-
      LJETS = GZJETS()
      IF (LJETS .LE. 0) GO TO 300
C-
C--- Sort JETS banks from  Max to Min by Energy
      CALL ZSORT(IXCOM,LJETS,5)
      LJETS = GZJETS()
      CALL ZTOPSY(IXCOM,LJETS)
      LJETS = GZJETS()
      PNOR  = Q(LJETS+5)
C-
C---- Start JETS loop...
C--
      CALL GTJETS_TOTAL(NUMJT,IER)
      IF (NUMJT.EQ.0 .OR. IER.NE.0)   GO TO 300
      JETEP = 0
      IF (IJETS .EQ. 0) THEN
        CALL GTJETS_NOEP
      ELSE
        CALL GTJETS_JNEP_RESET
      ENDIF
C-
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
      CALL VZERO(XC,3)
C--------------scaling--------
C-
      IF (ET .LT. ETMIN)    GO TO 15
      IF (E  .GT. PNOR) THEN
        SCALE  = PNOR/E
        E      = PNOR
        EXYZ(1)= SCALE*EXYZ(1)
        EXYZ(2)= SCALE*EXYZ(2)
        EXYZ(3)= SCALE*EXYZ(3)
      ENDIF
      CALL PIDIST(ETMIN,LTRK,EXYZ,E,XP,XC,XPV,DIST)
      IF (DIST.LT.DMIN) THEN
        DMIN = DIST
        PJT  = EBUF(4)
        PTJT = EBUF(5)
      ENDIF
C-
C-
      IF (DMIN .LT. .02) THEN
        WRITE(LINE(1),2000)
 2000   FORMAT('     #  ID    P       PT      ETA     PHI    EMFRAC')
        WRITE(LINE(2),989)NJET,'JET',PJT,PTJT,ETAJ,PHIJ,EMFRAC
  989   FORMAT(I6,A4,5F8.2)
C.V.    CALL PUTEXT_CREATE(IDBOX)
C.V.    CALL PUTEXT(IDBOX,LINE,2)
        DO I =1,2
          CALL INTMSG(LINE(I))
        ENDDO
        DMIN = 1.E6
      ENDIF
C---
C--- GO TO THE NEXT JETS BANK
C-
   15 LJETS = LQ(LJETS)
      GO TO 10

C----------------------------------------
C.N-- Process muon
C
  300 NMUO  = 0
C------------If no jets -------------
C-
      IF (PNOR .LE. 0.) THEN
        PNOR=40.
      ENDIF
C-
      LPMUO = GZPMUO(1)
   20 IF (LPMUO .EQ. 0) GOTO 600   !going to PELC...LATER!
      NMUO  = NMUO + 1
      CALL UCOPY(Q(LPMUO+10),PXYZ(1),3)
      P     = ABS(Q(LPMUO+13))
      PT    = Q(LPMUO+14)
      SETA  = Q(LPMUO+16)
      SPHI  = Q(LPMUO+17)
      CALL UCOPY(Q(LPMUO+25),XV(1),3)
c.n.
      CALL VZERO(XC,3)
      IF (PT .LT. PTMIN) GOTO 40
      IF ( P .GT. PNOR ) THEN
        SCALE  = PNOR/P
        P      = PNOR
        PXYZ(1)= SCALE*PXYZ(1)
        PXYZ(2)= SCALE*PXYZ(2)
        PXYZ(3)= SCALE*PXYZ(3)
      ENDIF
C-
      IF (IQ(LPMUO+5) .EQ. 1) THEN
        LVERT = LQ(LPMUO-4)
        IF (LVERT .GT. 0) THEN
          CALL UCOPY (Q(LVERT+3),XV(1),3)
        ELSE
          CALL VZERO(XV,3)
          LTRK  = 400.*P/PNOR
        ENDIF
        CALL PIDIST(PTMIN,LTRK,PXYZ, P, XV, XC, XPV, DIST)
      ELSEIF (IQ(LPMUO+5) .EQ. 2) THEN
        CALL UCOPY(Q(LPMUO+25),XV(1),3)
        CALL PIDIST(PTMIN,LTRK,PXYZ, P, XV, XC, XPV, DIST)
      ELSEIF (IQ(LPMUO+4) .GE. 1) THEN
        CALL PIDIST(PTMIN,LTRK,PXYZ, P, XP, XC, XPV, DIST)
      ENDIF
C-
      IF(DIST.LT.DMIN) THEN
        DMIN=DIST
        PSV = ABS(Q(LPMUO+13))
        PTSV= Q(LPMUO+14)
      ENDIF
C-
C-
      IF (DMIN .LT. .02) THEN
        WRITE(LINE(1),2001)
 2001   FORMAT('     #    ID     P       PT     ETA     PHI')
        WRITE(LINE(2),990)NMUO,'MUON',PSV,PTSV,SETA,SPHI
  990   FORMAT(I6,A6,4F8.2)
C.V.    CALL PUTEXT_CREATE(IDBOX)
C.V.    CALL PUTEXT(IDBOX,LINE,2)
        DO I =1,2
          CALL INTMSG(LINE(I))
        ENDDO
        DMIN = 1.E6
      ENDIF
   40 LPMUO = LQ(LPMUO)
      GOTO 20
C---------------------------------------
C---------------------------------------
C.N.- Process elec
C-
  600 NELE = 0
      DMIN = 1.E6
      LPELC = GZPELC()
   50 IF (LPELC .LE. 0) GOTO 700   !GOING TO TAU..
      NELE  = NELE + 1
      CALL UCOPY (Q(LPELC+3),EXYZ(1),3)
      E     = Q(LPELC+6)
      ET    = Q(LPELC+7)
      SETA  = Q(LPELC+9)
      SPHI  = Q(LPELC+10)
      DCLAPP= Q(LPELC+22)
C.N.--
C-    Since no end points for track
      CALL VZERO(XC,3)
      IF (ET .LT. ETMIN) GOTO 45
      IF ( E .GT. PNOR ) THEN
        SCALE  = PNOR/E
        E      = PNOR
        EXYZ(1)= SCALE*EXYZ(1)
        EXYZ(2)= SCALE*EXYZ(2)
        EXYZ(3)= SCALE*EXYZ(3)
      ENDIF
      CALL PIDIST(ETMIN,LTRK,EXYZ, E, XP, XC, XPV, DIST)
      IF(DIST.LT.DMIN) THEN
        LHMTE = LQ(LPELC-1)
        IF(LHMTE .GT.0)THEN
          TRHMTDM  = IQ(LHMTE+4)
          CHISTRHMT= Q(LHMTE+7)
          PROBTRHMT= Q(LHMTE+8)
        ENDIF
        DMIN=DIST
        PSV = Q(LPELC+6)
        PTSV= Q(LPELC+7)
      ENDIF
C-
      IF (DMIN .LT. .02) THEN
        WRITE(LINE(1),2002)
 2002   FORMAT('     #    ID     P       PT     ETA     PHI   DSCLAPP')
        WRITE(LINE(2),991)NELE,'ELEC',PSV,PTSV,SETA,SPHI,DCLAPP
  991   FORMAT(I6,A6,4F8.2,F10.4)
        WRITE(LINE(3),7000)
 7000   FORMAT('----HMTE----------------------------------------------')
        WRITE(LINE(4),5000)
 5000   FORMAT('  HMAT_DIM   CHISQ_HMAT  PROB_HMAT')
        WRITE(LINE(5),5001)TRHMTDM,CHISTRHMT,PROBTRHMT
 5001   FORMAT(I6,5X,2F10.4)
C.V.    CALL PUTEXT_CREATE(IDBOX)
C.V.    CALL PUTEXT(IDBOX,LINE,2)
        DO I =1,5
          CALL INTMSG(LINE(I))
        ENDDO
        DMIN = 1.E6
      ENDIF
   45 LPELC = LQ(LPELC)
      GOTO 50
C
C---------------------------------
C---------------------------------
C-
C====== Process PTAU ================================================
C-
  700 NTAU = 0
C-
      CALL GTPTAU_TOTAL(NUM_TAU,IER)
      IF (IER .EQ. 0) THEN
C-
C--- Start PTAU loop...
C-
        DO IT = 1,NUM_TAU
          CALL GTPTAU(IT,ETAU,ETTAU,THETAT,ETAT,PHIT,RMS_WIDTH,IER)
C-
          NTAU    = NTAU + 1
          CALL UCOPY (ETAU(1), EXYZ(1), 3)
          E       =  ETAU(4)
          ET      =  ETTAU
          SETA    =  ETAT
          SPHI    =  PHIT
C------
C-----Since no end points known----
C-
          CALL VZERO(XC,3)
C-
C-------Scaling---------
          IF (ET .LT. ETMIN)    GO TO 55
          IF ( E .GT. PNOR ) THEN
            SCALE  = PNOR/E
            E      = PNOR
            EXYZ(1)= SCALE*EXYZ(1)
            EXYZ(2)= SCALE*EXYZ(2)
            EXYZ(3)= SCALE*EXYZ(3)
          ENDIF
C-
          CALL PIDIST(ETMIN,LTRK,EXYZ, E, XP, XC, XPV, DIST)
          IF(DIST.LT.DMIN) THEN
            DMIN=DIST
            PSV = ETAU(4)
            PTSV= ETTAU
          ENDIF
C-
          IF (DMIN .LT. .02) THEN
            WRITE(LINE(1),2003)
 2003       FORMAT('     #    ID     P       PT     ETA     PHI')
            WRITE(LINE(2),992)NTAU,'TAU',PSV,PTSV,SETA,SPHI
  992       FORMAT(I6,A6,4F8.2)
C.V.      CALL PUTEXT_CREATE(IDBOX)
C.V.      CALL PUTEXT(IDBOX,LINE,2)
            DO I =1,2
              CALL INTMSG(LINE(I))
            ENDDO
            DMIN = 1.E6
          ENDIF
   55   ENDDO
      ENDIF
C---------------------------------
C----Process Photons
C----------------------------------
C-
      NPHO = 0
      LPPHO = GZPPHO()
C-
C--- Start PPHO loop...
C-
   52 IF (LPPHO .LE. 0)     GO TO 900
      NPHO    = NPHO + 1
      CALL UCOPY (Q(LPPHO+3), EXYZ(1), 3)
      E       =  Q(LPPHO+6)
      ET      =  Q(LPPHO+7)
      SETA    =  Q(LPPHO+9)
      SPHI    =  Q(LPPHO+10)
C-
C-    Since no end points for track
C-
      CALL VZERO(XC,3)
C----------------
C----Scaling------
      IF (ET .LT. ETMIN) GOTO 57
      IF ( E .GT. PNOR ) THEN
        SCALE  = PNOR/E
        E      = PNOR
        EXYZ(1)= SCALE*EXYZ(1)
        EXYZ(2)= SCALE*EXYZ(2)
        EXYZ(3)= SCALE*EXYZ(3)
      ENDIF
      CALL PIDIST(ETMIN,LTRK,EXYZ, E, XP, XC, XPV, DIST)
      IF(DIST.LT.DMIN) THEN
        DMIN=DIST
        LHMTP = LQ(LPPHO-1)
        IF(LHMTP .GT. 0) THEN
          TRHMTDM  = IQ(LHMTP+4)
          CHISTRHMT= Q(LHMTP+7)
          PROBTRHMT= Q(LHMTP+8)
        ENDIF
        PSV = Q(LPPHO+6)
        PTSV= Q(LPPHO+7)
      ENDIF
C-
      IF (DMIN .LT. .02) THEN
        WRITE(LINE(1),2004)
 2004   FORMAT('     #    ID     P       PT     ETA     PHI')
        WRITE(LINE(2),993)NPHO,'PHOT',PSV,PTSV,SETA,SPHI
  993   FORMAT(I6,A6,4F8.2)
        WRITE(LINE(3),7001)
 7001   FORMAT('----HMTP-----------------------------------------')
        WRITE(LINE(4),5002)
 5002   FORMAT('  HMAT_DIM   CHISQ_HMAT  PROB_HMAT')
        WRITE(LINE(5),5003)TRHMTDM,CHISTRHMT,PROBTRHMT
 5003   FORMAT(I6,5X,2F10.4)
C.V.    CALL PUTEXT_CREATE(IDBOX)
C.V.    CALL PUTEXT(IDBOX,LINE,2)
        DO I =1,5
          CALL INTMSG(LINE(I))
        ENDDO
        DMIN = 1.E6
      ENDIF
C----going to next photon bank
C-
   57 LPPHO = LQ(LPPHO)
      GOTO 52
  900 IF (JERR .EQ. 0) THEN
        CALL PC_RESET_CAPH
      ENDIF
C-
  999 RETURN
      END
