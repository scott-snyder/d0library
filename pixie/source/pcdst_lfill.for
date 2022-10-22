      SUBROUTINE PCDST_LFILL(ARRAY,IARRAY,XPMUO,NTYP,IOK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fill lego plot array from DST
C-
C-   Inputs  : none
C-   Outputs : ARRAY  - ET in ETA-PHI bins
C-             IARRAY - ID code for bins
C-                      code=1 for muon
C-                      code=2 for elec
C-                      code=3 for miss Et
C-                      code=4 for JETS
C-                      code=7 for JET_SIZE
C-                      code=20*1+ID for traks
C-             XPMUO  - Array for PMUO
C-             NTYP   - Number of objects
C-             IOK - 0 if banks are OK, 1 if do not exist
C-
C-   Created  10-SEP-1991   Sharon Hagopian
C-   Updated  13-JAN-1992   j.f. Det÷uf: jet size coded in mod(iarray,20)
C-                           ratio EM/ET coded in iarray/100
C-   Updated  24-FEB-1992   S. Hagopian, Interactive variable DRAWTK added
C-   Updated  17-MAR-1992   Nobu. Oshima - Modified for new JETS Bank.
C-   Updated  26-SEP-1992   Nobu. Oshima - Use GTJETS and GTJETS_NOEP.
C-   Updated   4-FEB-1994   Nobu. Oshima - Added new array XPMUO
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS/LIST'
      INCLUDE 'D0$INC:PI.DEF'
      INCLUDE 'D0$LINKS:IZZTRK.LINK/LIST'
      INTEGER IARRAY(NPHIL,2*NETAL),J
      INTEGER IT,IOK,IER,JERR
      INTEGER JP,JE,JP1,JE1,IRAT     !! 23-12-91
      INTEGER LJETS,GZJETS
      INTEGER LJTSH
      INTEGER LPMUO,GZPMUO
      INTEGER LPELC,GZPELC
      INTEGER LZTRH,LZTRK,GZZTRH       !!21-11-91
      INTEGER LZFIT                    !!21-11-91
      INTEGER IJETS,DRAWTK,KK,K
      INTEGER IVERS,NUMJT,JETEP
      INTEGER DRMUOT,NMUOT
      INTEGER NTRACK                        ! No. tracks
      INTEGER LMUOT,GZMUOT
      INTEGER QUAD,NSAMUS,IFW3,IFW4
      INTEGER IFW1,IFW2,NPTRK
      INTEGER NJET,NMUO,NELE,NTRA,IPASS,NXMUO
      REAL    EBUF(7),GTHETA,GPHI,GETA
      REAL ARRAY(NPHIL,2*NETAL),XPMUO(3,50)
      REAL ETMIN,THETA
      REAL ETACD,ETAI
      REAL ETA,PHI,ET
      REAL TE,EM,TET,EMT,TANTHE
      REAL XI,YI,ZI
      REAL XMAGC,YMAGC,ZMAGC,ELCAL,ELFE,SPARE1
      REAL XCOSOM,YCOSOM,ZCOSOM,SPARE2
      REAL XCOSIM,YCOSIM,ZCOSIM
      REAL CHSQBV,CHSNBV,MOM,MOMER
      INTEGER IP,IETA,NJGOOD
      INTEGER IDP,IDE,IM,NTYP(9)
      CHARACTER*4  PATH
C---------------------------------------------------------------------
      IOK=0
      CALL VZERO(ARRAY,NPHIL*2*NETAL)
      CALL VZERO(XPMUO,150)
      CALL VZERO_i(IARRAY,NPHIL*2*NETAL)
C zero variables
      NJET  = 0
      NTRA  = 0
      NMUO  = 0
      NXMUO = 0
      NELE  = 0
      NMUOT = 0
      DO 3 KK=1,6
    3 NTYP(KK)=0
C====== Process JETS ================================================
C-
      CALL PATHGT(PATH)
C      WRITE(16,*)' dans pcdst_lfill,path ',PATH
C-
      IF (PATH .EQ. 'RECO') THEN
        CALL PC_SET_CAPH('JETS',JERR)
        IF (JERR .NE. 0)      GO TO 300
      ENDIF
      CALL PUGET_i('PHYDIS DRAW JETS',IJETS)
C-
      LJETS = GZJETS()
      IF (LJETS .LE. 0) THEN
        CALL ERRMSG('PIXIE','PC3DST',
     &              'JETS bank does not exist','W')
        GO TO 300
      ENDIF
C--- Sort JETS banks from  Max-E to Min-E
      CALL ZSORT(IXCOM,LJETS,5)
      LJETS = GZJETS()
      CALL ZTOPSY(IXCOM,LJETS)
      LJETS = GZJETS()
C-
C--- Start JETS loop...
C-
      IER = 0
      NJGOOD=0
      CALL GTJETS_TOTAL(NUMJT,IER)
      IF (NUMJT.EQ.0 .OR. IER.NE.0)   GO TO 300
      JETEP = 0
      IF (IJETS .EQ. 0) THEN
        CALL GTJETS_NOEP
      ELSE
        CALL GTJETS_JNEP_RESET
      ENDIF
   10 NJET = NJET + 1
      IER = 0
      CALL GTJETS (NJET,IVERS,EBUF,GTHETA,GPHI,GETA,IER)
      IF (NJET.GT.NUMJT .OR. IER.EQ.-4) THEN
        NJET = NJET - JETEP - 1
        GO TO 300
      ELSEIF (IER .EQ. -5) THEN
        JETEP = JETEP + 1
        GO TO 10
      ENDIF
      ETA = GETA
      PHI = GPHI
      ET  = EBUF(5)
      IETA=(ETA+3.7)*10.+1.
      IP=(PHI/TWOPI)*64 +1
      IF(IP.GT.0.AND.IP.LE.NPHIL  .AND.
     +   IETA.GT.0.AND.IETA.LE.2*NETAL)GO TO 5
      WRITE(6,*)' ERROR - IP= ',IP,' IETA= ',IETA
      GO TO 10
    5 CONTINUE
      NJGOOD=NJGOOD+1
      ARRAY(IP,IETA)=ET
C-
C--- Handle only new JETS Bank
C-
      IRAT = 100*EBUF(7)
C ====  Analyse Jet size:
C      IDP = EBUF(6)*64./TWOPI
      IDE = EBUF(6)*100.
      IARRAY(IP,IETA)=4+100*IRAT+10000*IDE
C-
      GO TO 10
C-
C====== Process PMUO ================================================
C-
  300 NMUO = 0
C
      LPMUO = GZPMUO(0)
      IF (LPMUO.EQ.0) THEN
        CALL ERRMSG('PIXIE','PCDST_LFILL',
     &              'PMUO bank does not exist','W')
        GO TO 600
      ENDIF
C-
C--- Start PMUO loop...
C-
   20 IF (LPMUO .LE. 0)     GO TO 600
      NMUO = NMUO + 1
      ETA  = Q(LPMUO+16)
      PHI  = Q(LPMUO+17)
      ET   = Q(LPMUO+14)
C
      IP=(PHI/TWOPI)*64. +1.
      IETA=10.*(ETA+3.7)+1.
      IF(IP.GT.0.AND.IP.LE.NPHIL  .AND.
     &   IETA.GT.0.AND.IETA.LE.2*NETAL) THEN
        NXMUO = NXMUO + 1
        IF(NXMUO .LE. 50) THEN
          XPMUO(1,NXMUO) = IP
          XPMUO(2,NXMUO) = IETA
          XPMUO(3,NXMUO) = ET
        ENDIF
      ENDIF
C--- GO TO THE NEXT PMUO BANK
C-
      LPMUO = LQ(LPMUO)
      GO TO 20
C ========= PROCESS TRACKS  =====================ADDED 21-11-91 !!
  600 NTRA= 0
      CALL PUGETV('PHYDIS ETMIN',ETMIN)
      CALL PUGET_i('PHYDIS DRAW TRACKS',DRAWTK)
      IF(DRAWTK.EQ.0)GO TO 700
C
      LZTRH=GZZTRH()
      IF(LZTRH.LE.0)GO TO 700
      LZTRK=LQ(LZTRH-1)
      IF(LZTRK.EQ.0)GO TO 700  ! request a track
C                             !+----------------+
      DO WHILE( LZTRK.NE.0)   !| Loop on tracks |
C                             !+----------------+
        NTRA=NTRA+1
        IM=1
        LZFIT=LQ(LZTRK-1)
C  look if track is an electron candidate
        LPELC=GZPELC()
C                            !+----------------------------+
        DO WHILE (LPELC.NE.0)!| Loop on electron candidates|
C                            !+----------------------------+
          IF(LQ(LPELC-3).EQ. LZTRK)THEN
            IM=2
            GO TO 48
          END IF
          LPELC=LQ(LPELC)
        END DO
   48   CONTINUE
C  im=1 tout venant,im=2 electron
C
        THETA  = Q(LZFIT+13)
        PHI    = Q(LZFIT+10)
        TANTHE = TAN(THETA*.5)
        IF (TANTHE .LE. 0.) TANTHE = .001
        ETA    =-ALOG(TANTHE)
C
        IP=(PHI/TWOPI)*64. +1.
        IETA=10.*(ETA+3.7)+1.
C      WRITE(72,802)IP,IETA
        IF(IP.GT.0.AND.IP.LE.NPHIL  .AND.
     +    IETA.GT.0.AND.IETA.LE.2*NETAL)GO TO 78
C        WRITE(6,*)' ERROR - IP=',IP,' IETA=',IETA
        GO TO 700
C       !! Color for jet,ele,muo,nut coded in mod(iarray,20)
C       !! Color for traks coded in iarray/20
C       !! iarray/20=1     exspecting 'WHI'
C       !! Tracks are coded only in IARRAY not in ARRAY
   78   IARRAY(IP,IETA)= 20*1+IARRAY(IP,IETA)      !!
   84   LZTRK=LQ(LZTRK)
      END DO  ! End of loop on tracks
  700 CONTINUE
      NMUOT=0
      K=1
C Check if MUON tracks to be plotted
      CALL PUGET_i('PHYDIS DRAW MUOT',DRMUOT)
      IF(DRMUOT.EQ.0)GO TO 900
C    Get number of tracks from MTRH (muon track header bank) for MUOT
C    =====================================================================
      CALL GTMTRH(NTRACK)
      IF(NTRACK.LE.0)GO TO 900
C Check that MUOT bank exists
      LMUOT=GZMUOT(0)
      IF(LMUOT.LE.0)GO TO 900
  720 CALL GTMUOT(K,NPTRK,NSAMUS,QUAD,IFW1,IFW2,IFW3,IFW4,
     X      XI,YI,ZI,XMAGC,YMAGC,ZMAGC,XCOSIM,YCOSIM,ZCOSIM,XCOSOM,
     X      YCOSOM,ZCOSOM,CHSQBV,CHSNBV,MOM,MOMER,ELCAL,ELFE,SPARE1,
     X      SPARE2)
C CHECK TRACK QUALITY
      IF(IFW4.GT.2)GO TO 750
C    DRAW GOOD FIT TRACKS ONLY )
C    =======================================================
      IF (IFW2.GT.2)   GO TO 750
      IF (CHSQBV.GT.900.)              GO TO 750
C
      IF (MOM.EQ.0) GO TO 750
      NMUOT=NMUOT+1
      PHI=ATAN2(YCOSIM,XCOSIM)
      THETA=ACOS(ZCOSIM)
      TANTHE=TAN(THETA/2.)
      ETA=-ALOG(TANTHE)
      IP=(PHI/TWOPI)*64. +1.
      IETA=10.*(ETA+3.7)+1.
      IF(IP.GT.0.AND.IP.LE.NPHIL  .AND.
     +   IETA.GT.0.AND.IETA.LE.2*NETAL)GO TO 730
C        WRITE(6,*)' ERROR - IP=',IP,' IETA=',IETA
      GO TO 750
C       !! Color for jet,ele,muo,nut coded in mod(iarray,20)
C       !! Color for traks coded in iarray/20
C       !! iarray/20=1     exspecting 'WHI'
C       !! Tracks are coded only in IARRAY not in ARRAY
  730 IARRAY(IP,IETA)= 60*1+IARRAY(IP,IETA)      !!
  750 CONTINUE
      K=K+1
      IF(K.LE.NTRACK)GO TO 720
  900 CONTINUE
C
      NTYP(1)= NTRA
      NTYP(2)= NMUO
      NTYP(3)= 0
      NTYP(4)= 0
      NTYP(5)= NJGOOD
      NTYP(6)= NJGOOD
      NTYP(9)= NMUOT
C=                              =============last of process tracks
C-
C---  RESET CAPH
C-
  999 IF (JERR .EQ. 0) THEN
        CALL PC_RESET_CAPH
      ENDIF
      RETURN
      END
C ===================================================== end pcdst_lfill
