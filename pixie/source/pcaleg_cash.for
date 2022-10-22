      SUBROUTINE PCALEG_CASH
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Make LEGO plot for D0 calorimeter using
C-                         CASH Bank which associate with PELC Bank.
C-                         EM energy in red, hadronic energy in blue.
C-
C-   Created  02-FEB-1993   Nobuaki Oshima
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$PARAMS:BYTE_ORDER.PARAMS'
C-
      INTEGER NX,NY,IMARK
      REAL XMIN,XMAX,YMIN,YMAX,ZMAX
      CHARACTER*3 COL1, COL2, COLORS(28)
      CHARACTER*8 LABELS(28)
      CHARACTER*24 XLAB,YLAB,ZLAB
      CHARACTER*20 PLTITL
      INTEGER NXMIN,NYMIN,N,NXG,NYG
      INTEGER GZPELC,GZPPHO,LPPHO,LPELC,LCACL,LCASH
      INTEGER LDCASH,INDCES,NRP,NCH, KCOL
      INTEGER IETA_HOT(5),IPHI_HOT(5)
      INTEGER IARRAY(NPHIL,2*NETAL)
      REAL EDPTH(5),PDPTH(5),ENERGY_HOT(5)
      REAL ZSCAL
      REAL ARRAY1(NPHIL,2*NETAL),ARRAY2(NPHIL,2*NETAL)
      REAL ENER, ESUM, EMAX,EADD
      REAL EMIN   ! MINIMUM ENERGY TO APPEAR IN LEGO PLOT
      REAL MXEMBN,MXHDBN,MXIMBN,MXTOTBN
      REAL MXEMHS,MXHDHS,MXIMHS,MXTOTHS
      INTEGER IL,IP,IE,I,J, IETA, IER
      INTEGER LAYMIN, LAYMAX, NCLUS, ICOL(14)
      CHARACTER*32 MESS1
      LOGICAL CALPLOT, ICDMG
      LOGICAL EZERROR, FIXEMAX
      BYTE BYTES(4)
      EQUIVALENCE (BYTES(1),INDCES)
C-
      DATA ICOL/13,16,15, 4, 6, 9, 7,14,10,12,11, 8, 5, 3/
      DATA IMARK/0/
C----------------------------------------------------------------------
C-
C--- Get minimum energy and min./max layer for cell to be plotted...
C--- Select correct RCP bank
C-
      CALL EZPICK('PX_PHYDIS_RCP')
      IF ( EZERROR(IER) ) THEN
        CALL ERRMSG('PIXIE','PCALEG_CASH',
     &    'Unable to pick RCP bank PX_PHYDIS_RCP','W')
        GOTO 999
      ENDIF
C-
      CALL PUGETV('PHYDIS EMIN',EMIN)
      CALL PUGET_i('PHYLEGO LAYMN',LAYMIN)
      CALL PUGET_i('PHYLEGO LAYMX',LAYMAX)
      CALL PUGET_l('PHYLEGO FEMAX',FIXEMAX)
C-
      CALL EZRSET
C-
      IF (LAYMIN .LT. MNLYEM) LAYMIN = MNLYEM
      IF (LAYMAX .GT. MXLYFH) LAYMAX = MXLYFH
      IF (LAYMIN .GT. LAYMAX) LAYMIN = LAYMAX
C-
      EMAX = 0.
      CALL VZERO(ARRAY1,NPHIL*2*NETAL)
      CALL VZERO(ARRAY2,NPHIL*2*NETAL)
C---
      LPELC = GZPELC()
      LPPHO = GZPPHO()
      IF (LPELC .LE. 0) THEN
        CALL ERRMSG('PIXIE','PCALEG_CASH',
     &    'PELC Bank does not exist.','W')
        IF (LPPHO .LE. 0) THEN
          CALL ERRMSG('PIXIE','PCALEG_CASH',
     &      'PPHO Bank does not exist.','W')
          GO TO 999
        ELSE
          LPELC = LPPHO
        ENDIF
      ENDIF
C-
C--- Loop for all PELC/PPHO Banks
C-
      NCLUS = 0
   10 LCACL = LQ(LPELC-2)
      IF (LCACL .LE. 0) THEN
        CALL ERRMSG('PIXIE','PCALEG_CASH',
     &    'CACL Bank does not exist.','W')
        GOTO 110
      ENDIF
C-
      LCASH = LQ(LCACL-2)
      IF (LCASH .LE. 0) THEN
        CALL ERRMSG('PIXIE','PCALEG_CASH',
     &    'CASH Bank does not exist.','W')
        GOTO 999
      ENDIF
C---
C-
      NCLUS = NCLUS + 1
      IF ( FIXEMAX )   THEN
        EADD = 0.
        CALL CEMDPTH(LCASH,IETA_HOT,IPHI_HOT,EDPTH,PDPTH,ENERGY_HOT)
        DO J = 1,5
          EADD = EADD + ENERGY_HOT(J)
        ENDDO
        IF (EMAX .LT. EADD ) THEN
          EMAX = EADD
        ENDIF
      ENDIF
C-
      NRP   = 2               ! repetition number
      NCH   = IQ(LCASH+2)     ! number of channels
C---
C-
      ESUM = 0.
      DO 100 I=1,NCH
        LDCASH = LCASH+(I-1)*NRP
        INDCES = IQ(LDCASH+3)
        ENER   =  Q(LDCASH+4)
        IE = BYTES(BYTE4)
        IP = BYTES(BYTE3)
        IL = BYTES(BYTE2)
        IF (IL.LT.LAYMIN .OR. IL.GT.LAYMAX) GO TO 100
        IF (ENER .LE. 0.) GO TO 100
        ESUM = ESUM + ENER
        IF (IE .LT. 0) THEN
          IETA = IE + NETAL + 1
        ELSE
          IETA = IE + NETAL
        ENDIF
C-
        IF(IL.GE.MNLYEM .AND. IL.LE.MXLYCH) THEN
          ARRAY1(IP,IETA) = ARRAY1(IP,IETA) + ENER
          IARRAY(IP,IETA) = NCLUS
        ENDIF
  100 CONTINUE
      KCOL  = MOD(NCLUS,14)
      CALL PXCOLITOC(ICOL(KCOL),COLORS(NCLUS))
      WRITE(LABELS(NCLUS),'(I2,1X,F5.1)')NCLUS,ESUM
C-
C--- Next PELC/PPHO...
  110 LPELC = LQ(LPELC)
      IF (LPELC .GT. 0) THEN
        GO TO 10
      ELSEIF(LPPHO.GT.0 .AND. LPPHO.NE.LPELC) THEN
        LPELC = LPPHO
        LPPHO = 0
        GO TO 10
      ENDIF
C---
C-
      NY=NETAL*2
      NX=NPHIL
      YMIN=-NETAL
      YMAX=NETAL
      XMIN=1.
      XMAX=NPHIL
      ZMAX=-1.
      XLAB='IPHI'
      YLAB='IETA'
      ZLAB='E'
      COL1= 'RED'   ! Red for EM
      COL2= 'CYA'   ! Cyan for HAD
      PLTITL='ENERGY CASH ETA-PHI'
      NXMIN=1
      NYMIN=1
      NXG=1
      NYG=1
      N=NX
      ZSCAL=.2
      CALPLOT = .TRUE.                 ! Cal Plot E
C-
      IF ( FIXEMAX )   THEN
        ZMAX = EMAX
      ENDIF
C-
      CALL P3LEGO(NX,XMIN,XMAX,NY,YMIN,YMAX,EMIN,ZMAX,PLTITL,
     &     XLAB,YLAB,ZLAB,ARRAY1,IARRAY,
     &     NXMIN,NYMIN,NXG,NYG,N,ZSCAL,IMARK)
C
C-
C--- Draw messages
      WRITE(MESS1,200) EMIN
      CALL PCTEXT(1,MESS1)
      CALL LEGEND_LEFT(COLORS,LABELS,NCLUS)
C-
  200 FORMAT(' PHYDIS EMIN =',F4.0,' GeV')
C---
C-
  999 RETURN
      END
