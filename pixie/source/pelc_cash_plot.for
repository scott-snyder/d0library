      SUBROUTINE PELC_CASH_PLOT
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Make CASH plot and histogram layer by layer 
C-                         for D0 calorimeter.
C-
C-   Modified 23-MAR-1993   Sailesh Chopra
C-   Created  02-FEB-1993   Nobuaki Oshima
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$PARAMS:BYTE_ORDER.PARAMS'
C      INCLUDE 'D0$INC:PXPARA.INC'
C-FOR LABELS AND LEGENDS
      integer xjust,yjust
      INTEGER KCOL,KINT,KFIL,KSTY
      INTEGER COLORS(5),NUM,IC
      REAL COLRNGE,rj
      CHARACTER*15 LABELS(5)
      CHARACTER*3 CCOL,ccolors(5)
      DATA NUM/5/
C******
      INTEGER NX,NY,IMARK
      REAL ZMAX
      CHARACTER*3 COL1, COL2
      CHARACTER*24 XLAB,YLAB,ZLAB
      CHARACTER*32 PLTITL
      INTEGER NXMIN,NYMIN,N
      INTEGER NXG,NYG,isiz
      INTEGER GZPELC,LPELC,LCACL,LCASH
      INTEGER LDCASH,INDCES,NRP,NCH,LAYMIN1,LAYMIN3
      INTEGER IETA_HOT(8),IPHI_HOT(8)
      INTEGER META(5),MPHI(5),META3(10),MPHI3(10),MIN_ETA,
     &  MAX_ETA,MIN_PHI,MAX_PHI
      REAL MENER(5),MENER3(5)
      REAL EDPTH(8),PDPTH(8),ENERGY_HOT(8)
      REAL ZSCAL,XFAC,YFAC
      REAL ENER, ESUM(5), EMAX,EADD
      REAL EMIN   ! MINIMUM ENERGY TO APPEAR IN LEGO PLOT
      REAL MXEMBN,MXHDBN,MXIMBN,MXTOTBN
      REAL MXEMHS,MXHDHS,MXIMHS,MXTOTHS,RLEVEL
      REAL XMIN1,XMAX1,YMIN1,YMAX1
      REAL XBOX(4),YBOX(4)
      REAL EEM(5),YVAL(12),XVAL(12),JUMP
      REAL VXMIN,VXMAX,VYMIN,VYMAX,XMIN,XMAX,
     &  YMIN,YMAX
      INTEGER IL,IP,IE,I,J, IETA, IER,ELE_NUM,PHO_NUM,II,JJ,LENGTH
      REAL RIP,RIE
      INTEGER LAYMIN, LAYMAX, IX, JBIT,IELC,IPHO,IN_COUNT
      INTEGER GZPPHO,LPPHO,LPPHO1,LPELC1
      INTEGER XW(4),YW(4),ZW(4)
      CHARACTER*32 MESS1,MESS2
      CHARACTER*52 EXMESS
      CHARACTER*80 STRING,PROM1,PROM2,OUTSTR1,OUTSTR2
      CHARACTER EP
      LOGICAL CALPLOT, ICDMG
      LOGICAL EZERROR, FIXEMAX,first
      BYTE BYTES(4)
      EQUIVALENCE (BYTES(1),INDCES)
      REAL DENOM,COUNT
      INTEGER MULT_ETA,MULT_PHI
      INTEGER LENOCC
      EXTERNAL LENOCC
      REAL ENER_ARR(5,5),ENER_ARR3(10,10),STEP,ENER_EM3
C-
      SAVE IN_COUNT
      DATA IN_COUNT/0/
      data first/.true./
      DATA col1/'RED'/
      DATA PROM1/'Enter the Electron Index'/
      DATA PROM2/'Electron/Photon[E]'/
      DATA IMARK/0/
      DATA COLRNGE/1.0/
C----------------------------------------------------------------------
C-
C--- Get minimum energy and min./max layer for cell to be plotted...
C--- Select correct RCP bank
C-
      CALL JIQDIL(RLEVEL)
      IN_COUNT = IN_COUNT + 1
      IF (IN_COUNT .GT. 1) GOTO 111
      CALL VZERO(ESUM,5)
C_
C---- GET TOTAL NUMBER OF ELECTRONS AND PHOTONS
      ELE_NUM = 0
      PHO_NUM = 0
      LPELC = GZPELC()
      IF (LPELC .NE. 0) CALL GTPELC_TOTAL(ELE_NUM,IER)
      IF (GZPPHO() .NE. 0) CALL GTPPHO_TOTAL(PHO_NUM,IER)
      IF (ELE_NUM .EQ. 0 .AND. PHO_NUM .EQ. 0) THEN
        IN_COUNT = 0
        CALL PUMESS('NO ELECTRON OR PHOTON FOUND')
        GOTO 999
      ENDIF
C--- CHOOSE ELECTRON index
C-
      IELC=1
      IF(ELE_NUM .GT. 0 ) THEN
        WRITE(OUTSTR1,11)ELE_NUM
        CALL PUTMSG(OUTSTR1)
      ENDIF
      IF(PHO_NUM .GT. 0) THEN
        WRITE(OUTSTR2,12)PHO_NUM
        CALL PUTMSG(OUTSTR2)
      ENDIF
   11 FORMAT('NUMBER OF ELECTRONS IN EVENT = ',I3)
   12 FORMAT('NUMBER OF PHOTONS IN EVENT   = ',I3)
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
        PROM1 = 'Enter The Electron Index'
      ELSEIF (PHO_NUM .GT. 0)THEN
        EP = 'P'
        PROM1 = 'Enter The Photon Index'
      ELSE
        GOTO 999
      ENDIF
C-
      CALL GETPAR(1,PROM1,'U',STRING)
      CALL SWORDS(STRING,II,JJ,LENGTH)
      IF(LENGTH.NE.0)THEN
        READ(STRING(1:LENGTH),*,ERR=999) IELC
      ENDIF
      IF(EP(1:1) .EQ. 'E' .AND. IELC .GT. ELE_NUM) THEN
        IELC = ELE_NUM
      ELSEIF(EP(1:1) .EQ. 'P' .AND. IELC .GT. PHO_NUM) THEN
        IELC = PHO_NUM
      ENDIF
      WRITE(STRING,201)IELC
      CALL PUTMSG(STRING)
  201 FORMAT('ELECTRON/PHOTON SELECTED = ',I3)
C-
C--- After second action...
C-
  111 CONTINUE
      DO I = 1,10
        DO J = 1,10
          IF (I .LE. 5 .AND. J .LE. 5) THEN
            ENER_ARR(I,J) = 0
          ENDIF
          ENER_ARR3(I,J) = 0
        ENDDO
      ENDDO

      CALL EZPICK('PX_PHYDIS_RCP')
      IF ( EZERROR(IER) ) THEN
        CALL ERRMSG('PIXIE','PELC_CASH_PLOT',
     &    'Unable to pick RCP bank PX_PHYDIS_RCP','W')
        GOTO 999
      ENDIF
C-
      CALL PUGETV('PHYDIS EMIN',EMIN)
      CALL PUGETV('PHYLEGO LAYMN',LAYMIN)
      CALL PUGETV('PHYLEGO LAYMX',LAYMAX)
      CALL PUGETV('PHYLEGO FEMAX',FIXEMAX)
C-
      CALL EZRSET
C-
      IF (LAYMIN .LT. MNLYEM) LAYMIN = MNLYEM
      IF (LAYMAX .GT. MXLYFH) LAYMAX = MXLYFH
      IF (LAYMIN .GT. LAYMAX) LAYMIN = LAYMAX
C-
      LAYMIN3 = LAYMIN
      EMAX = 0.
C-
      LPELC = GZPELC()
      LPPHO = GZPPHO()
      IF (EP .EQ. 'E') THEN
        IF (LPELC .LE. 0) THEN
          CALL ERRMSG('PIXIE','PELC_CASH_PLOT',
     &      'PELC Bank does not exist.','W')
          GOTO 999
        ENDIF
        DO I = 1,IELC-1
          LPELC1 = LPELC
          LPELC=LQ(LPELC)
          IF (LPELC .LE. 0) THEN
            LPELC = LPELC1
            GOTO 21
          ENDIF
        ENDDO
   21   CONTINUE
        WRITE(STRING,202)I-1
        LCACL = LQ(LPELC-2)
      ELSE
        IF(LPPHO .LE. 0) THEN
          CALL ERRMSG('PIXIE','PELC_CASH_PLOT',
     &      'PPHO Bank does not exist.','W')
          GOTO 999
        ENDIF
        DO I = 1,IELC-1
          LPPHO1 = LPPHO
          LPPHO=LQ(LPPHO)
          IF (LPPHO .LE. 0) THEN
            LPPHO = LPPHO1
            GOTO 31
          ENDIF
        ENDDO
   31   CONTINUE
        WRITE(STRING,202)I-1
        LCACL = LQ(LPPHO - 2)
      ENDIF
  202 FORMAT('ELECTRON/PHOTON SELECTED = ',I3)
C-
      IF (LCACL .LE. 0) THEN
        CALL ERRMSG('PIXIE','PELC_CASH_PLOT',
     &    'CACL Bank does not exist.','W')
        GOTO 999
      ENDIF
C-
      LCASH = LQ(LCACL-2)
      IF (LCASH .LE. 0) THEN
        CALL ERRMSG('PIXIE','PELC_CASH_PLOT',
     &    'CASH Bank does not exist.','W')
        GOTO 999
      ENDIF
C-
      IF ( FIXEMAX )   THEN
        EADD = 0.
        ENER_EM3 = 0.
        CALL PU_CEMDPTH(LCASH,IETA_HOT,IPHI_HOT,EDPTH,PDPTH,ENERGY_HOT)
        DO J = 1,8
          EADD = EADD + ENERGY_HOT(J)
          IF (J .GE. 3 .AND. J .LE. 6) THEN
            ENER_EM3 = ENER_EM3+ENERGY_HOT(J)
          ENDIF
        ENDDO
        IF (EMAX .LT. EADD ) THEN
          EMAX = EADD
        ENDIF
      ENDIF
C-**********FILLING ETA ARRAY
      IF(LAYMIN .NE. 3) THEN
        MIN_ETA = IETA_HOT(LAYMIN) - 2
        MAX_ETA = IETA_HOT(LAYMIN) + 2
        IF( (MAX_ETA .LE. 0 .AND. MIN_ETA .GE. 0).OR. 
     &    (MAX_ETA .GE. 0 .AND. MIN_ETA .LE. 0)) THEN   !DIFFERENT SIGNS
          IF(IETA_HOT(LAYMIN) .GT. 0) THEN
            MIN_ETA = IETA_HOT(LAYMIN) - 3
          ELSE
            MAX_ETA = IETA_HOT(LAYMIN) + 3
          ENDIF
        ENDIF
        J = 0
        DO I = 1,5
          IF(MIN_ETA + J .EQ. 0) J=J+1
          META(I) = MIN_ETA + J
          J= J+1
        ENDDO
C-**********FILLING PHI ARRAY
        MIN_PHI = IPHI_HOT(LAYMIN) - 2
        MAX_PHI = IPHI_HOT(LAYMIN) + 2
        DO I = 1,5
          MPHI(I) = MIN_PHI + I - 1
          IF(MPHI(I) .LE. 0) MPHI(I) = MPHI(I) + 64
          IF(MPHI(I) .GT. 64) MPHI(I) = MPHI(I) - 64
        ENDDO
      ELSE                                    !LAYMIN = 3
  131   CONTINUE
        IF( ENERGY_HOT(LAYMIN).LE.0) THEN 
          IF( LAYMIN.LT.LAYMAX)THEN
            LAYMIN = LAYMIN+1
            GOTO 131
          ENDIF
        ENDIF
        MIN_ETA = IETA_HOT(LAYMIN) - 2
        MAX_ETA = IETA_HOT(LAYMIN) + 2
        IF( (MAX_ETA .LE. 0 .AND. MIN_ETA .GE. 0).OR. 
     &    (MAX_ETA .GE. 0 .AND. MIN_ETA .LE. 0)) THEN   !DIFFERENT SIGNS
          IF(IETA_HOT(LAYMIN) .GT. 0) THEN
            MIN_ETA = IETA_HOT(LAYMIN) - 3
          ELSE
            MAX_ETA = IETA_HOT(LAYMIN) + 3
          ENDIF
        ENDIF
        J = 0
        DO I = 0,4
          IF(MIN_ETA + J .EQ. 0) J=J+1
          META3(2*I+1) = MIN_ETA + J
          J= J+1
        ENDDO
        MIN_PHI = IPHI_HOT(LAYMIN) - 2
        MAX_PHI = IPHI_HOT(LAYMIN) + 2
        DO I = 0,4
          MPHI3(2*I+1) = MIN_PHI + I
          IF(MPHI3(2*I+1) .LE. 0) MPHI3(2*I+1) = MPHI3(2*I+1) + 64
          IF(MPHI3(2*I+1) .GT. 64) MPHI3(2*I+1) = MPHI3(2*I+1) - 64
        ENDDO
      ENDIF
C-
      CALL PUOPEN
      CALL PU_GET_VIEWPORT_SIZE(VXMIN,VXMAX,VYMIN,VYMAX,XMIN,XMAX,
     &  YMIN,YMAX)
      CALL JCOLOR(0)
      LAYMIN = LAYMIN3
      IF(LAYMIN .EQ. 3 .AND. LAYMAX .EQ. 6) THEN
        DENOM = 10.
        COUNT = 9.
      ELSE
        DENOM = 5.
        COUNT = 4.
      ENDIF
C*******************
C- Define Rectangle Coordinates
C*******************
      XMIN1 = XMIN+(XMAX-XMIN)/5.
      XMAX1 = XMAX-(XMAX-XMIN)/5.
      YMIN1 = YMIN+(YMAX-YMIN)/5.
      YMAX1 = YMAX-(YMAX-YMIN)/5.
      IF (IN_COUNT .GT. 4) GOTO 222      !--- PLOT HISTOGRAM
C-**********
C---DRAWING OF THE GRID
C-**********
      CALL JRECT(XMIN1,YMIN1,XMAX1,YMAX1)
      DO I =0,COUNT
        CALL JMOVE(XMIN1,YMIN1+(YMAX1-YMIN1)*I/DENOM)
        CALL JDRAW(XMAX1,YMIN1+(YMAX1-YMIN1)*I/DENOM)
      ENDDO
      DO I = 0,COUNT
        CALL JMOVE(XMIN1+(XMAX1-XMIN1)*I/DENOM,YMIN1)
        CALL JDRAW(XMIN1+(XMAX1-XMIN1)*I/DENOM,YMAX1)
      ENDDO
C-***********
C---END GRID
C-***********
   20 CONTINUE
      NRP   = 2               ! repetition number
      NCH   = IQ(LCASH+2)     ! number of channels
C-***********
C---Fill energy Arrays
C-***********
      DO 100 I=1,NCH
        LDCASH = LCASH+(I-1)*NRP
        INDCES = IQ(LDCASH+3)
        ENER   =  Q(LDCASH+4)
        IE = BYTES(BYTE4)
        IP = BYTES(BYTE3)
        IL = BYTES(BYTE2)
        IF (IL.LT.LAYMIN .OR. IL.GT.LAYMAX) GO TO 100
        LAYMIN1 = LAYMIN
        IF (LAYMIN .EQ. 7)LAYMIN1=4
        IF (IL .ge. 8)LAYMIN1=5
        IF (ENER .LE. 0.) GO TO 100
        ESUM(LAYMIN1) = ESUM(LAYMIN1)+ENER
C-***********
C--- FOR LAYER 3
C-***********
        mult_eta = 0
        mult_phi = 0
        IF (IL .GE. 3 .AND.IL .LE. 6)THEN
          IF (IL .EQ. 3 .OR. IL .EQ. 4) THEN
            DO II = 0,4
              IF (META3(2*II+1) .EQ. IE) MULT_ETA = 2*II+1
            ENDDO
          ELSE
            DO II = 0,4
              IF (META3(2*II+1) .EQ. IE) MULT_ETA = 2*II+1 + 1
            ENDDO
          ENDIF
          IF(IL .EQ. 3 .OR. IL .EQ. 5) THEN
            DO II = 0,4
              IF (MPHI3(2*II+1) .EQ. IP) MULT_PHI = 2*II+1
            ENDDO
          ELSE
            DO II = 0,4
              IF (MPHI3(2*II+1) .EQ. IP) MULT_PHI = 2*II+1 + 1
            ENDDO
          ENDIF
          IF(MULT_ETA .GE. 1 .AND. MULT_ETA .LE. 10) THEN
            IF(MULT_PHI .GE. 1 .AND. MULT_PHI .LE. 10) THEN
              ENER_ARR3(MULT_ETA,MULT_PHI) = ENER + 
     &          ENER_ARR3(MULT_ETA,MULT_PHI)
            ENDIF
          ENDIF
        ELSE
          DO II = 1,5
            IF (META(II) .EQ. IE) MULT_ETA = II
            IF (MPHI(II) .EQ. IP) MULT_PHI = II
          ENDDO
          IF(MULT_ETA .GE. 1 .AND. MULT_ETA .LE. 5) THEN
            IF(MULT_PHI .GE. 1 .AND. MULT_PHI .LE. 5) THEN
              ENER_ARR(MULT_ETA,MULT_PHI) = ENER
            ENDIF
          ENDIF
        ENDIF
  100 CONTINUE
C-***********
C-Fill Energetic Cells
C-***********
      DO 110 i = 1,10
        DO 210 J = 1,10
          STEP = (XMAX1-XMIN1)/10.
          ENER = ENER_ARR3(I,J)*4
          IF (LAYMIN .LT. 3 .OR. LAYMIN .GT. 6)THEN
            STEP = (XMAX1 - XMIN1)/5.
            ENER = ENER_ARR(I,J)
            IF( J .GT. 5)GOTO 110
            IF(I .GT. 5) GOTO 211
          ENDIF
          IF(ENER .GT. 0) THEN
            XBOX(1)=XMIN1+STEP*(I-1)
            XBOX(2)=XBOX(1)
            XBOX(3)=XBOX(1)+STEP
            XBOX(4)=XBOX(3)
            YBOX(1)=YMIN1+STEP*(J-1)
            YBOX(2)=YBOX(1)+STEP
            YBOX(3)=YBOX(2)
            YBOX(4)=YBOX(1)
            COLRNGE=1.0
            CALL PCECOL(ENER,0.0,COLRNGE,IC,CCOL,NUM)
            CALL PXCOLN('CDC',IC,3,.FALSE.,KCOL,KINT,KFIL,KSTY)
            IF(RLEVEL .EQ. -2.) THEN
              CALL PXCOLR(CCOL)
              CALL JPOLGN(XBOX,YBOX,4)
            ELSE
              CALL PXCOLFILL(CCOL)
              CALL JPOLGN(XBOX,YBOX,4)
            ENDIF
          ENDIF
  210   CONTINUE
  110 CONTINUE
  211 CONTINUE
C-***********
C-Put labels
C-***********
      STEP = (XMAX1-XMIN1)/5.
      IL = LAYMIN
      LAYMIN1 = LAYMIN
      IF(LAYMIN .LE. 7) THEN
        IF(LAYMIN .EQ. 7) LAYMIN1 = 4
C        CALL JJUST(2,2)
      WRITE(STRING,1002)LAYMIN1,ESUM(LAYMIN1)
      ENDIF
 1002 FORMAT('LAYER=',I1,'         ESUM=',F6.2)
      IF(IL .GE. 3 .AND. IL .LE. 6) IL = 3
      if(il .ge.8) il = 8
      CALL JFONT(5)
      CALL JSIZE(7.0,7.0)
      XJUST = 2
      YJUST = 2
      CALL JJUST(XJUST,YJUST)
      CALL JMOVE(0.0,YMax-(YMax-YMIN)/30.)
      CALL JHSTRG(STRING(1:LENOCC(STRING)))
      IF(IL .EQ. 3)THEN
        CALL JMOVE(0.0,YMAX-(YMAX-YMIN)/30.-7.0*1.5)
        CALL JHSTRG('           (Energy=Energy*4)')
      ENDIF
      CALL JMOVE(0.0,YMIN+(YMAX-YMIN)/30.)
      CALL JHSTRG('Eta')
      RJ = 0.66
        XJUST = 2
        YJUST = 2
      DO I = 1,5
        CALL JFONT(5)
        CALL JSIZE(7.0,7.0)
        CALL JJUST(XJUST,YJUST)
        CALL JMOVE(XMIN1+STEP*(I-1)+STEP*RJ/2.,YMIN1+YMIN1/4)
        IF (IL .EQ. 3) THEN
          WRITE(STRING,1221)META3(2*I-1)
        ELSE
          WRITE(STRING,1221)META(I)
        ENDIF
        CALL JHSTRG(STRING(1:LENOCC(STRING)))
      ENDDO
      XJUST = 2
      YJUST = 2
      CALL JJUST(xjust,yjust)
      CALL JMOVE(XMIN+(XMAX-XMIN)/20,0.0)
      CALL JPATH(2)
      CALL JHSTRG('Phi')
      CALL JPATH(1)
      J = 6
      DO I = 1,5
        CALL JFONT(5)
        CALL JSIZE(7.0,7.0)
        CALL JJUST(3,2)
        CALL JMOVE(XMIN+(XMAX-XMIN)/J,YMIN1+STEP*(I-1)+STEP/2.)
        IF(IL .EQ. 3) THEN
          WRITE(STRING,1221)MPHI3(2*I-1)
        ELSE
          WRITE(STRING,1221)MPHI(I)
        ENDIF
        CALL JHSTRG(STRING(1:LENOCC(STRING)))
      ENDDO
 1221 FORMAT(I3)
      CALL JRCLOS
      CALPLOT = .TRUE.                 ! Cal Plot E
C-
      IF ( FIXEMAX )   THEN
        ZMAX = EMAX
      ENDIF
C-
      IF (IN_COUNT .GT. 4) GOTO 222
      GOTO 999
C-
C--- Reset IN_COUNT 
C-
  222 CONTINUE
      NRP   = 2               ! repetition number
      NCH   = IQ(LCASH+2)     ! number of channels
C-***********
C-Sum Energy for Various Layers
C-***********
      DO 1000 I=1,NCH
        LDCASH = LCASH+(I-1)*NRP
        INDCES = IQ(LDCASH+3)
        ENER   =  Q(LDCASH+4)
        IE = BYTES(BYTE4)
        IP = BYTES(BYTE3)
        IL = BYTES(BYTE2)
        IF (IL.LT.LAYMIN .OR. IL.GT.LAYMAX) GO TO 1000
        LAYMIN1 = LAYMIN
        IF (LAYMIN .EQ. 7)LAYMIN1=4
        IF (IL .ge. 8)LAYMIN1=5
        IF (ENER .LE. 0.) GO TO 1000
        ESUM(LAYMIN1) = ESUM(LAYMIN1)+ENER
 1000 CONTINUE
C-***********
C-Draw Energy Histogram
C-***********
      IF (IN_COUNT .GT. 4) THEN
        IF(IN_COUNT .EQ. 5) THEN
          CALL JMOVE(XMIN,YMIN+(YMAX-YMIN)/5)
          CALL JDRAW(XMAX,YMIN+(YMAX-YMIN)/5)
C--- FIND MAX ENERGY
          EMAX = ESUM(1)
          DO J = 1,5
            IF (EMAX .LT. ESUM(J))EMAX=ESUM(J)
          ENDDO
          YMAX1 = YMAX-(YMAX-YMIN)/40
          XVAL(1) = XMIN+(XMAX-XVAL(1))*0.08
          YVAL(1) = YMIN+(YMAX-YMIN)/5
          DO J = 1,5
            YVAL(2*J) = YVAL(1)+YMAX1*ESUM(J)/EMAX
            YVAL(2*J+1) = YVAL(2*J)
            XVAL(2*J) = XVAL(1)+(XMAX-XVAL(1))*(J-1)/5
            XVAL(2*J+1) = XVAL(1)+(XMAX-XVAL(1))*(J)/5
          ENDDO
          XVAL(12)=XMAX
          YVAL(12)=YMIN+(YMAX-YMIN)/5
          CALL JPOLY(XVAL,YVAL,12)
C--- DRAWING SCALE MARKING
          DO J = 1,4
            CALL JMOVE(XVAL(2*J+1),YVAL(1))
            CALL JDRAW(XVAL(2*J+1),YVAL(1)-YVAL(1)*0.07)
          ENDDO
C--- WRITING AXIS LABLES
          CALL JFONT(5)
          CALL JGAP(1.0)
          CALL JSIZE(3.0,7.0)
          CALL JJUST(2,2)
          CALL JPATH(2)
          CALL JMOVE(XVAL(1),YVAL(1))
          CALL JDRAW(XVAL(1),YVAL(1)+YMAX1)
          CALL JMOVE(XMIN+(XMAX-XMIN)*0.02,YMAX-(YMAX-YMIN1)/2)
          CALL JHSTRG('ENERGY')
          CALL JPATH(1)
          CALL JSIZE(4.0,9.0)
          XJUST = 3
          YJUST = 2
          CALL JJUST(XJUST,YJUST)
          CALL JMOVE(XVAL(1)+(XMAX-XMIN)*0.1,YVAL(1)+YMAX1)
          WRITE(STRING,1001)EMAX
1001      FORMAT(F8.2)
          CALL JGAP(1)
          CALL JHSTRG(STRING(1:LENOCC(STRING)))
          JUMP = (XVAL(3)-XVAL(1))/2
          JUMP = JUMP 
          CALL JMOVE(XVAL(1)+JUMP,YMIN+(YMAX-YMIN)/10.)
          CALL JHSTRG('EM1')
          CALL JMOVE(XVAL(3)+JUMP,YMIN+(YMAX-YMIN)/10.)
          CALL JHSTRG('EM2')
          CALL JMOVE(XVAL(5)+JUMP,YMIN+(YMAX-YMIN)/10.)
          CALL JHSTRG('EM3')
          CALL JMOVE(XVAL(7)+JUMP,YMIN+(YMAX-YMIN)/10.)
          CALL JHSTRG('EM4')
          CALL JMOVE(XVAL(9)+JUMP,YMIN+(YMAX-YMIN)/10.)
          CALL JHSTRG('FH1')
          CALL JRCLOS
          GOTO 999
        ELSE
          IF(IN_COUNT .EQ. 6) THEN            !put Legends
            IN_COUNT = 0
            CALL VZERO(XVAL,12)
            CALL VZERO(YVAL,12)
            CALL JRCLOS
            CALL PCELAB(COLORS,LABELS)
            DO I = 1,NUM
            CALL PXCOLITOC(COLORS(I),CCOLORS(I))
            ENDDO
            CALL CASH_LEGEND(CCOLORS,LABELS,NUM)
            IF(EP(1:1) .EQ. 'E') THEN
              CALL PELC_INFO(LPELC,EP)
            ELSE
              CALL PELC_INFO(LPPHO,EP)
            ENDIF
          ENDIF
        ENDIF
      ENDIF
C---
C-
  999 RETURN
      END

