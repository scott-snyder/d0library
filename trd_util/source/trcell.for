      SUBROUTINE TRCELL(VIN,TRANOD,TRCATH,IERR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Compute TRD hit anodes and cathodes for a
C-                         given track
C-                         Algorithms as in CLUPOS.FOR
C-
C-   Inputs  : VIN= parameters of the track
C-   Outputs : TRANOD(1,j) =anode number at the entrance point for layer #j
C-                   (2,j) =  ""   ""             exit
C-             TRCATH(1,j) =anode number at the entrance point for layer #j
C-                   (2,j) =  ""   ""             exit
C-   Controls:
C-
C-   Created  23-FEB-1989   A. Zylberstejn
C-   Updated  13-OCT-1992   A. Zylberstejn  take plus AND minus one cell around
C-                                          the cell hit by the track
C-   Updated  26-NOV-1992   A. Zylberstejn  Add half a cell on both side for
C-                                          layer 3
C-   Updated   7-DEC-1992   A. Zylberstejn  : go back to adding only the cell
C-   the closest to the track
C-   Updated   7-FEB-1995   Qizhong Li-Demarteau  Fixed EZGET call for memory
C-                                                overwritting problem
C-   Updated  16-JUN-1995   A. ZYLBERSTEJN  store r,theta,phi,z in
C-                                                  TRD_PHI_Z.INC'
C-   Updated  20-JUN-1996   A. Zylberstejn  : fix a bug affecting cathodes for
C-   layer 3 in region of cath. nb. =256
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:FIRSHT.INC'
      INCLUDE 'D0$INC:GCONST.INC/LIST'
      INCLUDE 'D0$INC:GEOMTC.INC/LIST'
      INCLUDE 'D0$INC:GEOMTR.INC/LIST'
      INCLUDE 'D0$INC:TCNTRL.INC'
      INCLUDE 'D0$INC:TRDBGU.INC'
      INCLUDE 'D0$INC:TRDVOL.INC'
      INCLUDE 'D0$PARAMS:TRD_NB_OF_WIRES.PARAMS'
      INCLUDE 'D0$INC:TRD_NB_OF_WIRES.inc'
      INCLUDE 'D0$INC:TRWCOD_512.INC'
      INCLUDE 'D0$INC:TRD_PHI_Z.INC'
      INCLUDE 'D0$INC:WORKSP.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC'
C
      INTEGER I,LLG,TRANOD(2,3),TRCATH(2,3),JAIN,JAOUT
      INTEGER DNCATO,LOUT,TRUNIT,TCHNB
      INTEGER IDENT,LL,LGTRH,GZGTRH,LVREF,LPREF
      INTEGER ICH,IGTRAK,K,IAIN,IAOUT,II,ICIN,ICOUT,IER,IERR,IWG
      INTEGER IANOD,ICATHOD,ISWHISG(12),KG,KAIN,KAOUT,LAIN,LAOUT
      REAL VIN(6),VOUT(6),RIN,PHITR,PHICAT,DPHIST,X,Y,ZIN,PHIS
      REAL DPHICG,PRICH(6),RX,RY
      REAL CPHOF(3),SPHOF(3),DPHI,DPHI1,DPHI2,PHIG,PHIR,PHIT,ZG
      REAL PHIGCH(3),PHIIN,THETIN
      CHARACTER*3 C3,HSTBOK,SWHISG(12)
      CHARACTER*1 C1
      CHARACTER*4 DO_HISTO(4)
      INTEGER NHSTBOK
      LOGICAL FIRST,TESTTG,DOPRINT,ADD_ONE_CELL,HEXIST,TRD_DO_PRINT
C----------------------------------------------------------------------
C
      DATA FIRST/.TRUE./
      IF(FIRST)THEN
        LGTRH=0
        LOUT=TRUNIT()
        FIRST=.FALSE.
C  NOT ELEGANT:SHOULD BE DONE IN INITRD AND TRANSMITTED
C                                             THROUGH COMMON BLOCK
        DO 31 ICH = 1,  3
          CPHOF(ICH)=COS(OFSDPH(ICH))
          SPHOF(ICH)=SIN(OFSDPH(ICH))
   31   CONTINUE
        TESTTG=.FALSE.
        CALL EZPICK('TRD_RCP')
        CALL EZGET('GENERAL_TRD_HISTOS',ISWHISG(1),IER)
        DO I=1,12
          CALL UHTOC(ISWHISG(I),3,SWHISG(I),3)
        END DO
        CALL EZGET('DEB_GEANT',K,IER)
        TESTTG=K.EQ.1
        CALL EZ_GET_CHARS('HSTBOK',NHSTBOK,DO_HISTO,IER)
C        CALL EZGET('HSTBOK',HSTBOK,IER)
        ADD_ONE_CELL=.FALSE.
        CALL EZGET('ADD_ONE_CELL',I,IER)
        CALL UHTOC(I,3,C3,3)
        IF(IER.EQ.0 .AND. (C3.EQ.'Y' .OR. C3.EQ.'y'))ADD_ONE_CELL=.TRUE.
        CALL EZRSET
      END IF
      DOPRINT=TRD_DO_PRINT()
C      WRITE(C1,FMT='(i1)')HSTBOK
      IF(DO_HISTO(1).EQ.'Y')CALL HCDIR('//PAWC/TRD',' ')  ! go to TRD directory
      CALL VZERO(TRANOD,6)
      CALL VZERO(TRCATH,6)
      IERR=0
      CALL VZERO(PRICH,3)
      IF(DOPRINT .AND. TESTTG)THEN
        LGTRH=GZGTRH()
        THETIN=ACOS(VIN(6))
        PHIIN=ATAN2(-VIN(5),-VIN(4))+PI
        IF(LGTRH.NE.0)  THEN
          CALL GTISAT(LVREF,LPREF,PHIIN,THETIN)
        END IF
      END IF
      PHIR=(ATAN2(-VIN(5),-VIN(4))+PI)*RADDEG
      DO 100 ICH=1,3
C  --------------------------------------------------
C  COMPUTE HIT ANODE AND CATHODE AT THE ENTRANCE POINT
C  --------------------------------------------------
        WS(4500+ICH)= -1000.
        RIN=RADWIN(ICH)
        CALL EXTCYL(VIN,VOUT,RIN,IGTRAK)
        IF(DOPRINT .AND. SWTDBG.GT.1)
     +      WRITE(LOUT,2044)IGTRAK,ICH,VOUT(3),RIN,ZACT(2)
        IF(IGTRAK.NE.0)GO TO 100
        IF(ABS(VOUT(3)).GT.ZACT(2))GO TO 100
        ZIN=VOUT(3)
        X=CPHOF(ICH)*VOUT(1)+SPHOF(ICH)*VOUT(2)
        Y=-SPHOF(ICH)*VOUT(1)+CPHOF(ICH)*VOUT(2)
        PHITR=ATAN2(-Y,-X)+PI
        IAIN=PHITR/DPHIAN(ICH)+1
C        print*,'in trcell,ich',ich,'phitr',phitr,' dphian',
C     &    dphian(ich),' iain',iain
        IF(DOPRINT.AND. SWTDBG.GT.1)
     +    WRITE(LOUT,*)' IN TRCELL X,Y',X,Y,'PHITR',PHITR
C       --------------
        IF(TESTTG)THEN
          LGTRH=GZGTRH()
          IF(LGTRH.NE.0)THEN !look for GEANT track
            LL=LQ(LGTRH-ICH)
            DPHI=1000.
            KG=0
   41       IF(LL.NE.0)THEN
              RX = (Q(LL+4)-Q(LL+7))
              RY = (Q(LL+5)-Q(LL+8))
              KG=KG+1
              IF (RX.EQ.0..AND.RY.EQ.0.) GOTO 44 !Particle going inwards
C                                                 !or not crossing the TRD
              PHIT=(ATAN2(RY,RX)+PI)*RADDEG
              IF(DPHI.LT.ABS(PHIT-PHIR))GO TO 44
C               IF(ABS(Q(LL+3)-VOUT(3)).GT.5.)GO TO 44
              PHIG=PHIT
              DPHI=ABS(PHIT-PHIR)
              ZG=Q(LL+3)
              LLG=LL
              IDENT=0
              IF(IQ(LL+14).GT.3)IDENT=1
              IWG=MOD(IQ(LL+15),1000)
   44         LL=LQ(LL)
              GO TO 41
            END IF
            PHIGCH(ICH)=PHIG
C            WRITE(C1,FMT='(i1)')HSTBOK
C            IF(do_histo(1).EQ.'Y')THEN
C              IF(do_histo(C1.EQ.'Y')THEN
            IF(HEXIST(FIRSHT+510+ICH))THEN
              CALL HF1(FIRSHT+510+ICH,PHIR-PHIG,1.)
C             CALL HF1(FIRSHT+504,PHIG-PHIS,1.)
C             CALL HF1(FIRSHT+505,PHIR-PHIS,1.)
            END IF
          END IF
          DPHI1=PHITR-(FLOAT(IWG)-.5)*DPHIAN(ICH)!ANG. DIST. TO THE WIRE
          DPHICG=DPHI1
        END IF
C       --------------
C
C
        DPHI1=PHITR-(FLOAT(IAIN)-.5)*DPHIAN(ICH)!ANG. DIST. TO THE WIRE
C  CATHODE
        PHICAT=PHITR-OFSDPH(ICH)-DPHIDZ(ICH)*VOUT(3)+TWOPI
        DPHIST=AMOD(PHICAT-OFSCAT(ICH),TWOPI)
        ICIN=DPHIST/DPHICA(ICH)+1.
C  -----------------------------------------------
C  COMPUTE HIT ANODE AND CATHODE AT THE EXIT POINT
C  -----------------------------------------------
        RIN=RADEXT(ICH)
        CALL EXTCYL(VIN,VOUT,RIN,IGTRAK)
        IF(DOPRINT.AND. SWTDBG.GT.1)
     +    WRITE(LOUT,2046)IGTRAK,ICH,VOUT(3)
 2046   FORMAT(' AFTER SECOND EXTCYL,IGTRACK',I4,'ICH',I2,'Z',G10.4)
        IF(IGTRAK.NE.0)GO TO 100
        IF(ABS(VOUT(3)).GT.ZACT(2))GO TO 100
        IF ((TNEVDG.GT.0).AND.(SWTDBG.EQ.1))THEN
          LL=LLG
   51     CONTINUE
        END IF
        X=CPHOF(ICH)*VOUT(1)+SPHOF(ICH)*VOUT(2)
        Y=-SPHOF(ICH)*VOUT(1)+CPHOF(ICH)*VOUT(2)
        PHITR=ATAN2(-Y,-X)+PI
        IAOUT=PHITR/DPHIAN(ICH)+1
        DPHI2=PHITR-(FLOAT(IAOUT)-.5)*DPHIAN(ICH)!ANG. DIST. TO THE WIRE
C  LOOK IF WE ADD OR SUBSTRACT ONE CELL TO THE COMPUTED ONE DEPENDING
C  OF THE POSITION IN THE CELL
C-        IF(.NOT. COSMIC1)THEN
        KAIN=IAIN
        KAOUT=IAOUT
        IF(ICH.EQ.3 .AND. NWIRE_PER_LAYER(3).EQ.256)THEN
          KAIN=(IAIN-1)/2+1
          KAOUT=(IAOUT-1)/2+1
        END IF
        CALL TRCROSS(ICH,KAIN,KAOUT)
        IF(ADD_ONE_CELL)THEN ! add one cell on both side
          IAIN=KAIN
          IAOUT=KAOUT
          II=MIN(IAIN,IAOUT)
          IAOUT=IAIN+IAOUT-II
          IAIN=II
          JAIN =IAIN-1
          JAOUT=IAOUT+1
          IF(JAOUT.GT. NWIRE_PER_LAYER(ICH))JAOUT=JAOUT-
     &      NWIRE_PER_LAYER(ICH)
          IF(JAIN.LE.0)JAIN= NWIRE_PER_LAYER(ICH)+JAIN
          IAIN=JAIN
          IAOUT=JAOUT
        ELSE
          IF(DPHI1.GT.0.)THEN
            JAIN=IAIN+1
            IF(JAIN.GT.NWIRE(ICH))JAIN=JAIN-NWIRE(ICH)
          ELSE
            JAIN=IAIN-1
            IF(JAIN.LT.1)JAIN=NWIRE(ICH)-JAIN
          END IF
          LAIN=JAIN
          IF(ICH.EQ.3 .AND. NWIRE_PER_LAYER(3).EQ.256)LAIN=(JAIN-1)/2+1
          IF(DOPRINT .AND. SWTDBG.GT.1)WRITE(LOUT,*)' IN TRCELL, LAYER',
     &      ICH,' IAIN,JAIN,LAIN',IAIN,JAIN,
     &      LAIN,' TWOCOD',TWCOD(TCHNB(KAIN,ICH)),
     &      TWCOD(TCHNB(LAIN,ICH)),' PHITR',PHITR
          IF(TWCOD(TCHNB(KAIN,ICH)))THEN ! Central Cell is coded
            IF(.NOT.TWCOD(TCHNB(LAIN,ICH)))JAIN=IAIN ! adjacent cell is not coded
          ELSE
            IAIN=JAIN
C            IF(.NOT.TWCOD(TCHNB(LAIN,ICH)))            IAIN=-100
          END IF
          JAOUT=IAOUT
          IF(IAOUT.NE.IAIN)THEN ! entrance cell .ne. exit cell
            IF(DPHI2.GT.0.)THEN
              JAOUT=IAOUT+1
              IF(JAOUT.GT.NWIRE(ICH))JAOUT=JAOUT-NWIRE(ICH)
            ELSE
              JAOUT=IAOUT-1
              IF(JAOUT.LT.1)JAOUT=NWIRE(ICH)-JAOUT
              IF(JAOUT.EQ.0)JAOUT=NWIRE(ICH)
            END IF
            LAOUT=JAOUT
            IF(ICH.EQ.3  .AND. NWIRE_PER_LAYER(3).EQ.256)
     &        LAOUT=(JAOUT-1)/2+1
            IF(DOPRINT.AND. SWTDBG.GT.1)WRITE(LOUT,*)' IN TRCELL,LAYER',
     &        ICH,' IAOUT,JAOUT,LAOUT',IAOUT,JAOUT,
     &        LAOUT,' TWOCOD',TWCOD(TCHNB(KAOUT,ICH)),
     &        TWCOD(TCHNB(LAOUT,ICH))
            IF(TWCOD(TCHNB(KAOUT,ICH)))THEN
              IF(.NOT.TWCOD(TCHNB(LAOUT,ICH)))JAOUT=IAOUT
            ELSE
              IAOUT=JAOUT
            END IF
          END IF
          IF(DOPRINT .AND. SWTDBG.GT.1)THEN
            WRITE(LOUT,*)'IN TRCELL ICH',ICH,'IAIN,IAOUT,JAIN,JAOUT',
     &        IAIN,IAOUT,JAIN,JAOUT,' DPHI1,DPHI2',DPHI1,DPHI2,
     &        ' CODED CELLS ',TWCOD(TCHNB(IAIN,ICH)),
     &        TWCOD(TCHNB(IAOUT,ICH)),TWCOD(TCHNB(JAIN,ICH)),
     &        TWCOD(TCHNB(JAOUT,ICH))
          END IF
          IAIN=MIN(IAIN,IAOUT,JAIN,JAOUT)
          IAOUT=MAX(IAIN,IAOUT,JAIN,JAOUT)
          IF(ICH.EQ.3 .AND. NWIRE_PER_LAYER(3).EQ.256)THEN
            IAIN=(IAIN-1)/2+1
            IAOUT=(IAOUT-1)/2+1
          END IF
C-          JAIN=IAIN
C-          JAOUT=IAOUT
C-      END IF
        END IF
C        IF (DOPRINT .AND.
C     +     ABS(DPHICG*RADDEG).GT.1.3 .AND.PRICH(ICH).EQ.0.)THEN
C          WRITE(LOUT,*)' In trcell,ich,iain,iaout,iwg',ICH,IAIN,
C     &      IAOUT,IWG,' dphi',DPHICG*RADDEG,' phi ISAJET',PHIS
CC           CALL PRGTLY(LOUT,ICH)
C          PRICH(ICH)=1.
C        END IF
        IF(IABS(IAIN-IAOUT).LT.10)THEN
C          IF(IWG.LT.IAIN .OR. IWG.GT.IAOUT)THEN
C            IF(HSTBOK.EQ.'Y' .AND. SWHISG(2).EQ.'Y')
C     +        CALL HF1(FIRSHT+526+ICH,PHIR-PHIS,1.)
C          END IF
        ELSE
          IF(IWG.GT.IAOUT .OR. IWG.LT.IAIN)THEN
            IF (DOPRINT)
     &         WRITE(LOUT,*)'IN TRCELL, IAIN,IAOUT,IWG',IAIN,IAOUT,IWG
C            IF(HSTBOK.EQ.'Y' .AND. SWHISG(2).EQ.'Y')
C     +        CALL HF1(FIRSHT+526+ICH,PHIR-PHIS,1.)
          END IF
        END IF
C  CATHODE
        PHICAT=PHITR-OFSDPH(ICH)-DPHIDZ(ICH)*VOUT(3)+TWOPI
        DPHIST=AMOD(PHICAT-OFSCAT(ICH),TWOPI)
        ICOUT=DPHIST/DPHICA(ICH)+1.
        CALL TRCROSS(ICH+3,ICIN,ICOUT)
        IF ((TNEVDG.GT.0).AND.(SWTDBG.EQ.1))THEN
 2070     FORMAT('IN TRCELL,PHITR,Z,IAIN,ICIN',2G10.4,2I4)
 2072     FORMAT('IN TRCELL,PHITR,Z,IAOUT,ICOUT',2G10.4,2I4)
        END IF
        IF(ICOUT.LT.ICIN)THEN  ! icin =lowest cath, icout highest cath.
          K=ICIN
          ICIN=ICOUT
          ICOUT=K
        END IF
        IF(IABS(IAIN-IAOUT).GT.10)THEN !REGION CLOSE TO PHI =0
          K=IAIN+IAOUT
          IAIN=MAX0(IAIN,IAOUT)
          IAOUT=K-IAIN
        END IF
        IF(IABS(ICOUT-ICIN).GT.10)THEN
          K=ICIN+ICOUT
          ICIN=MAX0(ICOUT,ICIN)
          ICOUT=K-ICIN
        END IF
        TRANOD(1,ICH)=IAIN
        TRANOD(2,ICH)=IAOUT
        DNCATO=IABS(ICIN-ICOUT)
        IF(DNCATO.GT.10)DNCATO= 258-DNCATO
        IF(DNCATO.LT.4)THEN
          TRCATH(1,ICH)=ICIN-1
          TRCATH(2,ICH)=ICOUT+1
          IF(TRCATH(1,ICH).LT.1)TRCATH(1,ICH)= 256
     &      -TRCATH(1,ICH)
          IF(TRCATH(2,ICH).GT. NWIRE_PER_LAYER(ICH))TRCATH(2,ICH)
     &      =TRCATH(2,ICH)- 256
        ELSE
          TRCATH(1,ICH)=ICIN
          TRCATH(2,ICH)=ICOUT
        END IF
C distance wire-anode
        RIN=RADAN(ICH)
        CALL EXTCYL(VIN,VOUT,RIN,IGTRAK)
        IF(DOPRINT .AND. SWTDBG.GT.1)
     +      WRITE(LOUT,2044)IGTRAK,ICH,VOUT(3),RIN,ZACT(2)
        IF(IGTRAK.NE.0)GO TO 100
        IF(ABS(VOUT(3)).GT.ZACT(2))GO TO 100
        PHIR=(ATAN2(-VIN(5),-VIN(4))+PI)*RADDEG
        X=CPHOF(ICH)*VOUT(1)+SPHOF(ICH)*VOUT(2)
        Y=-SPHOF(ICH)*VOUT(1)+CPHOF(ICH)*VOUT(2)
        PHITR=ATAN2(-Y,-X)+PI
C        Phi of the intesection of the track with the anode plane
        WS(4500+ICH)= PHITR
        IWS(4503+ICH)= PHITR/DPHIAN(ICH)+1! Hit cell in the anode plane
        IF(ICH.EQ.3 .AND. NWIRE_PER_LAYER(3).EQ.256)
     +     IWS(4503+ICH)=(IWS(4503+ICH)-1)/2+1
        PHI_TRD(ICH)=PHITR
        THETA_TRD(ICH)=ACOS(VIN(6))
        R_TRD(ICH)=RADAN(ICH)
        Z_TRD(ICH)=VOUT(3)

C  CATHODE
        PHICAT=PHITR-OFSDPH(ICH)-DPHIDZ(ICH)*VOUT(3)+TWOPI
        DPHIST=AMOD(PHICAT-OFSCAT(ICH),TWOPI)
        ICATHOD=DPHIST/DPHICA(ICH)+1.
        IWS(4506+ICH)=ICATHOD
        IF(DOPRINT)THEN
          WRITE(LOUT,4322)ICH,IAIN,IAOUT,
     +      ICIN,ICOUT,IWS(4503+ICH), ICATHOD,PHITR*180./PI
 4322     FORMAT( ' IN TRCELL,ICH',I2,'IAIN',I4,'IAOUT',I4,
     +      'ICIN',I4,'ICOUT',I4,' IANOD',I4,'ICATHOD',I4,
     +      ' PHITR',F7.2)
        END IF
  100 CONTINUE
  999 RETURN
 2044 FORMAT(' IN TRCELL,AFTER FIRST EXTCYL,IGTRACK',I4,'ICH',
     &     I2,'Z',G10.4,' RIN',G10.4,' ZACT(2)',G10.4)
      END
