      SUBROUTINE FPICHK(PITRAK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Find pi to mu decays that occur in the FDC.
C-
C-   Outputs : MUTRAK = TRUE if there is a pi->mu kink in FDC
C-
C-   Created  16-AUG-1990   Jeffrey Bantly
C-   Updated  17-SEP-1991   Susan K. Blessing  Change size of (I)QTRAK
C-    to accomodate theta and phi errors and two spare words.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZISP1.LINK'
      INCLUDE 'D0$LINKS:IZISV2.LINK'
      INCLUDE 'D0$LINKS:IZISP2.LINK'
      INCLUDE 'D0$INC:PI.DEF'
C
      INTEGER MXZTRK,MXISP2,MAXHIS,MAXHIS2,HOFSET
      PARAMETER( MXZTRK = 10 )
      PARAMETER( MXISP2 = 10 )
      PARAMETER( MAXHIS = 106 )
      PARAMETER( MAXHIS2 = 1000 )
      PARAMETER( HOFSET = 0 )
      INTEGER IDV1, ID1, IDV2, MID2, ID2(MXISP2)
      INTEGER LISV1,LISP1,LISV2,LISP2,FISV2
      INTEGER NISV1,NISP1,NISV2,NISP2,MISP1,MISP2
      INTEGER IOUT,IERR,NPIONS,NPIONS2
      INTEGER NMUONS2,MMUONS,MPIONS,FPIONS,IVTYPE,ICONT(10)
      INTEGER IFQTRAK(26),IVQTRAK(21),IDOFF,IDJ,IOFF,JISP2
      INTEGER LKZTRK,GZZTRK,LKZFIT,GZZFIT,IZTRK,I,IP2,TRKSTAT
      INTEGER IVTRK,IFTRK,PITRAK,NZTRK,NFDC,NVTX,ID,IZ,KP2(MXZTRK)
      INTEGER PIONID1,PIONID2,MUONID1,MUONID2,IDK,LADDER(0:2)
      INTEGER NID(MAXHIS2),IDH,NID2
C
      REAL    MASS1,PV1(4),PP1(4),X1,Y1,Z1,PHI1,THETA1,ETA1,PPERP1
      REAL    MASS2,PV2(4),PP2(4,MXISP2),X2,Y2,Z2
      REAL    MPHI2,MTHETA2,META2,MPP2(4)
      REAL    PHI2(MXISP2),THETA2(MXISP2),ETA2(MXISP2),PPERP2(MXISP2)
      REAL    ANGLE(MXISP2),COSANGLE,RADIUS,NUM,DENOM1,DENOM2
      REAL    ZPHI(MXZTRK),ZTHETA(MXZTRK),ZEPHI(MXZTRK)
      REAL    ZETHETA(MXZTRK),ZIONIZ(MXZTRK)
      REAL    ZX0(MXZTRK),ZY0(MXZTRK),ZR0(MXZTRK),ZZ0(MXZTRK)
      REAL    FPHI(MXZTRK),FTHETA(MXZTRK),FEPHI(MXZTRK)
      REAL    FETHETA(MXZTRK),FIONIZ(MXZTRK)
      REAL    FX0(MXZTRK),FY0(MXZTRK)
      REAL    VPHI(MXZTRK),VTHETA(MXZTRK),VEPHI(MXZTRK)
      REAL    VETHETA(MXZTRK),VIONIZ(MXZTRK)
      REAL    VX0(MXZTRK),VY0(MXZTRK),VS0(MXZTRK),VZ0(MXZTRK)
      REAL    FQTRAK(26),FQHSEC(3,34),FCONT(26)
      REAL    VQTRAK(21),VQHSEC(4,24),VQHZLA(3,6),VCONT(21)
      REAL    MINDIF_PHI,DIF_PHI,MINDIF_THETA,DIF_THETA
      REAL ISHIST(4,MAXHIS)
      EQUIVALENCE ( FQTRAK,IFQTRAK  )
      EQUIVALENCE ( VQTRAK,IVQTRAK  )
C
      LOGICAL MUTRAK,INSIDE_FDC,SEC_PION,SEC_MUON,WRAP,PI_PAIR,MU_PAIR
      LOGICAL INSIDE_VTX,BEYOND_CD,IN_BEAMPIPE,BETWEEN_VTXFDC
      LOGICAL FIRST,PRRECO
C
      CHARACTER*34 NAME(41),NAME2(MAXHIS-41),NAMED
      DATA FIRST/.TRUE./
      DATA NAME/
     & ' Position of all ISV2 in Z',' Radius of all ISV2',
     & ' ISV2 Z VS RADIUS','ISV2 Z VS RADIUS',
     & ' Angle bet init pion + sec',
     & ' Energy bet init pion+sec',
     & ' P perp bet pion and sec',' P perp bet pion and sec',
     & ' Angle bet pi,sec ISV2inFDC',
     & ' Energy bet pi,sec ISV2inFDC',
     & ' ISV2 particle type','ISP2 particle type',
     & ' ISV2 Z VS ZPHI-PHI1',' ISV2 Z VS ZPHI-PHI1',
     & ' ISV2 Z VS ZPHI-PHI2',' ISV2 Z VS ZPHI-PHI2',
     & ' ISV2 Z VS ZTHETA-THETA1',' ISV2 Z VS ZTHETA-THETA1',
     & ' ISV2 Z VS ZTHETA-THETA2',' ISV2 Z VS ZTHETA-THETA2',
     & ' Z TRACK IONIZATION',
     & ' Z POS VS (FDC-VTX) PHI',' Z POS VS (FDC-VTX) PHI',
     & ' Z POS VS (FDC-VTX) THETA',' Z POS VS (FDC-VTX) THETA',
     & ' VTX IONIZ VS FDC IONIZ',' VTX IONIZ VS FDC IONIZ',
     & ' Num ISP2 sec pion perISV2',' Num ISP2 banks per event',
     & ' Num ISV2 banks per event',' P Perp of ISP2 pion',
     & ' P Perp of ISP2 muon',' P Perp of ISP2 other',
     & ' ISV2 all vertex type',' ISV2 pion vertex type',
     & ' ISV2 muon vertex type',' ISV2 other vertex type',
     & ' general position of ISV2',' Num of pion ISV2 in FDC',
     & ' ISV2 particle type',' ISP2 particle type'/
      DATA NAME2/
     & ' ISV2 Z vs ISP2 Pperp pis',' ISV2 Z vs ISP2 Pperp pis',
     & ' ISV2 Z vs ISP2 Pperp mus',' ISV2 Z vs ISP2 Pperp mus',
     & ' ISV2 Z vs ISP2 Pperp other',' ISV2 Z vs ISP2 Pperp other',
     & ' Type of ZTRAK-0,v,f,vf',' ISP2 muons per ISV2',
     & ' Num ISP2 pions per evt',' Num ISP2 muons per evt',
     & ' ZTRK-ISP2, Phi vs Theta',' ZTRK-ISP2, Phi vs Theta',
     & ' ZTRK-ISP2, 2Phi vs Theta',' ZTRK-ISP2, 2Phi vs Theta',
     & ' ISV2 R vs Z mu- only',' ISV2 R vs Z mu- only',
     & ' ISV2 R vs Z mu- + mu+',' ISV2 R vs Z mu- + mu+',
     & ' ISV2 R vs Z pi- only',' ISV2 R vs Z pi- only',
     & ' ISV2 R vs Z pi- + pi+',' ISV2 R vs Z pi- + pi+',
     & ' IV11 - particle type',' IV11 - angle init pi,sec',
     & ' IV11 - R vs Z',' IV11 - R vs Z',
     & ' IV12 - particle type',' IV12 - angle init pi,sec',
     & ' IV12 - R vs Z',' IV12 - R vs Z',
     & ' IV999- particle type',' IV999- angle init pi,sec',
     & ' IV999- R vs Z',' IV999- R vs Z',
     & ' IVoth- particle type',' IVoth- angle init pi,sec',
     & ' IVoth- R vs Z',' IVoth- R vs Z',
     & ' IV11 - particle type',' IV12 - particle type',
     & ' IV999- particle type',' IVoth- particle type',
     & ' ISV2 Z VS VPHI-PHI1',' ISV2 Z VS VPHI-PHI1',
     & ' ISV2 Z VS VTHETA-THETA1',' ISV2 Z VS VTHETA-THETA1',
     & ' ISV2 Z VS FPHI-PHI1',' ISV2 Z VS FPHI-PHI1',
     & ' ISV2 Z VS FTHETA-THETA1',' ISV2 Z VS FTHETA-THETA1',
     & ' ISV2 Z VS VPHI-PHI2',' ISV2 Z VS VPHI-PHI2',
     & ' VTRK-ISP2, Phi vs Theta',' VTRK-ISP2, Phi vs Theta',
     & ' VTRK-ISP2, 2Phi vs Theta',' VTRK-ISP2, 2Phi vs Theta',
     & ' ISV2 Z VS FPHI-PHI2',' ISV2 Z VS FPHI-PHI2',
     & ' FTRK-ISP2, Phi vs Theta',' FTRK-ISP2, Phi vs Theta',
     & ' FTRK-ISP2, 2Phi vs Theta',' FTRK-ISP2, 2Phi vs Theta',
     & ' ISV2 Z POSITION',' ISV2 RADIUS',' PRIMARY-SECONDARY ANGLE'/
C----------------------------------------------------------------------
C
      CALL DHDIR('FTRAKS_RCP','H2BOOK_DIRECTOR',IERR,' ')
      IF(IERR.NE.0) THEN
        CALL ERRMSG('FTRAKS','PICHCK',' ERROR SETTING HBK DIR','W')
      ENDIF
C
      IF (FIRST) THEN    ! Book histograms
        CALL EZPICK('FTRAKS_RCP')
        CALL EZGET('PRRECO',PRRECO,IERR)
        CALL EZGET('ISHIST(1)',ISHIST(1,1),IERR)
        CALL EZRSET
        IDJ=0
        DO 10 IDH=1,MAXHIS
          IF(IDH.LE.41) THEN
            NAMED=NAME(IDH)
          ELSE
            NAMED=NAME2(IDH-41)
          ENDIF
          IF (ISHIST(1,IDH).EQ.1.) THEN
            CALL HBOOK1(IDH+HOFSET,NAMED,
     &           NINT(ISHIST(2,IDH)),ISHIST(3,IDH),ISHIST(4,IDH),0.)
          ENDIF
          IF (ISHIST(1,IDH).EQ.2.) THEN
            CALL HBOOK2(IDH+HOFSET,NAMED,NINT(ISHIST(2,IDH)),
     &               ISHIST(3,IDH),ISHIST(4,IDH),NINT(ISHIST(2,IDH+1)),
     &               ISHIST(3,IDH+1),ISHIST(4,IDH+1),0.)
          ENDIF
          IF (ISHIST(1,IDH).EQ.3.) IDJ=IDJ+1
   10   CONTINUE
        DO IOFF=200,700,50
          DO IDH=MAXHIS-IDJ+1,MAXHIS
            NAMED=NAME2(IDH-41)
            IDK=IDH-(MAXHIS-IDJ)
            IF (ISHIST(1,IDH).EQ.3.) THEN
              CALL HBOOK1(IOFF+IDK+HOFSET,NAMED,
     &           NINT(ISHIST(2,IDH)),ISHIST(3,IDH),ISHIST(4,IDH),0.)
            ENDIF
          ENDDO
        ENDDO
        FIRST = .FALSE.
      END IF
C
C ****  Accumulate ZTRAKS bank contents for comparisons.
C
      CALL GTZTRH(ICONT)    ! fetch number of ZTRAKS
      NZTRK=ICONT(2)
C
      CALL GTFTRH(ICONT)    ! fetch number of FTRAKS
      NFDC = ICONT(2)
C
      CALL GTVTXH(ICONT)    ! fetch number of VTRAKS
      NVTX = ICONT(2)
C
      IF(PRRECO) WRITE(61,*) ' PICHCK - NZTRK,NFDC,NVTX=',
     &                           NZTRK,NFDC,NVTX
      DO 500 IZTRK=1,NZTRK
        LKZTRK=GZZTRK(IZTRK)
        IF(LKZTRK.LE.0) GOTO 500
        LKZFIT=GZZFIT(IZTRK)
        ZPHI(IZTRK)   =0.0
        ZTHETA(IZTRK) =0.0
        ZEPHI(IZTRK)  =0.0
        ZETHETA(IZTRK)=0.0
        ZIONIZ(IZTRK) =0.0
        ZX0(IZTRK)    =0.0
        ZY0(IZTRK)    =0.0
        ZR0(IZTRK)    =0.0
        ZZ0(IZTRK)    =0.0
        IF(LKZFIT.GT.0) THEN
          ZPHI(IZTRK)   =Q(LKZFIT+10)
          ZTHETA(IZTRK) =Q(LKZFIT+13)
          ZEPHI(IZTRK)  =Q(LKZFIT+16)
          ZETHETA(IZTRK)=Q(LKZFIT+18)
          ZIONIZ(IZTRK) =Q(LKZFIT+26)
          ZX0(IZTRK)    =Q(LKZFIT+11)
          ZY0(IZTRK)    =Q(LKZFIT+12)
          ZR0(IZTRK)    =Q(LKZFIT+14)
          ZZ0(IZTRK)    =Q(LKZFIT+15)
        ENDIF
C
        IVTRK=IQ(LKZTRK+2)
        IFTRK=IQ(LKZTRK+4)
C
C  Get associated VTX track info, if any.
C
        VPHI(IZTRK)   =0.0
        VTHETA(IZTRK) =0.0
        VEPHI(IZTRK)  =0.0
        VETHETA(IZTRK)=0.0
        VIONIZ(IZTRK) =0.0
        VX0(IZTRK)    =0.0
        VY0(IZTRK)    =0.0
        VS0(IZTRK)    =0.0
        VZ0(IZTRK)    =0.0
C
        IF(IVTRK.GT.NVTX .OR. IVTRK.LE.0) GOTO 600
        CALL GTVTXT(IVTRK,VCONT,VQHSEC,VQHZLA)
        CALL UCOPY(VCONT,VQTRAK,21)
        IF(IVQTRAK(2).EQ.0) GOTO 600
        VPHI(IZTRK)=VQTRAK(6)
        VTHETA(IZTRK)=VQTRAK(9)
        VEPHI(IZTRK)=VQTRAK(16)
        VETHETA(IZTRK)=VQTRAK(18)
        VIONIZ(IZTRK)=VQTRAK(20)
        VX0(IZTRK)   =VQTRAK(7)
        VY0(IZTRK)   =VQTRAK(8)
        VS0(IZTRK)   =VQTRAK(10)
        VZ0(IZTRK)   =VQTRAK(11)
  600   CONTINUE            ! End VTXT
C
C  get associated FDC track info, if any.
C
        FPHI(IZTRK)   =0.0
        FTHETA(IZTRK) =0.0
        FEPHI(IZTRK)  =0.0
        FETHETA(IZTRK)=0.0
        FIONIZ(IZTRK) =0.0
        ZX0(IZTRK)    =0.0
        ZY0(IZTRK)    =0.0
C
        IF(IFTRK.GT.NFDC .OR. IFTRK.LE.0) GOTO 700
        CALL GTFDCT(IFTRK,FCONT,FQHSEC,LADDER)
        CALL UCOPY(FCONT,FQTRAK,26)
        IF(IFQTRAK(2).EQ.0) GOTO 700
        FPHI(IZTRK)=FQTRAK(6)
        FTHETA(IZTRK)=FQTRAK(22)
C                    FEPHI(IZTRK)=FQTRAK(16)
C                    FETHETA(IZTRK)=FQTRAK(18)
        FIONIZ(IZTRK)=FQTRAK(20)
        FX0(IZTRK)   =FQTRAK(4)
        FY0(IZTRK)   =FQTRAK(5)
  700   CONTINUE            ! End FDCT
C
        IF(LKZFIT.LE.0) THEN
          ZPHI(IZTRK)   = FPHI(IZTRK)
          ZTHETA(IZTRK) = FTHETA(IZTRK)
          ZEPHI(IZTRK)  = 0.0
          ZETHETA(IZTRK)= 0.0
          ZIONIZ(IZTRK) = FIONIZ(IZTRK)
        ENDIF
C
        TRKSTAT=0
        IF(IVTRK.GT.0) TRKSTAT=1
        IF(IFTRK.GT.0) TRKSTAT=TRKSTAT+2
        IF (ISHIST(1,48).EQ.1.) CALL HFF1(48+HOFSET,NID(48),
     &                                       FLOAT(TRKSTAT),1.)
C
  500 CONTINUE                             ! End of ZTRAKS loop
C
      NISV1=0
      NISP1=0
      NISV2=0
      NISP2=0
      NMUONS2=0
      NPIONS=0
      NPIONS2=0
      FPIONS=0
C
      LISV1=0
  100 CALL GTISV1(LISV1,LISV1,IDV1,PV1,X1,Y1,Z1)        ! CHECK FOR ISV1
      IF(LISV1.LE.0) GOTO 101
      NISV1=NISV1+1
      MISP1=0
      LISP1=LISV1-IZISP1
  200 CALL GTISP1(LISP1,LISP1,ID1,PP1,PHI1,THETA1,ETA1)   ! CHECK FOR ISP1
      IF(LISP1.LE.0) GOTO 201
      NISP1=NISP1+1
      MISP1=MISP1+1
      IF(ID1 .NE. -120) THEN
        WRITE(*,*) ' FAILED - NO PION, ID=',ID1
        GOTO 200
      ENDIF
      NPIONS=NPIONS+1
      FISV2=0
      LISV2=LISP1-IZISV2
  300 CALL GTISV2(LISV2,LISV2,IDV2,PV2,X2,Y2,Z2,IVTYPE) ! CHECK FOR DECAY
      IF(LISV2.LE.0) GOTO 301
      IF(IVTYPE.NE.11) GOTO 300
C
      SEC_PION=.FALSE.
      SEC_MUON=.FALSE.
      PI_PAIR=.FALSE.
      MU_PAIR=.FALSE.
      PIONID1=0
      PIONID2=0
      MUONID1=0
      MUONID2=0
      NISV2=NISV2+1
      IF (ISHIST(1,1).EQ.1.) CALL HFF1(1+HOFSET,NID(1),Z2,1.)
      RADIUS=SQRT(X2**2.+Y2**2.)
      IF (ISHIST(1,2).EQ.1.) CALL HFF1(2+HOFSET,NID(2),RADIUS,
     &          1.)
      IF (ISHIST(1,3).EQ.2.) CALL HFF2(3+HOFSET,NID(3),
     &            Z2,RADIUS,1.)
      IF (ISHIST(1,11).EQ.1.) CALL HFF1(11+HOFSET,NID(11),
     &                                             FLOAT(IDV2),1.)
      IF (ISHIST(1,40).EQ.1.) CALL HFF1(40+HOFSET,NID(40),
     &                                             FLOAT(IDV2),1.)
C
C  Locate the secondary vertex.
C
      INSIDE_FDC    =.FALSE.
      INSIDE_VTX    =.FALSE.
      BEYOND_CD     =.FALSE.
      IN_BEAMPIPE   =.FALSE.
      BETWEEN_VTXFDC=.FALSE.
      IDOFF=700
      IF( ABS(Z2).LE. 58.4 ) THEN
        IF( RADIUS .LE. 16.2 .AND. RADIUS .GE. 3.7 ) THEN
          INSIDE_VTX=.TRUE.
          IDOFF=200
          IF (ISHIST(1,38).EQ.1.) CALL HFF1(38+HOFSET,NID(38),2.,1.)
        ENDIF
      ENDIF
      IF( ABS((ABS(Z2)-120.0)) .LE. 20.0 ) THEN
        IF( RADIUS .LE. 60. .AND. RADIUS .GE. 11.0 ) THEN
          INSIDE_FDC=.TRUE.
          IDOFF=400
          IF (ISHIST(1,38).EQ.1.) CALL HFF1(38+HOFSET,NID(38),4.,1.)
        ENDIF
      ENDIF
      IF( ABS(Z2) .GT. 140. .OR. RADIUS .GT. 62. ) THEN
        BEYOND_CD=.TRUE.
        IDOFF=500
        IF (ISHIST(1,38).EQ.1.) CALL HFF1(38+HOFSET,NID(38),5.,1.)
      ENDIF
      IF( RADIUS .LT. 3.7 ) THEN
        IN_BEAMPIPE=.TRUE.
        IDOFF=600
        IF (ISHIST(1,38).EQ.1.) CALL HFF1(38+HOFSET,NID(38),1.,1.)
      ENDIF
      IF( .NOT. INSIDE_VTX .AND. .NOT. INSIDE_FDC ) THEN
        IF( .NOT. BEYOND_CD .AND. .NOT. IN_BEAMPIPE ) THEN
          BETWEEN_VTXFDC=.TRUE.
          IDOFF=300
          IF (ISHIST(1,38).EQ.1.) CALL HFF1(38+HOFSET,NID(38),3.,1.)
        ENDIF
      ENDIF
C
C ****  Loop through all the ISP2 banks associated with the current ISV2 bank.
C
      MISP2=0
      MMUONS=0
      LISP2=LISV2-IZISP2
  400 CALL GTISP2(LISP2,LISP2,MID2,MPP2,MPHI2,MTHETA2,META2)
      IF(LISP2.GT.0 .AND. MISP2.LT.10) THEN
        IF(MID2.NE.14) GOTO 400
        DO IZTRK=1,NZTRK
          IF(PRRECO) WRITE(61,1001) IZTRK,ZPHI(IZTRK),ZTHETA(IZTRK),
     &           ZX0(IZTRK),ZY0(IZTRK),ZR0(IZTRK),ZZ0(IZTRK)
 1001     FORMAT(' ZTRAK - ',I2,' phi,theta=',2F8.3,' x0,y0,r0,z0=',4F8.
     &      2)
          IF(PRRECO) WRITE(61,1002) IVTRK,VPHI(IZTRK),VTHETA(IZTRK),
     &           VX0(IZTRK),VY0(IZTRK),VS0(IZTRK),VZ0(IZTRK)
 1002     FORMAT(' VTRAK - ',I2,' phi,theta=',2F8.3,' x0,y0,r0,z0=',4F8.
     &      2)
          IF(PRRECO) WRITE(61,1003) IFTRK,FPHI(IZTRK),FTHETA(IZTRK),
     &           FX0(IZTRK),FY0(IZTRK)
 1003     FORMAT(' FTRAK - ',I2,' phi,theta=',2F8.3,'   x0,y0=',2F8.2)
        ENDDO
        IF(PRRECO) WRITE(61,*) ' PICHCK - IDV1,X1,Y1,Z1=',
     &                                    IDV1,X1,Y1,Z1
        IF(PRRECO) WRITE(61,1004) ID1,PHI1,THETA1,ETA1
 1004   FORMAT(' PICHCK - ID1,PHI1,THETA1,ETA1=',I4,3F8.3)
        IF(PRRECO) WRITE(61,1005) IDV2,X2,Y2,Z2,RADIUS
 1005   FORMAT(' PICHCK - IDV2,X2,Y2,Z2,RADIUS=',I4,4F8.3)
        IF(PRRECO) WRITE(61,1006) MID2,MPHI2,MTHETA2,META2
 1006   FORMAT(' PICHCK - ID2,PHI2,THETA2,ETA2=',I4,3F8.3)
        NISP2=NISP2+1
        MISP2=MISP2+1
C
        ID2(MISP2)    =MID2
        DO I=1,4
          PP2(I,MISP2)=MPP2(I)
        ENDDO
        PHI2(MISP2)   =MPHI2
        THETA2(MISP2) =MTHETA2
        ETA2(MISP2)   =META2
C
        NUM = PP1(1)*PP2(1,MISP2) + PP1(2)*PP2(2,MISP2) +
     &                                        PP1(3)*PP2(3,MISP2)
        DENOM1 = SQRT(PP1(1)**2. + PP1(2)**2. + PP1(3)**2.)
        DENOM2 = SQRT(PP2(1,MISP2)**2. + PP2(2,MISP2)**2. +
     &                                        PP2(3,MISP2)**2.)
        COSANGLE = NUM / (DENOM1*DENOM2)
        ANGLE(MISP2) = ACOS(COSANGLE)
        PPERP1= SQRT(PP1(1)**2. + PP1(2)**2.)
        PPERP2(MISP2)= SQRT(PP2(1,MISP2)**2. + PP2(2,MISP2)**2.)
C
C        IF(ID2(MISP2).EQ. 14) THEN
        SEC_MUON=.TRUE.
        MUONID1=MISP2
C          NMUONS2=NMUONS2+1
C          MMUONS=MMUONS+1
C        ENDIF
        IF( ABS(Z2).LE. 58.4 ) THEN
          IF( RADIUS .LE. 16.2 .AND. RADIUS .GE. 3.7 ) THEN
            INSIDE_VTX=.TRUE.
            IDOFF=200
            IF (ISHIST(1,39).EQ.1.) CALL HFF1(39+HOFSET,NID(39),2.,1.)
          ENDIF
        ENDIF
        IF( ABS((ABS(Z2)-120.0)) .LE. 20.0 ) THEN
          IF( RADIUS .LE. 60. .AND. RADIUS .GE. 11.0 ) THEN
            INSIDE_FDC=.TRUE.
            IDOFF=400
            IF (ISHIST(1,39).EQ.1.) CALL HFF1(39+HOFSET,NID(39),4.,1.)
          ENDIF
        ENDIF
        IF( ABS(Z2) .GT. 140. .OR. RADIUS .GT. 62. ) THEN
          BEYOND_CD=.TRUE.
          IDOFF=500
          IF (ISHIST(1,39).EQ.1.) CALL HFF1(39+HOFSET,NID(39),5.,1.)
        ENDIF
        IF( RADIUS .LT. 3.7 ) THEN
          IN_BEAMPIPE=.TRUE.
          IDOFF=600
          IF (ISHIST(1,39).EQ.1.) CALL HFF1(39+HOFSET,NID(39),1.,1.)
        ENDIF
        IF( .NOT. INSIDE_VTX .AND. .NOT. INSIDE_FDC ) THEN
          IF( .NOT. BEYOND_CD .AND. .NOT. IN_BEAMPIPE ) THEN
            BETWEEN_VTXFDC=.TRUE.
            IDOFF=300
            IF (ISHIST(1,39).EQ.1.) CALL HFF1(39+HOFSET,NID(39),3.,1.)
          ENDIF
        ENDIF
C
C ****  Fill ISP2 vs ISP1 histograms.
C
        IF(ISHIST(1,5).EQ.1.) CALL HFF1(5+HOFSET,NID(5),ANGLE(MISP2),
     &              1.)
        IF(ISHIST(1,6).EQ.1.) CALL HFF1(6+HOFSET,NID(6),
     &              (PP1(4)-PP2(4,MISP2)),1.)
        IF(ISHIST(1,7).EQ.2.) CALL HFF2(7+HOFSET,NID(7),
     &                 PPERP1,PPERP2(MISP2),1.)
        IF(PRRECO) WRITE(61,1007) ANGLE(MISP2),PP1(4),PP2(4,MISP2),
     &        PPERP1,PPERP2(MISP2)
 1007   FORMAT(' PICHCK - ANGLE,PP1(4),PP2(4),PPERP1,PPERP2=',5F8.3)
C
      ENDIF
      IF(LISP2.GT.0) GOTO 400              ! End of valid ISP2 banks
C
      MU_PAIR=.FALSE.
      IF(SEC_MUON) THEN
        DO IP2=1,MISP2
          IF(ID2(IP2).EQ.-14) THEN
            MU_PAIR=.TRUE.
            MUONID2=IP2
          ENDIF
        ENDDO
      ENDIF
C
      IF(MU_PAIR) IDOFF=IDOFF+50
C
      JISP2=1
      IF(SEC_MUON .AND. .NOT.MU_PAIR) THEN
        IF (ISHIST(1,56).EQ.2.) CALL HFF2(56+HOFSET,NID(56),
     &            Z2,RADIUS,1.)
        JISP2=MUONID1
      ELSEIF(SEC_MUON .AND. MU_PAIR) THEN
        IF (ISHIST(1,58).EQ.2.) CALL HFF2(58+HOFSET,NID(58),
     &            Z2,RADIUS,1.)
        JISP2=MUONID2
      ENDIF
      IF(SEC_MUON) THEN
        DO ID=1,IDJ
          IF(ID.EQ.1) CALL HFF1(ID+IDOFF,NID(ID+IDOFF),Z2,1.)
          IF(ID.EQ.2) CALL HFF1(ID+IDOFF,NID(ID+IDOFF),RADIUS,1.)
          IF(ID.EQ.3) CALL HFF1(ID+IDOFF,NID(ID+IDOFF),ANGLE(JISP2),1.)
        ENDDO
      ENDIF
C
C  Fill histograms.  NEED LOOPS ETC
C
      DO 800 IZ=1,NZTRK
        MINDIF_THETA=1.0
        MINDIF_PHI=1.0
        WRAP=.FALSE.
        KP2(IZ)=0
        DO 801 IP2=1,MISP2
          DIF_THETA=ABS(ZTHETA(IZ)-THETA2(IP2))
          DIF_PHI=ABS(ZPHI(IZ)-PHI2(IP2))
          IF(DIF_THETA.LE. MINDIF_THETA) THEN
            IF(DIF_PHI.LE.MINDIF_PHI) THEN
              MINDIF_THETA=DIF_THETA
              MINDIF_PHI  =DIF_PHI
              WRAP=.FALSE.
              KP2(IZ)=IP2
            ELSEIF((ABS(TWOPI-DIF_PHI)).LE.MINDIF_PHI) THEN
              MINDIF_THETA=DIF_THETA
              MINDIF_PHI  =TWOPI-DIF_PHI
              WRAP=.TRUE.
              KP2(IZ)=IP2
            ENDIF
          ENDIF
  801   CONTINUE
C
        IF(SEC_MUON) THEN
          IF (ISHIST(1,13).EQ.2.) CALL HFF2(13+HOFSET,NID(13),
     &                 Z2,ABS(ZPHI(IZ)-PHI1),1.)
          IF (ISHIST(1,17).EQ.2.) CALL HFF2(17+HOFSET,NID(17),
     &                 Z2,ABS(ZTHETA(IZ)-THETA1),1.)
          IF(IVQTRAK(2).GT.0) THEN
            IF (ISHIST(1,84).EQ.2.) CALL HFF2(84+HOFSET,NID(84),
     &                 Z2,ABS(VPHI(IZ)-PHI1),1.)
            IF (ISHIST(1,86).EQ.2.) CALL HFF2(86+HOFSET,NID(86),
     &                 Z2,ABS(VTHETA(IZ)-THETA1),1.)
          ENDIF
          IF(IFQTRAK(2).GT.0) THEN
            IF (ISHIST(1,88).EQ.2.) CALL HFF2(88+HOFSET,NID(88),
     &                 Z2,ABS(FPHI(IZ)-PHI1),1.)
            IF (ISHIST(1,90).EQ.2.) CALL HFF2(90+HOFSET,NID(90),
     &                 Z2,ABS(FTHETA(IZ)-THETA1),1.)
          ENDIF
C
          IF (IVQTRAK(2).LE.0 .OR. IFQTRAK(2).LE.0) GOTO 802
          IF (ISHIST(1,22).EQ.2.) CALL HFF2(22+HOFSET,NID(22),
     &                 Z2,ABS(FPHI(IZ)-VPHI(IZ)),1.)
          IF (ISHIST(1,24).EQ.2.) CALL HFF2(24+HOFSET,NID(24),
     &                 Z2,ABS(FTHETA(IZ)-VTHETA(IZ)),1.)
        ENDIF
C        IF(PRRECO) THEN
C          WRITE(61,*) ' PICHCK - FPHI,FTHETA,VPHI,VTHETA=',
C     &        FPHI(IZ),FTHETA(IZ),VPHI(IZ),VTHETA(IZ)
C        ENDIF
  802   CONTINUE
C
        IF(KP2(IZ).LE.0) GOTO 800
C
        IF(.NOT.WRAP) THEN
          IF (ISHIST(1,15).EQ.2.) CALL HFF2(15+HOFSET,NID(15),
     &                 Z2,ABS(ZPHI(IZ)-PHI2(KP2(IZ))),1.)
          IF (ISHIST(1,52).EQ.2.) CALL HFF2(52+HOFSET,NID(52),
     &      ABS(ZTHETA(IZ)-THETA2(KP2(IZ))),
     &      ABS(ZPHI(IZ)-PHI2(KP2(IZ))),1.)
        ELSE
          IF (ISHIST(1,15).EQ.2.) CALL HFF2(15+HOFSET,NID(15),
     &                 Z2,(TWOPI-ABS(ZPHI(IZ)-PHI2(KP2(IZ)))),1.)
          IF (ISHIST(1,54).EQ.2.) CALL HFF2(54+HOFSET,NID(54),
     &      ABS(ZTHETA(IZ)-THETA2(KP2(IZ))),
     &      (TWOPI-ABS(ZPHI(IZ)-PHI2(KP2(IZ)))),1.)
        ENDIF
C
        IF( (VPHI(IZ)-PHI2(KP2(IZ))).LT.PI ) THEN
          IF (ISHIST(1,92).EQ.2.) CALL HFF2(92+HOFSET,NID(92),
     &                 Z2,ABS(ZPHI(IZ)-PHI2(KP2(IZ))),1.)
          IF (ISHIST(1,94).EQ.2.) CALL HFF2(94+HOFSET,NID(94),
     &      ABS(ZTHETA(IZ)-THETA2(KP2(IZ))),
     &      ABS(ZPHI(IZ)-PHI2(KP2(IZ))),1.)
        ELSE
          IF (ISHIST(1,92).EQ.2.) CALL HFF2(92+HOFSET,NID(92),
     &                 Z2,(TWOPI-ABS(ZPHI(IZ)-PHI2(KP2(IZ)))),1.)
          IF (ISHIST(1,96).EQ.2.) CALL HFF2(96+HOFSET,NID(96),
     &      ABS(ZTHETA(IZ)-THETA2(KP2(IZ))),
     &      (TWOPI-ABS(ZPHI(IZ)-PHI2(KP2(IZ)))),1.)
        ENDIF
C
        IF( (FPHI(IZ)-PHI2(KP2(IZ))).LT.PI ) THEN
          IF (ISHIST(1,98).EQ.2.) CALL HFF2(92+HOFSET,NID(98),
     &                 Z2,ABS(ZPHI(IZ)-PHI2(KP2(IZ))),1.)
          IF (ISHIST(1,100).EQ.2.) CALL HFF2(100+HOFSET,NID(100),
     &      ABS(ZTHETA(IZ)-THETA2(KP2(IZ))),
     &      ABS(ZPHI(IZ)-PHI2(KP2(IZ))),1.)
        ELSE
          IF (ISHIST(1,98).EQ.2.) CALL HFF2(98+HOFSET,NID(98),
     &                 Z2,(TWOPI-ABS(ZPHI(IZ)-PHI2(KP2(IZ)))),1.)
          IF (ISHIST(1,102).EQ.2.) CALL HFF2(102+HOFSET,NID(102),
     &      ABS(ZTHETA(IZ)-THETA2(KP2(IZ))),
     &      (TWOPI-ABS(ZPHI(IZ)-PHI2(KP2(IZ)))),1.)
        ENDIF
        IF (ISHIST(1,19).EQ.2.) CALL HFF2(19+HOFSET,NID(19),
     &                 Z2,ABS(ZTHETA(IZ)-THETA2(KP2(IZ))),1.)
        IF(PRRECO) THEN
C          WRITE(61,*) ' PICHCK - IZTRK,ZPHI,PHI1,PHI2=',IZ,
C     &                ZPHI(IZ),PHI1,PHI2(KP2(IZ))
C          WRITE(61,*) ' PICHCK - IP2,ZTHETA,THETA1,THETA2=',
C     &                KP2(IZ),ZTHETA(IZ),THETA1,THETA2(KP2(IZ))
        ENDIF
C
  800 CONTINUE
C
      GOTO 300                             ! End of valid ISV2 banks
  301 CONTINUE
C
      GOTO 200
  201 CONTINUE                             ! End of valid ISP1 banks
      GOTO 100
  101 CONTINUE                             ! End of valid ISV1 banks
C
  900 CONTINUE
C----------------------------------------------------------------------
  999 CONTINUE
      RETURN
      END
