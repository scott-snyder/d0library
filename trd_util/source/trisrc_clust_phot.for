      SUBROUTINE TRISRC_CLUST_PHOT(LPELC_IN,LCACL_IN,LPPHO_IN,IER)

C----------------------------------------------------------------------
C-
C-   Purpose and Methods :  Define roads between EM calo an dvertex to look for
C-                          TRD hits
C-
C-   Inputs  : pointers to bank PELC,PPHO,CACL. In the call only one of these 3
C-   arguments must be different from zero
C-             icas =0 direction taken from ZFIT, icas=1 taken from PELC
C-             ier: =0 if part. crosses the TRD
C-   Outputs :
C-
C-   Created   27-DEC-1993   A. Zylberstejn  : adapted from TRISRC_TRACK
C-   Updated  24-JAN-1996 L.T. Goss changed RMIN from RADEXT(2) to RADEXT(1)
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:geomtr.INC'
      INCLUDE 'D0$INC:TCNTRL.INC'
      INCLUDE 'D0$INC:TRINTR.INC'
      INCLUDE 'D0$INC:worksp.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:zlinkc.INC'
      INTEGER CAS,IER,NERR,NEVOLD,OFFS,IFOIS,IK,ICAS,IAUX(4)
      INTEGER LVERT,GZVERT,GZVERH,NVERT,NZBANK
      REAL XCL,YCL,ZCL,VECT
      INTEGER LCACL_IN,LPELC_IN,LPPHO_IN
      INTEGER I,IDIN,IVERT,ITR
      LOGICAL DIR_FROM_ZFIT,DIR_FROM_PELC,DIR_FROM_PPHO,DO_HISTO
      INTEGER IKK1,IKK2,IUCOMP,IZ,LOUT,TRUNIT
      INTEGER LOC,NGOOD,IGTRAC,IDENT,ICDI,IVTI,IFDI
      INTEGER GZZTRH,IVT(NTOTTR),ICD(NTOTTR),IFD(NTOTTR)
      INTEGER NVT,NDT,KG,IDUM,NFT
      REAL CT,VIN(6),VOUT(6),PHI,ST,TETA,THETA,QVTRK(21)
      REAL PHILO,PHIHI,TOLPHI,WIN(6)
      REAL PHII(100),TETAI(100),RGI,ZGI,PI
      INTEGER IDI
      LOGICAL DOPRINT,FIRST,TRD_DO_PRINT
      CHARACTER*4 C4
      CHARACTER*3 C3
      DATA PHILO,PHIHI,TOLPHI/0.,6.2832,0.0736/
      DATA NERR/0/,IFOIS/0/
      DATA FIRST/.TRUE./
      IF(FIRST)THEN
        FIRST=.FALSE.
        NEVOLD=0
        LOUT=TRUNIT()
        PI=ACOS(-1.)
        ZMIN=83.
        DIR_FROM_ZFIT=.FALSE.
        DIR_FROM_PELC=.FALSE.
        ICAS=2
        RMIN=RADEXT(1)
        CALL EZPICK('TRD_RCP')
        DO_HISTO=.FALSE.
        CALL EZGET('HSTBOK',IAUX,IER)
        IF(IER.EQ.0)THEN
          CALL UHTOC(IAUX(1),3,C3,3)
          DO_HISTO=C3.EQ.'Y' .OR. C3.EQ.'y' .OR. C3.EQ.'YES'
        END IF
C        CALL EZGET('PHOTON_CASE',icas,IER)
        CALL EZRSET
C        IF(LPPHO_IN.LE.0)THEN
C          IF(ICAS.LE.0)THEN
C            DIR_FROM_ZFIT=.TRUE.
C            DIR_FROM_PELC=.FALSE.
C            PRINT*,' DIRECTIONS FOR ELECTRONS ARE TAKEN FROM ZFIT'
C          ELSE
C            DIR_FROM_PELC=.TRUE.
C            DIR_FROM_ZFIT=.FALSE.
C            PRINT*,' DIRECTIONS FOR ELECTRONS ARE TAKEN FROM PELC'
C          END IF
C        ELSE
C        DIR_FROM_PPHO=.TRUE.
C        PRINT*,' DIRECTION FOR PHOTON TAKEN FOR PPHO'
C        END IF
C        PRINT*,' in TRISRC_CLUST_PHOT, DIR_FROM_PELC ',DIR_FROM_PELC,
C     &    ' DIR_FROM_ZFIT ',DIR_FROM_ZFIT,' DIR_FROM_PPHO',DIR_FROM_PPHO
      END IF
      IFOIS=IFOIS+1
      DOPRINT=TRD_DO_PRINT().AND.LOUT.GT.0
      IF(DOPRINT)
     +  WRITE(LOUT,4032)LPELC_IN,LPPHO_IN,LCACL_IN
 4032 FORMAT(
     &  ' Enter TRISRC_CLUST_phot with LPELC_IN,LPPHO_in,LCACL_IN=',
     &  3I8,/,'--------------------')
      IDENT=2
C      IF(LPELC_IN.GT.NNQ .OR. LPPHO_IN.GT.NNQ .OR.
      IF( LPELC_IN.LE.0 .AND. LPPHO_IN.LE.0 .AND.LCACL_IN.LE.0)THEN
        CALL ERRMSG('wrong values for input arguments ',
     &    'TRISRC_CLUST_phot',' ','w')
        GO TO 999
      END IF
      IF(IQ(LHEAD+9).NE.NEVOLD)THEN ! do the following only once per event
        NEVOLD=IQ(LHEAD+9)
        KG=0
        NVT=0
        NDT=0
        NFT=0
      END IF
      IF(DIR_FROM_ZFIT)THEN
        LZTRK=LQ(LPELC_IN-3)
        IF( LZTRK.LE.0)THEN
          CALL ERRMSG('No ztrk bank ',
     &      'TRISRC_CLUST_phot',' ','w')
          GO TO 999
        END IF
        IF(LQ(LZTRK-7).LE.0)THEN ! select CDC tracks
C          PRINT*,' no cdc track for icas=',ICAS
          GO TO 999
        END IF
        LZFIT=LQ(LZTRK-1)
        VIN(1)=Q(LZFIT+11)
        VIN(2)=Q(LZFIT+12)
        VIN(3)=Q(LZFIT+15)
        VIN(4)=Q(LZFIT+20)
        VIN(5)=Q(LZFIT+22)
        VIN(6)=Q(LZFIT+24)
        IF(DOPRINT)THEN
          WRITE(LOUT,*)' cos dir zfit',VIN(4),VIN(5),VIN(6)
        END IF
C  Check if track crosses the TRD
        CALL EXTCYL ( VIN, VOUT, RMIN, IGTRAC )
C
C ****  Fill the INFO array
        IF ( IGTRAC .NE. 0 .OR. ABS(VOUT(3) ) .GT. ZMIN )THEN
          IF(DOPRINT)WRITE(LOUT,*)' part does not cross TRD 1 VIN',
     &      (VIN(IK),IK=1,6),' rmin',RMIN,' zmin',ZMIN
C          WRITE(LOUT,*)' rmin,igtrac',RMIN,IGTRAC,' zout',VOUT(3)
          GO TO 999 !
        END IF
      ELSE
        LVERT = GZVERT(1)
        IF (LVERT .LE. 0) THEN
          CALL ERRMSG('Cant find bank VERT ','TRISRC_CLUST_phot',' ',
     &        'w')
          GO TO 999
        ENDIF
C
C ****  Count the number of vertex banks
C
        NVERT = NZBANK(IXCOM, LVERT)
        IF(NVERT.LE.0)THEN
          CALL ERRMSG('No vertex  ','TRISRC_CLUST_phot',' ',
     &        'w')
          GO TO 999
        END IF
C        PRINT*,' nvert',NVERT
        IF(LPELC_IN.NE.0)THEN
          IF(DIR_FROM_PELC) THEN ! direction from pelc
Cc            PRINT*,' no pelc bank for icas=',ICAS
C            GO TO 999
            LZTRK=LQ(LPELC_IN-3)
            IF(LZTRK.LE.0)THEN
C              PRINT*,' no ztrk bank for icas=',ICAS
              GO TO 999
            END IF
            XCL=Q(LPELC_IN+23)
            YCL=Q(LPELC_IN+24)
            ZCL=Q(LPELC_IN+25)
            C4='pelc'
          END IF
        ELSE IF(LPPHO_IN.NE.0)THEN!(DIR_FROM_PPHO)
          XCL=Q(LPPHO_IN+23)
          YCL=Q(LPPHO_IN+24)
          ZCL=Q(LPPHO_IN+25)
          C4='ppho'
C        LHMTP=LQ(LPPHO_IN-1)
C        IF(LHMTP.NE.0)THEN
C          PRINT*,' x,y,z,hmtp',Q(LHMTP+13),Q(LHMTP+14),
C     &      Q(LHMTP+15)
C        END IF
C      END IF
C      CALL ERRMSG(' illegal argument  ',' TRISRC_TRACK',' ','W')
C      GO TO 999
        ELSE IF(LCACL_IN.NE.0)THEN!(DIR_FROM_CAcL)
C          PRINT*,' in trisrc_clust_phot, lcacl_in',lcacl_in
          XCL=Q(LCACL_IN+14)
          YCL=Q(LCACL_IN+15)
          ZCL=Q(LCACL_IN+16)
          C4='cacl'
   24     CONTINUE
          IER=1
        END IF
C          PRINT*,' ',c4,' x,y,z ',XCL,YCL,ZCL,' zcut',zmin+20
C        IF(ABS(ZCL).GT.ZMIN+20.)GO TO 999 ! request cluster in central region
C loop on vertices
C        print*,' avant 82 nvert',nvert
        DO 82 I = 1,NVERT ! loop on vertices
C          PRINT*,' i',I
          LVERT = GZVERT(I)
C          PRINT*,' lvert',LVERT
          VIN(1)=Q(LVERT+3)
          VIN(2)=Q(LVERT+4)
          VIN(3)=Q(LVERT+5)
          IF(DOPRINT)WRITE(LOUT,*)
     +      'x,y,z vertex',VIN(1),VIN(2),VIN(3)
          ICDI=0
          IVTI=0
          IFDI=0
          IKK1      =  0
          IKK2      =  0
          NGOOD     =  0
C          WRITE(LOUT,*)' x,y,z vertex',VIN(1),VIN(2),VIN(3)
          VECT=SQRT((XCL-VIN(1))**2+(YCL-VIN(2))**2+(ZCL-VIN(3))**2)
C  compute director cosines
          VIN(4)=(XCL-VIN(1))/VECT
          VIN(5)=(YCL-VIN(2))/VECT
          VIN(6)=(ZCL-VIN(3))/VECT
C          END IF
          CALL EXTCYL ( VIN, VOUT, RMIN, IGTRAC )
C            WRITE(LOUT,*)' rmin,igtrac',RMIN,IGTRAC,' zout',VOUT(3)
          IF ( IGTRAC .NE. 0 .OR. ABS(VOUT(3) ) .GT. ZMIN )THEN
            IF(DOPRINT)WRITE(LOUT,*)' part does not cross TRD 1 VIN',
     &          (VIN(IK),IK=1,6),' rmin',RMIN,' zmin',ZMIN
            GO TO 82
          END IF
          GO TO 84
   82   CONTINUE
C        WRITE(LOUT,*)' part does not cross TRD 1 VIN',
C     &          (VIN(IK),IK=1,6),' rmin',RMIN,' zmin',ZMIN
        GO TO 999 ! no ppho-vertex track in the TRD
   84   CONTINUE
        IER=0
      END IF
C ****  This is a good track : stores its characteristics
      IDENT=0
      CALL SBIT1(IDENT,7)
      CALL SBIT1(IDENT,4)
      CALL TRDFIL(VIN,PHI,IDENT)
      IF(DOPRINT)WRITE(LOUT,*)' track type ',C4,' accepted in TRD'
      IER=0
      IF(DO_HISTO)      CALL DIST_TRACK_ANODE(VIN,DISTAN)
  999 CONTINUE
      RETURN
      END
