      SUBROUTINE TRISRC_CLUST_EM (LPELC_IN,LPPHO_IN,LCACL_IN)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :  Define roads between EM calo an dvertex to look for
C-                          TRD hits
C-
C-   Inputs  : pointers to bank PELC,PPHO,CACL. In the call only one of these 3
C-   arguments must be different from zero
C-   Outputs :
C-
C-   Created   27-DEC-1993   A. Zylberstejn  : adapted from TRISRC_TRACK
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:TCNTRL.INC'
      INCLUDE 'D0$INC:TRINTR.INC'
      INCLUDE 'D0$INC:worksp.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:zlinkc.INC'
      INTEGER CAS,IER,NERR,NEVOLD,OFFS,IFOIS
      INTEGER LVERT,GZVERT,GZVERH,NVERT
      REAL XCL,YCL,ZCL,VECT
      INTEGER LCACL_IN,LPELC_IN,LPPHO_IN
      INTEGER I,IDIN,IVERT,ITR
      INTEGER IKK1,IKK2,IUCOMP,IZ,LOUT,TRUNIT
      INTEGER LOC,NGOOD,IGTRAC,IDENT,ICDI,IVTI,IFDI
      INTEGER GZZTRH,IVT(NTOTTR),ICD(NTOTTR),IFD(NTOTTR)
      INTEGER NVT,NDT,KG,IDUM,NFT
      REAL CT,VIN(6),VOUT(6),PHI,ST,TETA,THETA,QVTRK(21)
      REAL PHILO,PHIHI,TOLPHI
      REAL PHII(100),TETAI(100),RGI,ZGI
      INTEGER IDI
      LOGICAL DOPRINT,FIRST,TRD_DO_PRINT
      CHARACTER*3 c3
      DATA PHILO,PHIHI,TOLPHI/0.,6.2832,0.0736/
      DATA NERR/0/,IFOIS/0/
      DATA FIRST/.TRUE./
      IF(FIRST)THEN
        FIRST=.FALSE.
        LOUT=TRUNIT()
      END IF
      IFOIS=IFOIS+1
      DOPRINT=TRD_DO_PRINT()
      LVERH = GZVERH()
      NVERT = IQ(LVERH+2)
      IF(NVERT.LE.0)THEN
        CALL ERRMSG('Cant find bank VERT ','TRISRC_CLUST_EM',' ','w')
        GO TO 999
      END IF
      IF(DOPRINT)
     +  WRITE(LOUT,4032)LPELC_IN,LPPHO_IN,LCACL_IN
 4032 FORMAT(' Enter TRISRC_CLUST_EM with LPELC_IN,LPPHO_in,LCACL_IN=',
     &   3I8,/,'--------------------')
      IF(IQ(LHEAD+9).NE.NEVOLD)THEN ! do the following only once per event
        NEVOLD=IQ(LHEAD+9)
        KG=0
        NVT=0
        NDT=0
        NFT=0
      END IF
        IF(LPELC_IN.NE.0)THEN
          XCL=Q(LPELC_IN+23)
          YCL=Q(LPELC_IN+24)
          ZCL=Q(LPELC_IN+25)
          c3='pelc'
          GO TO 24
        ELSE IF(LPPHO_IN.NE.0)THEN
          XCL=Q(LPPHO_IN+20)
          YCL=Q(LPPHO_IN+21)
          ZCL=Q(LPPHO_IN+22)
          c3='ppho'
          GO TO 24
        ELSE IF (LCACL_IN.NE.0)THEN
          XCL=Q(LCACL_IN+14)
          YCL=Q(LCACL_IN+15)
          ZCL=Q(LCACL_IN+16)
          c3='cacl'
          GO TO 24
        END IF
        CALL ERRMSG(' illegal argument  ',' TRISRC_TRACK',' ','W')
        GO TO 999
   24   CONTINUE
      DO 82 I = 1,NVERT ! loop on vertices
        LVERT = GZVERT(I)
        VIN(1)=Q(LVERT+3)
        VIN(2)=Q(LVERT+4)
        VIN(3)=Q(LVERT+5)
        ICDI=0
        IVTI=0
        IFDI=0
        IKK1      =  0
        IKK2      =  0
        NGOOD     =  0
        VECT=SQRT((XCL-VIN(1))**2+(YCL-VIN(2))*2+(ZCL-VIN(3))**2)
C  compute director cosines
        VIN(4)=(XCL-VIN(1))/VECT
        VIN(5)=(YCL-VIN(2))/VECT
        VIN(6)=(ZCL-VIN(3))/VECT
C  Check if track crosses the TRD
C ****  Look at the entrance point
        CALL EXTCYL ( VIN, VOUT, RMIN, IGTRAC )
        IF ( IGTRAC .NE. 0 .OR. ABS(VOUT(3) ) .GT. ZMIN )THEN
          IF(DOPRINT)WRITE(LOUT,*)' part does not cross TRD for vertex 
     &      nb',i
          GO TO 82
        END IF
        OFFS=0
C ****  This is a good track : stores its characteristics
        if(doprint)write(lout,*)' track type ',c3,' accepted in TRD'
C
        IDENT  = IDI
        CALL TRDFIL(VIN,PHI,IDENT)
        go to 100
   40   CONTINUE
   80   CONTINUE
        OFFS=1
   82 CONTINUE
  100 continue
  999 CONTINUE
      RETURN
      END
