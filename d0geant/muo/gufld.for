      SUBROUTINE GUFLD(VECT,F)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : reads the map field from MMAP for GEANT use 
C-   GEANT V3.14 Warning : VECT should not be altered in this routine.
C-
C-   Inputs  : VECT (3 dimensional vector of positions in global co.
C-   Outputs : F field in KGauss
C-   Controls: 
C-
C-   Created  14-MAR-1991   TF,AKl
C-   Updated  26-APR-1991   A.Klatchko 
C-   Updated  29-SEP-1990   A.Kiryunin  : include SAMUS toroids' field 
C-   Updated  24-JUL-1991   K. Wyatt Merritt  Fix incorrect usage of 
C-                          GEANT routine GMEDIA. 
C-   Updated   5-JAN-1992   A.Klatchko,S.Igarashi fix bug in Mirror values
C-   Updated  16-JAN-1992   Susumu Igarashi  fix bug in Mirror values 
C-   Updated   6-FEB-1992   Susumu Igarashi  consider boundary of media 
C-   Updated  15-FEB-1992   D. Hedin fix bugs in CF -Y usage
C-   Updated  24-JUL-1992   Susumu Igarashi  For version 3 MMAP
C-   Updated  15-APR-1993   Susumu Igarashi  For version 4 MMAP with Rotation
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:GCVOLU.INC'
      INCLUDE 'D0$INC:GCUNIT.INC'
      INCLUDE 'D0$INC:D0LOG.INC'
      REAL VECT(3),        ! INPUT:INITIAL COORDINATES  
     1 F(3),                ! OUTPUT:FIELD COMPONENTS   
     2 X(3),RAD,XCORN,YCORN,BCF,BEF 
      INTEGER NUMED,I,LMMAP,GZMMAP,IER,FIRST  
      INTEGER LMMAH,GZMMAH,VERSION
      INTEGER XINT,YINT,IABX1,IABY1,IABX2,IABY2,IABX3,IABY3,
     &        IABX4,IABY4   
      REAL BX1,BY1,BX2,BY2,BX3,BY3,BX4,BY4
      REAL    X1,X2,Y1,Y2,XX1,YY1,XX,YY,XX2,YY2
      REAL XL(3)
      INTEGER NUMED1,NUMED2,NUMED3,NUMED4
      INTEGER NUMBER0,NUMBER1,NUMBER2,NUMBER3,NUMBER4

      REAL BMAX,BXMAX,BYMAX
      REAL BX(4),BY(4),POINT(2),GRIDX(2),GRIDY(2),FIELD(2),SIX
      REAL EPS,GRID,D1,D2,D3,D4,BXX(2),BYY(2),R(4),BDX,BDY
      REAL EFXDIS,EFYDIS,EFXLOC,EFYLOC,EFREMA,CFXDIS,CFYDIS
      REAL CFXLOC,CFYLOC,CFREMA,SAMDIS,SAMLOC,SAMREMA,SYMMETRY
      INTEGER EFXNUM,EFYNUM,CFXNUM,CFYNUM,EFXCOR,EFYCOR,CFXCOR,CFYCOR
      INTEGER SAMNUM,SAMCOR
      INTEGER J
      REAL MAGTRA(3),MAGROTI(3,3),VECT1(3)
      REAL MAGROT(3,3),F1(3)

C  Samus Toroids
      INTEGER NMAG                      ! Number of the SAMUS magnet
      REAL    VECLOC(3),FIELDS(3)       ! Coordinate of particle and
                                        ! field in the SAMUS magnet (local)
      DATA FIRST/0/
C----------------------------------------------------------------------
C
**********************************************
*   Big Branch to the old version.   (V1)    *
**********************************************
      IF(SMUO(2).LT.1.5) THEN
         CALL GUFLD_V1(VECT,F)
         RETURN
      ENDIF

C-   GET FIELD MAP                                       

CCC  check map version number
      IF(FIRST.EQ.0) THEN
        FIRST=1
        LMMAH=GZMMAH(0)
        VERSION=IC(LMMAH+1)
        WRITE(LOUT,9000) VERSION
 9000   FORMAT(' GUFLD: Magnetic field version number =',I4)
        IF(VERSION.EQ.0.AND.SMUO(3).NE.0.)THEN
          CALL ERRMSG('Inconsistent version in MMAH',
     &         'GUFLD',' ','F')
        ENDIF
      ENDIF
C      
      CALL GSCVOL         !  Save GEANT volume tree before GMEDIA call!!
C
      X(1)=VECT(1)
      X(2)=VECT(2)
      X(3)=VECT(3)
C      
CC   GIVEN A POSITION (X(1),X(2),X(3)) = (X,Y,Z) WHERE FIELD IS TO BE FOUND
C
      CALL GMEDIA(X,NUMED)             !GETS MEDIA NUMBER
      NUMBER0=NUMBER(NLEVEL)
C
      F(1) = 0.0
      F(2) = 0.0
      F(3) = 0.0                
C
      IF (SMUO(3).EQ.0) THEN       !no field map old DH map
        IF(NUMED.EQ.29) THEN           ! CENTRAL SLABS    
          IF(ABS(X(1)).GT.XCORN) F(2)=BCF*ABS(X(1))/X(1)  
          IF(ABS(X(2)).GT.YCORN) F(1)=-BCF*ABS(X(2))/X(2) 
        ENDIF 
        IF(NUMED.EQ.28) THEN           ! CENTRAL CORNER   
          RAD=SQRT((ABS(X(1))-XCORN)**2+(ABS(X(2))-XCORN)**2) 
          F(1)=-(ABS(X(2))-YCORN)/RAD*ABS(X(2))/X(2)*BCF  
          F(2)= (ABS(X(1))-XCORN)/RAD*ABS(X(1))/X(1)*BCF  
        ENDIF 
        IF(NUMED.EQ.27) THEN           ! END TOROID  
          RAD=SQRT(X(1)**2+X(2)**2)   
          F(1)=-BEF*X(2)/RAD  
          F(2)= BEF*X(1)/RAD  
        ENDIF
        IF(NUMED.EQ.26) THEN
          F(1)=15.0
        ENDIF 
        IF (NUMED.EQ.109) THEN         ! SAMUS TOROID
          CALL SAMAG1 (VECT,VECLOC,NMAG)
          CALL SAMFLD (NMAG,VECLOC,FIELDS)
          CALL SAMAG2 (NMAG,FIELDS,F)
        ENDIF

      ELSE
        IF(VERSION.EQ.2)THEN  
C MAP Version 2
C  END TOROID and SAMUS TOROID
        IF (NUMED .EQ. 27. .OR. NUMED.EQ.26 .OR. NUMED.EQ.109) THEN
        X(1)=ABS(VECT(1))
        X(2)=ABS(VECT(2))
        LMMAP=GZMMAP(2)
C
          XX = X(1)/20.0
          XINT = INT(XX)
          YY = X(2)/20.0
          YINT = INT(YY)
C
          XX1 = XINT*20.0
          YY1 = YINT*20.0
          XX2 = XX1 + 20.0
          YY2 = YY1 + 20.0
C
CCC   NOW THE 4 NEW VALUES OF X,Y FOR WHICH THE BX,BY IS KNOWN
CCC   ARE (XX1,YY1),(XX2,YY1),(XX1,YY2),(XX2,YY2)
C      
          IABX1 = 2*(29*YINT) + XINT*2 +1
          IABY1 = 2*(29*YINT) + XINT*2 +2
          IABX2 = 2*(29*YINT) + XINT*2 +3
          IABY2 = 2*(29*YINT) + XINT*2 +4
          IABX3 = 2*(29*(YINT+1)) + XINT*2 +1
          IABY3 = 2*(29*(YINT+1)) + XINT*2 +2
          IABX4 = 2*(29*(YINT+1)) + XINT*2 +3
          IABY4 = 2*(29*(YINT+1)) + XINT*2 +4
C
CCC  OBTAIN THE BX,BY VALUES FROM THE ZEBRA BANK WITH THE PREVIOUS
CC   ADDRESSES
C
          BX1 = C(LMMAP+20+IABX1)
          BY1 = C(LMMAP+20+IABY1)
          BX2 = C(LMMAP+20+IABX2)
          BY2 = C(LMMAP+20+IABY2)
          BX3 = C(LMMAP+20+IABX3)
          BY3 = C(LMMAP+20+IABY3)
          BX4 = C(LMMAP+20+IABX4)
          BY4 = C(LMMAP+20+IABY4)
C
C   CONSIDER THE BOUNDARY OF MEDIA
C
          IF((BX1**2+BY1**2).LT.0.01.OR.(BX2**2+BY2**2).LT.0.01
     &   .OR.(BX3**2+BY3**2).LT.0.01.OR.(BX4**2+BY4**2).LT.0.01)THEN
            XL(1)=XX1
            XL(2)=YY1
            XL(3)=X(3)
            CALL GMEDIA(XL,NUMED1)
            XL(1)=XX2
            XL(2)=YY1
            CALL GMEDIA(XL,NUMED2)
            XL(1)=XX1
            XL(2)=YY2
            CALL GMEDIA(XL,NUMED3)
            XL(1)=XX2
            XL(2)=YY2
            CALL GMEDIA(XL,NUMED4)
            IF(NUMED1.NE.NUMED)THEN
              IF(NUMED2.EQ.NUMED)THEN
                BX1=BX2
                BY1=BY2
              ELSEIF(NUMED3.EQ.NUMED)THEN
                BX1=BX3
                BY1=BY3
              ELSEIF(NUMED4.EQ.NUMED)THEN
                BX1=BX4
                BY1=BY4
              ENDIF
            ENDIF
            IF(NUMED2.NE.NUMED)THEN
              IF(NUMED1.EQ.NUMED)THEN
                BX2=BX1
                BY2=BY1
              ELSEIF(NUMED3.EQ.NUMED)THEN
                BX2=BX3
                BY2=BY3
              ELSEIF(NUMED4.EQ.NUMED)THEN
                BX2=BX4
                BY2=BY4
              ENDIF
            ENDIF
            IF(NUMED3.NE.NUMED)THEN
              IF(NUMED1.EQ.NUMED)THEN
                BX3=BX1
                BY3=BY1
              ELSEIF(NUMED2.EQ.NUMED)THEN
                BX3=BX2
                BY3=BY2
              ELSEIF(NUMED4.EQ.NUMED)THEN
                BX3=BX4
                BY3=BY4
              ENDIF
            ENDIF
            IF(NUMED4.NE.NUMED)THEN
              IF(NUMED1.EQ.NUMED)THEN
                BX4=BX1
                BY4=BY1
              ELSEIF(NUMED2.EQ.NUMED)THEN
                BX4=BX2
                BY4=BY2
              ELSEIF(NUMED3.EQ.NUMED)THEN
                BX4=BX3
                BY4=BY3
              ENDIF
            ENDIF
          ENDIF
C 
CCC CALCULATE THE FIELD AT GIVEN POINT BY INTERPOLATING USING THE
CC FOUR NEAREST VALUES  (SEPARATE STEPS FOR BX,BY)
C  
          X1 = X(1) - XX1
          X2 = XX2 - X(1)
          Y1 = X(2) - YY1
          Y2 = YY2 - X(2)
C C
          F(1) = 10.*(Y2*(X2*BX1 + X1*BX2) + Y1*(BX3*X2 + BX4*X1)) /
     *  ((X1+X2)*(Y1+Y2))
C
          F(2) = 10.*(Y2*(X2*BY1 + X1*BY2) + Y1*(BY3*X2 + BY4*X1)) /
     *  ((X1+X2)*(Y1+Y2))
CC     Upper left  quadrant:X<0,Y>=0
          IF (VECT(1) .LT. 0.0 .AND. VECT(2) .GE. 0.0)F(2) = -F(2)   
C     Lower left quadrant:X<0,Y<0
          IF (VECT(1) .LT. 0.0 .AND. VECT(2) .LT. 0.0)THEN   
              F(1) = -F(1)
              F(2) = -F(2)
            ENDIF
C     Lower right quadrant:X>=0,Y<0
          IF (VECT(1) .GE. 0.0 .AND. VECT(2) .LT. 0.0)F(1) = -F(1) 
C
        ENDIF
        IF((NUMED .EQ. 29) .OR. (NUMED .EQ. 28)) THEN       !CF
        X(1)=ABS(VECT(1))
C
         LMMAP=GZMMAP(1)
CC   GIVEN A POSITION (X(1),X(2),X(3)) = (X,Y,Z) WHERE FIELD IS TO BE FOUND
C
          XX = X(1)/20.0
          XINT = INT(XX)
          YY = X(2)/20.0
          YINT = INT(YY)
          IF(X(2).LT.0.) YINT=YINT-1.       ! HAVE NEAREST LOWER VALUE
C
          XX1 = XINT*20.0
          YY1 = YINT*20.0
          XX2 = XX1 + 20.0
          YY2 = YY1 + 20.0
C
CCC   NOW THE 4 NEW VALUES OF X,Y FOR WHICH THE BX,BY IS KNOWN
CCC   ARE (XX1,YY1),(XX2,YY1),(XX1,YY2),(XX2,YY2)
C      
            IABX1 = 1058 + 2*(23*YINT) + XINT*2 +1
            IABY1 = 1058 + 2*(23*YINT) + XINT*2 +2
            IABX2 = 1058 + 2*(23*YINT) + XINT*2 +3
            IABY2 = 1058 + 2*(23*YINT) + XINT*2 +4
            IABX3 = 1058 + 2*(23*(YINT+1)) + XINT*2 +1
            IABY3 = 1058 + 2*(23*(YINT+1)) + XINT*2 +2
            IABX4 = 1058 + 2*(23*(YINT+1)) + XINT*2 +3
            IABY4 = 1058 + 2*(23*(YINT+1)) + XINT*2 +4
C
CCC  OBTAIN THE BX,BY VALUES FROM THE ZEBRA BANK WITH THE PREVIOUS
CC   ADDRESSES
C
            BX1 = C(LMMAP+20+IABX1)
            BY1 = C(LMMAP+20+IABY1)
            BX2 = C(LMMAP+20+IABX2)
            BY2 = C(LMMAP+20+IABY2)
            BX3 = C(LMMAP+20+IABX3)
            BY3 = C(LMMAP+20+IABY3)
            BX4 = C(LMMAP+20+IABX4)
            BY4 = C(LMMAP+20+IABY4)
C
C   CONSIDER THE BOUNDARY OF MEDIA
C
          IF((BX1**2+BY1**2).LT.0.01.OR.(BX2**2+BY2**2).LT.0.01
     &   .OR.(BX3**2+BY3**2).LT.0.01.OR.(BX4**2+BY4**2).LT.0.01
     &   .OR. ABS(YY1).EQ.300. .OR. ABS(YY2).EQ.300.)THEN
            XL(1)=XX1
            XL(2)=YY1
            XL(3)=X(3)
            CALL GMEDIA(XL,NUMED1)
            NUMBER1=NUMBER(NLEVEL)
            XL(1)=XX2
            XL(2)=YY1
            CALL GMEDIA(XL,NUMED2)
            NUMBER2=NUMBER(NLEVEL)
            XL(1)=XX1
            XL(2)=YY2
            CALL GMEDIA(XL,NUMED3)
            NUMBER3=NUMBER(NLEVEL)
            XL(1)=XX2
            XL(2)=YY2
            CALL GMEDIA(XL,NUMED4)
            NUMBER4=NUMBER(NLEVEL)
            IF(NUMED1.NE.NUMED.OR.NUMBER1.NE.NUMBER0)THEN
              IF(NUMED2.EQ.NUMED.AND.NUMBER2.EQ.NUMBER0)THEN
                BX1=BX2
                BY1=BY2
              ELSEIF(NUMED3.EQ.NUMED.AND.NUMBER3.EQ.NUMBER0)THEN
                BX1=BX3
                BY1=BY3
              ELSEIF(NUMED4.EQ.NUMED.AND.NUMBER4.EQ.NUMBER0)THEN
                BX1=BX4
                BY1=BY4
              ENDIF
            ENDIF
            IF(NUMED2.NE.NUMED.OR.NUMBER2.NE.NUMBER0)THEN
              IF(NUMED1.EQ.NUMED.AND.NUMBER1.EQ.NUMBER0)THEN
                BX2=BX1
                BY2=BY1
              ELSEIF(NUMED3.EQ.NUMED.AND.NUMBER3.EQ.NUMBER0)THEN
                BX2=BX3
                BY2=BY3
              ELSEIF(NUMED4.EQ.NUMED.AND.NUMBER4.EQ.NUMBER0)THEN
                BX2=BX4
                BY2=BY4
              ENDIF
            ENDIF
            IF(NUMED3.NE.NUMED.OR.NUMBER3.NE.NUMBER0)THEN
              IF(NUMED1.EQ.NUMED.AND.NUMBER1.EQ.NUMBER0)THEN
                BX3=BX1
                BY3=BY1
              ELSEIF(NUMED2.EQ.NUMED.AND.NUMBER2.EQ.NUMBER0)THEN
                BX3=BX2
                BY3=BY2
              ELSEIF(NUMED4.EQ.NUMED.AND.NUMBER4.EQ.NUMBER0)THEN
                BX3=BX4
                BY3=BY4
              ENDIF
            ENDIF
            IF(NUMED4.NE.NUMED.OR.NUMBER4.NE.NUMBER0)THEN
              IF(NUMED1.EQ.NUMED.AND.NUMBER1.EQ.NUMBER0)THEN
                BX4=BX1
                BY4=BY1
              ELSEIF(NUMED2.EQ.NUMED.AND.NUMBER2.EQ.NUMBER0)THEN
                BX4=BX2
                BY4=BY2
              ELSEIF(NUMED3.EQ.NUMED.AND.NUMBER3.EQ.NUMBER0)THEN
                BX4=BX3
                BY4=BY3
              ENDIF
            ENDIF
          ENDIF
C        
CCC CALCULATE THE FIELD AT GIVEN POINT BY INTERPOLATING USING THE
CC FOUR NEAREST VALUES  (SEPARATE STEPS FOR BX,BY)
C  
            X1 = X(1) - XX1
            X2 = XX2 - X(1)
            Y1 = X(2) - YY1
            Y2 = YY2 - X(2)
C
            F(1) = 10.*(Y2*(X2*BX1 + X1*BX2) + Y1*(BX3*X2 + BX4*X1)) /
     *  ((X1+X2)*(Y1+Y2))
C
            F(2) = 10.*(Y2*(X2*BY1 + X1*BY2) + Y1*(BY3*X2 + BY4*X1)) /
     *  ((X1+X2)*(Y1+Y2))
C Mirror values     
            IF(VECT(1) .LT. 0.0)THEN
             F(2) = -F(2)     
            ENDIF
C       
        ENDIF

C MAP Version 3
        ELSEIF(VERSION.EQ.3 .OR. VERSION.EQ.4)THEN

C     This part of the program uses the STP file called 
C     MUO_STPFILE_2_N.DAT.  The fiel is sorted in X, then Y.
C     The parameters of the field map (starting point, grid
C     spacing, etc. is not hard wired and is read in from the
C     STP file.

C     VERSION 4 has small rotation for both North and South EF magnets.
C     MUO_STPFILE_3.DAT has MMAH that stores rotation matrix and
C     translation. MMAG is updated for each position and rotation.

        IF (NUMED .EQ. 27) THEN                  !END TOROID

          LMMAP=GZMMAP(2)

          efxnum=ic(lmmap+11)   !Number of points in the X direction
          efynum=ic(lmmap+12)   !Number of points in the Y direction
          efxloc=c(lmmap+13)    !X starting point
          efyloc=c(lmmap+14)    !Y starting point
          efxdis=c(lmmap+15)    !Grid spacing in the X direction
          efydis=c(lmmap+16)    !Grid spacing in the Y direction

C   --  Check if the map is symmetric about (0,0)
          symmetry=1.0-abs(efxloc)/abs(efxloc+((efxnum-1)*efxdis))

C   --  EFXCOR,EFYCOR are by how many points the map is offset.
C       This is needed to get the first point of the graph
C       to be mapped to a location of 1

          if (symmetry .lt. 0.001) then
             efxcor=(efxnum/2)-1
          else
             efxcor=-nint(efxloc/efxdis)
          endif
          symmetry=1.0-abs(efyloc)/abs(efyloc+((efynum-1)*efydis))
          if (symmetry .lt. 0.001) then
             efycor=(efynum/2)-1
          else
             efycor=-nint(efyloc/efydis)
          endif

C   -- EFREMA is the amount the map is offset from the spacing
C      if size of map is from -4.95 to 4.95 by 0.1, the EFREMA is 0.05
C      if size of map is from -4.95 to 4.95 by 0.05, the EFREMA is 0

          efrema=100*efxdis*abs((efxloc/efxdis)-int(efxloc/efxdis))
          efxdis=efxdis*100   !Convert X grid spacing to CM
          efydis=efydis*100   !Convert Y grid spacing to CM

C translate VECT into the local coordinates of magnets for version 4
          IF(VERSION.EQ.4)THEN
            IF(VECT(3).LT.0.)THEN                 ! EFN
              DO 213 I=1,3
  213           MAGTRA(I)=-C(LMMAH+10+I)
              DO 214 I=1,3
                DO 214 J=1,3
  214             MAGROTI(J,I)=C(LMMAH+10+I+3*J)
            ELSEIF(VECT(3).GT.0.)THEN                ! EFS
              DO 215 I=1,3
  215           MAGTRA(I)=-C(LMMAH+22+I)
              DO 216 I=1,3
                DO 216 J=1,3
  216             MAGROTI(J,I)=C(LMMAH+22+I+3*J)
            ENDIF
            CALL VADD( X, MAGTRA, VECT1, 3 )
            CALL VMATR( VECT1, MAGROTI, X, 3, 3 )
          ENDIF

C     FIND THE ARAY INDECES OF THE GRID POSITIONS IN THE ZEBRA MAP
          XX = X(1)/efxdis
          XINT = NINT(XX)+efxcor
          YY = X(2)/efydis
          YINT = NINT(YY)+efycor

C     CALCULATE THE GRID POINTS
          XX1 = (XINT-efxcor)*efxdis
          YY1 = (YINT-efycor)*efydis
          XX2 = XX1 + efxdis
          YY2 = YY1 + efydis
C
CCC   NOW THE 4 NEW VALUES OF X,Y FOR WHICH THE BX,BY IS KNOWN
CCC   ARE (XX1,YY1),(XX2,YY1),(XX1,YY2),(XX2,YY2)
C
          IABX1 = 2*(efynum*XINT) + YINT*2 +1
          IABY1 = 2*(efynum*XINT) + YINT*2 +2
          IABX2 = 2*(efynum*XINT) + YINT*2 +3
          IABY2 = 2*(efynum*XINT) + YINT*2 +4
          IABX3 = 2*(efynum*(XINT+1)) + YINT*2 +1
          IABY3 = 2*(efynum*(XINT+1)) + YINT*2 +2
          IABX4 = 2*(efynum*(XINT+1)) + YINT*2 +3
          IABY4 = 2*(efynum*(XINT+1)) + YINT*2 +4
C
CCC  OBTAIN THE BX,BY VALUES FROM THE ZEBRA BANK WITH THE PREVIOUS
CC   ADDRESSES
C
          BX1 = C(LMMAP+20+IABX1)
          BY1 = C(LMMAP+20+IABY1)
          BX2 = C(LMMAP+20+IABX2)
          BY2 = C(LMMAP+20+IABY2)
          BX3 = C(LMMAP+20+IABX3)
          BY3 = C(LMMAP+20+IABY3)
          BX4 = C(LMMAP+20+IABX4)
          BY4 = C(LMMAP+20+IABY4)
CCC   DOUBLE CHECK IF NEAR THE EDGE
C
C   CONSIDER THE BOUNDARY OF MEDIA
C
          IF((BX1**2+BY1**2).LT.0.01.OR.(BX2**2+BY2**2).LT.0.01
     &      .OR.(BX3**2+BY3**2).LT.0.01.OR.(BX4**2+BY4**2).LT.0.01)THEN
CCC    FIND LARGEST
            BMAX=BX1**2+BY1**2
            BXMAX=BX1
            BYMAX=BY1
            IF(BX2**2+BY2**2.GT.BMAX) THEN
              BMAX=BX2**2+BY2**2
              BXMAX=BX2
              BYMAX=BY2
            ENDIF
            IF(BX3**2+BY3**2.GT.BMAX) THEN
              BMAX=BX3**2+BY3**2
              BXMAX=BX3
              BYMAX=BY3
            ENDIF
            IF(BX4**2+BY4**2.GT.BMAX) THEN
              BMAX=BX4**2+BY4**2
              BXMAX=BX4
              BYMAX=BY4
            ENDIF
            IF(BX1**2+BY1**2.LT..01) THEN
              BX1=BXMAX
              BY1=BYMAX
            ENDIF
            IF(BX2**2+BY2**2.LT..01) THEN
              BX2=BXMAX
              BY2=BYMAX
            ENDIF
            IF(BX3**2+BY3**2.LT..01) THEN
              BX3=BXMAX
              BY3=BYMAX
            ENDIF
            IF(BX4**2+BY4**2.LT..01) THEN
              BX4=BXMAX
              BY4=BYMAX
            ENDIF
          ENDIF

CCCCCCC
C         MAKE A DJUSTMENTS FOR INSIDE BOUNDARY OF EF WHERE SAMUS GOES
C

          IF ((BX1 .GE. 999.9) .OR. (BX2 .GE. 999.9) .OR.
     &        (BX3 .GE. 999.9) .OR. (BX4 .GE. 999.9)) THEN

            IF (BX4 .GE. 999.9) THEN
              IF (BX3 .GE. 999.9) THEN
                BX3=BX1
                BX4=BX2
                BY3=BY1
                BY4=BY2
              END IF
              IF (BX2 .GE. 999.9) THEN
                BX2=BX1
                BX4=BX3
                BY2=BY1
                BY4=BY3
              END IF
            ENDIF

            IF (BX1 .GE. 999.9) THEN
              IF (BX2 .GE. 999.9) THEN
                BX1=BX3
                BX2=BX4
                BY1=BY3
                BY2=BY4
              END IF
              IF (BX3 .GE. 999.9) THEN
                BX1=BX2
                BX3=BX4
                BY1=BY2
                BY3=BY4
              END IF
            END IF


            IF ((BX1 .GE. 999.9) .AND. (BX2 .LT. 999.9) .AND.
     +        (BX3 .LT. 999.9)) THEN
              BX1=0.0
              BY1=0.0
            END IF

            IF ((BX2 .GE. 999.9) .AND. (BX1 .LT. 999.9) .AND.
     +        (BX4 .LT. 999.9)) THEN
              BX2=0.0
              BY2=0.0
            END IF

            IF ((BX3 .GE. 999.9) .AND. (BX1 .LT. 999.9) .AND.
     +        (BX4 .LT. 999.9)) THEN
              BX3=0.0
              BY3=0.0
            END IF

            IF ((BX4 .GE. 999.9) .AND. (BX2 .LT. 999.9) .AND.
     +        (BX3 .LT. 999.9)) THEN
              BX4=0.0
              BY4=0.0
            END IF

          END IF

CCC CALCULATE THE FIELD AT GIVEN POINT BY INTERPOLATING USING THE
CC FOUR NEAREST VALUES  (SEPARATE STEPS FOR BX,BY)

C C   GET VARIABLES READY FOR PASSING INTO THE INTERPOLATING ALGORITHM
          POINT(1)=X(1)
          POINT(2)=X(2)
          GRIDX(1)=XX1
          GRIDX(2)=XX2
          GRIDY(1)=YY1
          GRIDY(2)=YY2
          BX(1)=BX1
          BX(2)=BX2
          BX(3)=BX3
          BX(4)=BX4
          BY(1)=BY1
          BY(2)=BY2
          BY(3)=BY3
          BY(4)=BY4


C     GET FIELD VALUE AT DESIRED POINT BY INTERPOLATING

          GRID=efrema

          GRIDX(1) = GRIDX(1)-GRID
          GRIDX(2) = GRIDX(2)-GRID
          GRIDY(1) = GRIDY(1)-GRID
          GRIDY(2) = GRIDY(2)-GRID
          X1 = ABS(POINT(1) - GRIDX(1))
          X2 = ABS(GRIDX(2) - POINT(1))
          Y1 = ABS(POINT(2) - GRIDY(1))
          Y2 = ABS(GRIDY(2) - POINT(2))
C    CHECK IF ON A GIVEN CORNER
          IF(Y1 .LT. EPS .AND. X1 .LT. EPS)THEN
            FIELD(1) = 10.*BX(1)
            FIELD(2) = 10.*BY(1)
            GOTO 999
          ELSEIF(Y2 .LT. EPS .AND. X2 .LT. EPS)THEN
            FIELD(1) = 10.*BX(4)
            FIELD(2) = 10.*BY(4)
            GOTO 999
          ELSEIF(Y1 .LT. EPS .AND. X2 .LT. EPS)THEN
            FIELD(1) = 10.*BX(3)
            FIELD(2) = 10.*BY(3)
            GOTO 999
          ELSEIF(Y2 .LT. EPS .AND. X1 .LT. EPS)THEN
            FIELD(1) = 10.*BX(2)
            FIELD(2) = 10.*BY(2)
            GOTO 999
          ENDIF
          BDX = .5*(BX(3) - BX(1))/GRID !FIRST DERIVATIVE IN X
          BX1 = BX(1) + X1*BDX
          BDX = .5*(BX(4) - BX(2))/GRID !FIRST DERIVATIVE IN X
          BX2 = BX(2) + X1*BDX
          BDY = 0.5*(BY(2) - BY(1))/GRID !FIRST DERIVATIVE IN Y
          BY1 = BY(1) + Y1*BDY
          BDY = 0.5*(BY(4) - BY(3))/GRID
          BY2 = BY(3) + Y1*BDY
          BDY = .5*(BY2 - BY1)/GRID
          BDX = .5*(BX2 - BX1)/GRID
          FIELD(1) = 10.*(BX1 + BDX*Y1)
          FIELD(2) = 10.*(BY1 + BDY*X1)
  999     continue

          IF(VERSION.EQ.3)THEN
            F(1)=FIELD(1)
            F(2)=FIELD(2)
          ELSEIF(VERSION.EQ.4)THEN
C North
            IF(VECT(3).LT.0.)THEN
              DO 201 I=1,3
                DO 201 J=1,3
  201             MAGROT(I,J)=C(LMMAH+10+I+3*J)    ! Rotation matrix
C South
            ELSE
              DO 202 I=1,3
                DO 202 J=1,3
  202             MAGROT(I,J)=C(LMMAH+22+I+3*J)
            ENDIF
            F1(1)=FIELD(1)
            F1(2)=FIELD(2)
            F1(3)=0.
            CALL VMATR( F1, MAGROT, F, 3, 3 )
          ENDIF


        ELSE IF(NUMED .EQ. 29 .OR. NUMED .EQ. 28) THEN       !CF

C    -- CF variables have the same meaning as ef variables

          LMMAP=GZMMAP(1)

          cfxnum=ic(lmmap+11)
          cfynum=ic(lmmap+12)
          cfxloc=c(lmmap+13)
          cfyloc=c(lmmap+14)
          cfxdis=c(lmmap+15)
          cfydis=c(lmmap+16)
          symmetry=1.0-abs(cfxloc)/abs(cfxloc+((cfxnum-1)*cfxdis))
          if (symmetry .lt. 0.001) then
             cfxcor=(cfxnum/2)-1
          else
             cfxcor=-nint(cfxloc/cfxdis)
          endif
          symmetry=1.0-abs(cfyloc)/abs(cfyloc+((cfynum-1)*cfydis))
          if (symmetry .lt. 0.001) then
             cfycor=(cfynum/2)-1
          else
             cfycor=-nint(cfyloc/cfydis)
          endif
          cfrema=100*cfxdis*abs((cfxloc/cfxdis)-int(cfxloc/cfxdis))
          cfxdis=cfxdis*100
          cfydis=cfydis*100

C translate VECT into the local coordinates of magnets for version 4
          IF(VERSION.EQ.4)THEN
            DO 211 I=1,3
  211         MAGTRA(I)=-C(LMMAH+34+I)
            DO 212 I=1,3
              DO 212 J=1,3
  212           MAGROTI(J,I)=C(LMMAH+34+I+3*J)
            CALL VADD( X, MAGTRA, VECT1, 3 )
            CALL VMATR( VECT1, MAGROTI, X, 3, 3 )
          ENDIF

CC   GIVEN A POSITION (X(1),X(2),X(3)) = (X,Y,Z) WHERE FIELD IS TO BE FOUND
C
C     SEE IF SYMMETRY REFLECTION WILL BE NEEDED
          IF (X(1) .NE. 0.0) THEN
            SIX=X(1)/ABS(X(1))
          END IF

          X(1)=ABS(X(1))

C     CALCULATE INDECES OF  MAP ARRAY IN ZEBRA FILE
          XX = X(1)/cfxdis
          XINT = NINT(XX)+cfxcor
          YY = X(2)/cfydis
          YINT = NINT(YY)+cfycor

C     DETERMINE FOUR NEAREST NEIGHBORS AROUND POINT TO BE CALCULATED
          XX1 = (XINT-cfxcor)*cfxdis
          YY1 = (YINT-cfycor)*cfydis
          XX2 = XX1 + cfxdis
          YY2 = YY1 + cfydis
C
CCC   NOW THE 4 NEW VALUES OF X,Y FOR WHICH THE BX,BY ARE KNOWN
CCC   ARE (XX1,YY1),(XX2,YY1),(XX1,YY2),(XX2,YY2)
C
          IABX1 = 2*(cfynum*XINT) + YINT*2 +1
          IABY1 = 2*(cfynum*XINT) + YINT*2 +2
          IABX2 = 2*(cfynum*XINT) + YINT*2 +3
          IABY2 = 2*(cfynum*XINT) + YINT*2 +4
          IABX3 = 2*(cfynum*(XINT+1)) + YINT*2 +1
          IABY3 = 2*(cfynum*(XINT+1)) + YINT*2 +2
          IABX4 = 2*(cfynum*(XINT+1)) + YINT*2 +3
          IABY4 = 2*(cfynum*(XINT+1)) + YINT*2 +4

CC        ADJUST FOR LEFT BOUNDARY OF GRID
          IF (XINT .LT. 0.0) THEN
            IABX1=IABX3
            IABY1=IABY3
            IABX2=IABX4
            IABY2=IABY4
          END IF

C
CCC  OBTAIN THE BX,BY VALUES FROM THE ZEBRA BANK WITH THE PREVIOUS
CC   ADDRESSES
C
          BX1 = C(LMMAP+20+IABX1)
          BY1 = C(LMMAP+20+IABY1)
          BX2 = C(LMMAP+20+IABX2)
          BY2 = C(LMMAP+20+IABY2)
          BX3 = C(LMMAP+20+IABX3)
          BY3 = C(LMMAP+20+IABY3)
          BX4 = C(LMMAP+20+IABX4)
          BY4 = C(LMMAP+20+IABY4)

C     ADJUST FOR MOVEMENT BEYOND GRID
          IF((BX1**2+BY1**2).LT.0.01.OR.(BX2**2+BY2**2).LT.0.01
     &      .OR.(BX3**2+BY3**2).LT.0.01.OR.(BX4**2+BY4**2).LT.0.01)THEN
CCC    FIND LARGEST
            BMAX=BX1**2+BY1**2
            BXMAX=BX1
            BYMAX=BY1
            IF(BX2**2+BY2**2.GT.BMAX) THEN
              BMAX=BX2**2+BY2**2
              BXMAX=BX2
              BYMAX=BY2
            ENDIF
            IF(BX3**2+BY3**2.GT.BMAX) THEN
              BMAX=BX3**2+BY3**2
              BXMAX=BX3
              BYMAX=BY3
            ENDIF
            IF(BX4**2+BY4**2.GT.BMAX) THEN
              BMAX=BX4**2+BY4**2
              BXMAX=BX4
              BYMAX=BY4
            ENDIF
            IF(BX1**2+BY1**2.LT..01) THEN
              BX1=BXMAX
              BY1=BYMAX
            ENDIF
            IF(BX2**2+BY2**2.LT..01) THEN
              BX2=BXMAX
              BY2=BYMAX
            ENDIF
            IF(BX3**2+BY3**2.LT..01) THEN
              BX3=BXMAX
              BY3=BYMAX
            ENDIF
            IF(BX4**2+BY4**2.LT..01) THEN
              BX4=BXMAX
              BY4=BYMAX
            ENDIF
          ENDIF
C
CCC CALCULATE THE FIELD AT GIVEN POINT BY INTERPOLATING USING THE
CC FOUR NEAREST VALUES  (SEPARATE STEPS FOR BX,BY)



CC C   GET VARIABLES READY FOR PASSING INTO THE INTERPOLATING ALGORITHM

          POINT(1)=X(1)
          POINT(2)=X(2)
          GRIDX(1)=XX1
          GRIDX(2)=XX2
          GRIDY(1)=YY1
          GRIDY(2)=YY2
          BX(1)=BX1
          BX(2)=BX2
          BX(3)=BX3
          BX(4)=BX4
          BY(1)=BY1
          BY(2)=BY2
          BY(3)=BY3
          BY(4)=BY4


C     GET FIELD VALUE AT DESIRED POINT BY INTERPOLATING

          GRID=cfrema

          GRIDX(1) = GRIDX(1)-GRID
          GRIDX(2) = GRIDX(2)-GRID
          GRIDY(1) = GRIDY(1)-GRID
          GRIDY(2) = GRIDY(2)-GRID
          X1 = ABS(POINT(1) - GRIDX(1))
          X2 = ABS(GRIDX(2) - POINT(1))
          Y1 = ABS(POINT(2) - GRIDY(1))
          Y2 = ABS(GRIDY(2) - POINT(2))
C    CHECK IF ON A GIVEN CORNER
          IF(Y1 .LT. EPS .AND. X1 .LT. EPS)THEN
            FIELD(1) = 10.*BX(1)
            FIELD(2) = 10.*BY(1)
            GOTO 998
          ELSEIF(Y2 .LT. EPS .AND. X2 .LT. EPS)THEN
            FIELD(1) = 10.*BX(4)
            FIELD(2) = 10.*BY(4)
            GOTO 998
          ELSEIF(Y1 .LT. EPS .AND. X2 .LT. EPS)THEN
            FIELD(1) = 10.*BX(3)
            FIELD(2) = 10.*BY(3)
            GOTO 998
          ELSEIF(Y2 .LT. EPS .AND. X1 .LT. EPS)THEN
            FIELD(1) = 10.*BX(2)
            FIELD(2) = 10.*BY(2)
            GOTO 998
          ENDIF
          BDX = .5*(BX(3) - BX(1))/GRID !FIRST DERIVATIVE IN X
          BX1 = BX(1) + X1*BDX
          BDX = .5*(BX(4) - BX(2))/GRID !FIRST DERIVATIVE IN X
          BX2 = BX(2) + X1*BDX
          BDY = 0.5*(BY(2) - BY(1))/GRID !FIRST DERIVATIVE IN Y
          BY1 = BY(1) + Y1*BDY
          BDY = 0.5*(BY(4) - BY(3))/GRID
          BY2 = BY(3) + Y1*BDY
          BDY = .5*(BY2 - BY1)/GRID
          BDX = .5*(BX2 - BX1)/GRID
          FIELD(1) = 10.*(BX1 + BDX*Y1)
          FIELD(2) = 10.*(BY1 + BDY*X1)
  998     continue

C     SYMMETRY REFLECTION FOR OTHER HALF OF CF
          IF(SIX .LT. 0.0)THEN
            FIELD(2) = -FIELD(2)
          ENDIF

          IF(VERSION.EQ.3)THEN
            F(1)=FIELD(1)
            F(2)=FIELD(2)
          ELSEIF(VERSION.EQ.4)THEN
            DO 203 I=1,3
              DO 203 J=1,3
  203           MAGROT(I,J)=C(LMMAH+34+I+3*J)    ! Rotation matrix
            F1(1)=FIELD(1)
            F1(2)=FIELD(2)
            F1(3)=0.
            CALL VMATR( F1, MAGROT, F, 3, 3 )
          ENDIF


        ELSE IF(NUMED .EQ. 109) THEN                       !SAMUS

C  -- SAMUS variables have the same meanings as the EF amd CF
C  -- variable, only it is assumed the the X and Y parameters of
C  -- the SAMUS field map are the same.

          LMMAP=GZMMAP(2)

          efxnum=ic(lmmap+11)   !Number of points in the X direction
          efynum=ic(lmmap+12)   !Number of points in the Y direction

          samloc=c(lmmap+18)
          samnum=ic(lmmap+19)
          samdis=c(lmmap+20)
          symmetry=1.0-abs(samloc)/abs(samloc+((samnum-1)*samdis))
          if (symmetry .lt. 0.001) then
             samcor=(samnum/2)-1
          else
             samcor=-nint(samloc/samdis)
          endif
          samrema=100*samdis*abs((samloc/samdis)-int(samloc/samdis))
          samdis=samdis*100

C     FIND THE ARAY INDECES OF THE GRID POSITIONS IN THE ZEBRA MAP
          XX = X(1)/samdis
          XINT = NINT(XX)+samcor
          YY = X(2)/samdis
          YINT = NINT(YY)+samcor

C     CALCULATE THE GRID POINTS
          XX1 = (XINT-samcor)*samdis
          YY1 = (YINT-samcor)*samdis
          XX2 = XX1 + samdis
          YY2 = YY1 + samdis
C
CCC   NOW THE 4 NEW VALUES OF X,Y FOR WHICH THE BX,BY IS KNOWN
CCC   ARE (XX1,YY1),(XX2,YY1),(XX1,YY2),(XX2,YY2)
C
          IABX1 = 2*(samnum*XINT) + YINT*2 +1
          IABY1 = 2*(samnum*XINT) + YINT*2 +2
          IABX2 = 2*(samnum*XINT) + YINT*2 +3
          IABY2 = 2*(samnum*XINT) + YINT*2 +4
          IABX3 = 2*(samnum*(XINT+1)) + YINT*2 +1
          IABY3 = 2*(samnum*(XINT+1)) + YINT*2 +2
          IABX4 = 2*(samnum*(XINT+1)) + YINT*2 +3
          IABY4 = 2*(samnum*(XINT+1)) + YINT*2 +4

C MAKE ADJUSTMENTS FOR THE AREA BEYOND THE GRID AND BETWEEN SAMUS AND EF
          IF ((X(1) .GE. 87.5) .OR. (X(2) .GE. 87.5) .OR. (X(1) .LE.
     &      -87.5) .OR. (X(2) .LE. -87.5)) THEN

            IF ((X(1) .GE. 87.5) .AND. (X(2) .GE. 87.5)) THEN
              IABX2=IABX1
              IABX3=IABX1
              IABX4=IABX1
              IABY2=IABY1
              IABY3=IABY1
              IABY4=IABY1
            ENDIF

            IF ((X(1) .GE. 87.5) .AND. (X(2) .LE. -87.5)) THEN
              IABX1=IABX2
              IABX3=IABX2
              IABX4=IABX2
              IABY1=IABY2
              IABY3=IABY2
              IABY4=IABY2
            ENDIF

            IF ((X(1) .LE. -87.5) .AND. (X(2) .GE. 87.5)) THEN
              IABX2=IABX3
              IABX1=IABX3
              IABX4=IABX3
              IABY2=IABY3
              IABY1=IABY3
              IABY4=IABY3
            ENDIF

            IF ((X(1) .LE. -87.5) .AND. (X(2) .LE. -87.5)) THEN
              IABX1=IABX4
              IABX2=IABX4
              IABX3=IABX4
              IABY1=IABY4
              IABY2=IABY4
              IABY3=IABY4
            ENDIF

            IF (X(1) .GE. 87.5) THEN
              IABX3=IABX1
              IABY3=IABY1
              IABX4=IABX2
              IABY4=IABY2
            END IF
            IF (X(1) .LE. -87.5) THEN
              IABX1=IABX3
              IABY1=IABY3
              IABX2=IABX4
              IABY2=IABY4
            END IF

            IF (X(2) .GE. 87.5) THEN
              IABX2=IABX1
              IABY2=IABY1
              IABX4=IABX3
              IABY4=IABY3
            END IF
            IF (X(2) .LE. -87.5) THEN
              IABX1=IABX2
              IABY1=IABY2
              IABX3=IABX4
              IABY3=IABY4
            END IF
          ENDIF

CCC  OBTAIN THE BX,BY VALUES FROM THE ZEBRA BANK WITH THE PREVIOUS
CC   ADDRESSES

          BX1 = C(LMMAP+(efxnum*efynum*2)+20+IABX1)
          BY1 = C(LMMAP+(efxnum*efynum*2)+20+IABY1)
          BX2 = C(LMMAP+(efxnum*efynum*2)+20+IABX2)
          BY2 = C(LMMAP+(efxnum*efynum*2)+20+IABY2)
          BX3 = C(LMMAP+(efxnum*efynum*2)+20+IABX3)
          BY3 = C(LMMAP+(efxnum*efynum*2)+20+IABY3)
          BX4 = C(LMMAP+(efxnum*efynum*2)+20+IABX4)
          BY4 = C(LMMAP+(efxnum*efynum*2)+20+IABY4)

CCC CALCULATE THE FIELD AT GIVEN POINT BY INTERPOLATING USING THE

CC   GET VARIABLES READY FOR PASSING INTO THE INTERPOLATING ALGORITHM
          POINT(1)=X(1)
          POINT(2)=X(2)
          GRIDX(1)=XX1
          GRIDX(2)=XX2
          GRIDY(1)=YY1
          GRIDY(2)=YY2
          BX(1)=BX1
          BX(2)=BX2
          BX(3)=BX3
          BX(4)=BX4
          BY(1)=BY1
          BY(2)=BY2
          BY(3)=BY3
          BY(4)=BY4

C     GET FIELD VALUE AT DESIRED POINT BY INTERPOLATING

          GRID=samrema

          GRIDX(1) = GRIDX(1)-GRID
          GRIDX(2) = GRIDX(2)-GRID
          GRIDY(1) = GRIDY(1)-GRID
          GRIDY(2) = GRIDY(2)-GRID
          X1 = ABS(POINT(1) - GRIDX(1))
          X2 = ABS(GRIDX(2) - POINT(1))
          Y1 = ABS(POINT(2) - GRIDY(1))
          Y2 = ABS(GRIDY(2) - POINT(2))
C    CHECK IF ON A GIVEN CORNER
          IF(Y1 .LT. EPS .AND. X1 .LT. EPS)THEN
            FIELD(1) = 10.*BX(1)
            FIELD(2) = 10.*BY(1)
            GOTO 997
          ELSEIF(Y2 .LT. EPS .AND. X2 .LT. EPS)THEN
            FIELD(1) = 10.*BX(4)
            FIELD(2) = 10.*BY(4)
            GOTO 997
          ELSEIF(Y1 .LT. EPS .AND. X2 .LT. EPS)THEN
            FIELD(1) = 10.*BX(3)
            FIELD(2) = 10.*BY(3)
            GOTO 997
          ELSEIF(Y2 .LT. EPS .AND. X1 .LT. EPS)THEN
            FIELD(1) = 10.*BX(2)
            FIELD(2) = 10.*BY(2)
            GOTO 997
          ENDIF
          BDX = .5*(BX(3) - BX(1))/GRID !FIRST DERIVATIVE IN X
          BX1 = BX(1) + X1*BDX
          BDX = .5*(BX(4) - BX(2))/GRID !FIRST DERIVATIVE IN X
          BX2 = BX(2) + X1*BDX
          BDY = 0.5*(BY(2) - BY(1))/GRID !FIRST DERIVATIVE IN Y
          BY1 = BY(1) + Y1*BDY
          BDY = 0.5*(BY(4) - BY(3))/GRID
          BY2 = BY(3) + Y1*BDY
          BDY = .5*(BY2 - BY1)/GRID
          BDX = .5*(BX2 - BX1)/GRID
          FIELD(1) = 10.*(BX1 + BDX*Y1)
          FIELD(2) = 10.*(BY1 + BDY*X1)
  997     continue

          F(1)=FIELD(1)
          F(2)=FIELD(2)

        ENDIF               ! CF, EF or SAMUS switch

        ENDIF               ! VERSION switch
      ENDIF                 ! SMUO(3) switch
C
      CALL GFCVOL         !  Restore GEANT volume tree after GMEDIA call!!
C
      RETURN
      END 
