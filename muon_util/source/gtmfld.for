      SUBROUTINE GTMFLD(QUAD,VECT,F)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : The routine reads the map field from MMAP
C-        OR ALTERNATIVELY USES D.Hedin FIELD MAP. It is patterned
C    off of GTBFLD but has additions to check for old MONTE CARLO, to
C    make fiducial cuts on the magnet, and to use the magnet's local
C    coordinates
C-   Inputs  : QUAD-quadrant,VECT-position
C-   Outputs : F-field in Kgauss
C-   Controls:
C-
C-   Created  13-MAR-1991  T.F,A.Kl
C-   Updated  26-APR-1991   A.Klatchko
C-   Akl      26-APR-1991 convert output to Kgauss
C    D. Hedin 1/92 first use of GTMFLD. add call to MAGLOC so now local.
C      (MAGLOC makes fiducial cut)
C     use MUD1 to flag old Monte Carlo. fix bug for central bottom
C    DH 2/92 fix edge effects, fix INT problem for central bottom
C
C    Alex Mesin 7/16/92 - Uses either constant magnetic field map,
C                                   MUO_STPFILE.DAT (sorted in Y,X)
C                                   MUO_STPFILE_2_N.DAT (sorted in X,Y)
C    Jim Linnemann 2/13/93  - Change intmsg to errmsg for L2
C    Atsushi Taketani 24-MAR-93 Support magnet polarity flip
C    Susumu Igarashi 4/13/93  - For magnet rotation MMAH version 4
C    Atsushi Taketani 5/28/93 3dim magnet polarity flip
C    sss 7/9/93 - Make sure lmmah is valid before using it
C-   Updated  27-JAN-1995   Andrei Mayorov   add call to EZPICK
C-   Updated  29-AUG-1995   Andrei Mayorov   check if LMUD1 = 0
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      LOGICAL USE_FIELD_MAP
      REAL VECT(3),        ! INPUT:INITIAL COORDINATES
     1  F(3),               ! OUTPUT:FIELD COMPONENTS
     2  X(3),PX,PY,PZ,PBX,PBY,PBZ,BXMAX,BYMAX,BMAX
      INTEGER NUMED,I,LMMAP,GZMMAP,IER,FIRST,QUAD,DMUO,TIMES
      INTEGER LMMAH,GZMMAH
      INTEGER XINT,YINT,IABX1,IABY1,IABX2,IABY2,IABX3,IABY3,
     &        IABX4,IABY4,L,GZMUD1,MC,NUMVER
      REAL BX1,BY1,BX2,BY2,BX3,BY3,BX4,BY4
      REAL    X1,X2,Y1,Y2,XX1,YY1,XX,YY,XX2,YY2

      REAL BX(4),BY(4),POINT(2),GRIDX(2),GRIDY(2),FIELD(2),SIX
      REAL EPS,GRID,BDX,BDY

      REAL EFXDIS,EFYDIS,EFXLOC,EFYLOC,EFREMA,CFXDIS,CFYDIS
      REAL CFXLOC,CFYLOC,CFREMA,SAMDIS,SAMLOC,SAMREMA,SYMMETRY
      REAL MAGROT(3,3),F1(3)
      INTEGER J,VERSION_MAP
      INTEGER EFXNUM,EFYNUM,CFXNUM,CFYNUM,EFXCOR,EFYCOR,CFXCOR,CFYCOR
      INTEGER SAMNUM,SAMCOR
      REAL     POL_WAM, POL_SAM
      INTEGER  IMERR

      DATA FIRST/0/
      DATA TIMES/1/
      DATA NUMVER/536875808/
      DATA EPS /0.001/
C----------------------------------------------------------------------
C
C-   GET FIELD MAP
C
      IF(FIRST.EQ.0)THEN
        call ezpick('MURECO_RCP')
        CALL EZGET('USE_FIELD_MAP',USE_FIELD_MAP,IER)
        CALL EZRSET
        FIRST=1
        L=GZMUD1(0)
        if(l.gt.0) then
        MC=0
        IF(IQ(L+4).EQ.1) THEN
          MC=1                     ! OLD MONTE CARLO
          CALL ERRMSG('MU_FIELD','GTMFLD',
     &   ' Old MC generated with Constant Magnetic Field Map ','S')
        ELSE IF(IQ(L+4).EQ.NUMVER) THEN ! NEW MONTE CARLO
          VERSION_MAP=MOD(IQ(L+6),256)
          IF(VERSION_MAP.EQ.0)THEN
            MC=1
          CALL ERRMSG('MU_FIELD','GTMFLD',
     &        ' MC generated with Constant Magnetic Field Map ','S') 
          ELSEIF(VERSION_MAP.EQ.2)THEN
            MC=2
          CALL ERRMSG('MU_FIELD','GTMFLD',
     &        ' MC generated with Magnetic Field Map Version 2','S') 
          ELSEIF(VERSION_MAP.EQ.3)THEN
            MC=3
          CALL ERRMSG('MU_FIELD','GTMFLD',
     &        ' MC generated with Magnetic Field Map Version 3','S') 
          ELSEIF(VERSION_MAP.EQ.4)THEN
            MC=4
          CALL ERRMSG('MU_FIELD','GTMFLD',
     &        ' MC generated with Magnetic Field Map Version 4','S') 
          ENDIF
          end if

        ENDIF
        IF(USE_FIELD_MAP) THEN
          DMUO=2               ! USE CALCULATED FIELD MAP
        ELSE
          DMUO=1               ! USE CONSTANT FIELD MAP
        ENDIF

        IF(MC.EQ.1) DMUO=1   !DONOT USE MAP IF OLD MC
      ENDIF
      F(1) = 0.0
      F(2) = 0.0
      F(3) = 0.0

      IF (TIMES .EQ. 1) THEN
        TIMES=2
        LMMAH=GZMMAH(0)   !Get location of which field map to use

        IF ((IC(LMMAH+1) .EQ. 2) .AND. (USE_FIELD_MAP)) THEN
          DMUO=2     !USE MUO_STPFILE.DAT (sorted in Y,X)
          CALL ERRMSG('MU_FIELD','GTMFLD',
     &      ' Magnetic Field Map Version 2 being used ','S')
          IF(MC.NE.2.AND.MC.NE.0)THEN
            CALL ERRMSG('MU_FIELD_BAD','GTMFLD',
     &      'Inconsistent version between GEANT and RECO','W')
          ENDIF
        ELSEIF ((IC(LMMAH+1) .EQ. 3) .AND. (USE_FIELD_MAP)) THEN
          DMUO=3     !USE MUO_STPFILE_2.DAT (sorted in X,Y)
          CALL ERRMSG('MU_FIELD','GTMFLD',
     &     ' Magnetic Field Map Version 3 being used ','S')
          IF(MC.NE.3.AND.MC.NE.0)THEN
            CALL ERRMSG('MU_FIELD_BAD','GTMFLD',
     &       'Inconsistent version between GEANT and RECO','W')
          ENDIF
        ELSEIF ((IC(LMMAH+1) .EQ. 4) .AND. (USE_FIELD_MAP)) THEN
          DMUO=4     !USE MUO_STPFILE_3.DAT (magnet rotation)
          CALL ERRMSG('MU_FIELD','GTMFLD',
     &     ' Magnetic Field Map Version 4 being used ','S')
          IF(MC.NE.4.AND.MC.NE.0)THEN
            CALL ERRMSG('MU_FIELD_BAD','GTMFLD',
     &       'Inconsistent version between GEANT and RECO','W')
          ENDIF
        ELSE
          DMUO=1     !USE CONSTANT FIELD MAP
          CALL ERRMSG('MU_FIELD','GTMFLD',
     &  ' Constant Magnetic Field Map being used (BT = 20.0)','S')
          IF(MC.NE.1.AND.MC.NE.0)THEN
            CALL ERRMSG('MU_FIELD_BAD','GTMFLD',
     &       'Inconsistent version between GEANT and RECO','W')
          ENDIF
        ENDIF
      ENDIF

      IF (DMUO.EQ.1 ) THEN       !no field map old DH map

        PX=VECT(1)
        PY=VECT(2)
        PZ=VECT(3)
        IF ((PX .NE. 0.0) .AND. (PY .NE. 0.0)) THEN
          CALL MUBFLD(QUAD,PX,PY,PZ,PBX,PBY,PBZ)   !field in Kgauss
        ELSE
          PBX=20.0
          PBY=0.0
          PBZ=0.0
        ENDIF
        F(1) = PBX
        F(2) = PBY
        F(3) = PBZ



      ELSEIF (DMUO .EQ. 2) THEN

C        This part of the program (only if DMUO=2) uses the old
C        code of GTMFLD.  It is hard wired to the parameters of 
C        the field map in MUO_STPFILE.DAT.  It does not have a
C        separate subroutine to calculate the SAMUS field, so if
C        a point is located in SAMUS, the program uses the same
C        code as for the EF.  


        CALL MAGLOC(VECT,NUMED,X)  !Check if the pont is in a magnet

C    X IS NOW IN MAGNET LOCAL COORDINATES
C    NUMED=0 THEN NOT IN MAGNET FIDUCIAL, =2 END, =1 CENTRAL
C                                         =3 SAMUS

        IF (NUMED .EQ. 3) NUMED=2

        IF (NUMED .EQ. 2) THEN                 !END TOROID
          X(1)=ABS(X(1))
          X(2)=ABS(X(2))
          LMMAP=GZMMAP(2)     !Get starting address of the field map
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
     *      ((X1+X2)*(Y1+Y2))
C
          F(2) = 10.*(Y2*(X2*BY1 + X1*BY2) + Y1*(BY3*X2 + BY4*X1)) /
     *      ((X1+X2)*(Y1+Y2))
CC     Upper left  quadrant:X<0,Y>=0
          IF (VECT(1) .LT. 0.0 .AND. VECT(2) .GE. 0.0)F(2) = -F(2)
C     Lower left quadrant:X<0,Y<0
          IF (VECT(1) .LT. 0.0 .AND. VECT(2) .LT. 0.0)THEN
            F(1) = -F(1)
            F(2) = -F(2)
          ENDIF
C     Lower right quadrant:X>=0,Y>0
          IF (VECT(1) .GE. 0.0 .AND. VECT(2) .LT. 0.0)F(1) = -F(1)
C
        ELSE IF(NUMED .EQ. 1) THEN       !CF
          X(1)=ABS(X(1))
C
          LMMAP=GZMMAP(1)
CC   GIVEN A POSITION (X(1),X(2),X(3)) = (X,Y,Z) WHERE FIELD IS TO BE FOUND
C
          XX = X(1)/20.0
          XINT = INT(XX)
          YY = X(2)/20.0
          YINT = INT(YY)
          IF(X(2).LT.0.) YINT=YINT-1.
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
     *      ((X1+X2)*(Y1+Y2))
C
          F(2) = 10.*(Y2*(X2*BY1 + X1*BY2) + Y1*(BY3*X2 + BY4*X1)) /
     *      ((X1+X2)*(Y1+Y2))
C Mirror values
          IF(VECT(1) .LT. 0.0)THEN
            F(2) = -F(2)
          ENDIF
        ENDIF




      ELSEIF (DMUO .EQ. 3 .OR. DMUO .EQ. 4) THEN

C     This part of the program uses the STP file called 
C     MUO_STPFILE_2_N.DAT.  The fiel is sorted in X, then Y.
C     The parameters of the field map (starting point, grid
C     spacing, etc. is not hard wired and is read in from the
C     STP file.

C     VERSION 4 has small rotation for both North and South EF magnets.
C     MUO_STPFILE_3.DAT has MMAH that stores rotation matrix and 
C     translation. MMAG is updated for each position and rotation.

        CALL MAGLOC(VECT,NUMED,X)

C    X IS NOW IN MAGNET LOCAL COORDINATES
C    NUMED=0 THEN NOT IN MAGNET FIDUCIAL, =2 END, =1 CENTRAL,
C                                         =3 SAMUS

        IF (NUMED .EQ. 2) THEN                  !END TOROID

          LMMAP=GZMMAP(2)

          EFXNUM=IC(LMMAP+11)   !Number of points in the X direction
          EFYNUM=IC(LMMAP+12)   !Number of points in the Y direction
          EFXLOC=C(LMMAP+13)    !X starting point
          EFYLOC=C(LMMAP+14)    !Y starting point
          EFXDIS=C(LMMAP+15)    !Grid spacing in the X direction
          EFYDIS=C(LMMAP+16)    !Grid spacing in the Y direction

C   --  Check if the map is symmetric about (0,0)
          SYMMETRY=1.0-ABS(EFXLOC)/ABS(EFXLOC+((EFXNUM-1)*EFXDIS))

C   --  EFXCOR,EFYCOR are by how many points the map is offset.
C       This is needed to get the first point of the graph
C       to be mapped to a location of 1

          IF (SYMMETRY .LT. 0.001) THEN
            EFXCOR=(EFXNUM/2)-1
          ELSE
            EFXCOR=-NINT(EFXLOC/EFXDIS)
          ENDIF
          SYMMETRY=1.0-ABS(EFYLOC)/ABS(EFYLOC+((EFYNUM-1)*EFYDIS))
          IF (SYMMETRY .LT. 0.001) THEN
            EFYCOR=(EFYNUM/2)-1
          ELSE
            EFYCOR=-NINT(EFYLOC/EFYDIS)
          ENDIF

C   -- EFREMA is the amount the map is offset from the spacing
C      if size of map is from -4.95 to 4.95 by 0.1, the EFREMA is 0.05
C      if size of map is from -4.95 to 4.95 by 0.05, the EFREMA is 0

          EFREMA=100*EFXDIS*ABS((EFXLOC/EFXDIS)-INT(EFXLOC/EFXDIS))
          EFXDIS=EFXDIS*100   !Convert X grid spacing to CM
          EFYDIS=EFYDIS*100   !Convert Y grid spacing to CM

C     FIND THE ARAY INDECES OF THE GRID POSITIONS IN THE ZEBRA MAP
          XX = X(1)/EFXDIS
          XINT = NINT(XX)+EFXCOR
          YY = X(2)/EFYDIS
          YINT = NINT(YY)+EFYCOR

C     CALCULATE THE GRID POINTS
          XX1 = (XINT-EFXCOR)*EFXDIS
          YY1 = (YINT-EFYCOR)*EFYDIS
          XX2 = XX1 + EFXDIS
          YY2 = YY1 + EFYDIS
C
CCC   NOW THE 4 NEW VALUES OF X,Y FOR WHICH THE BX,BY IS KNOWN
CCC   ARE (XX1,YY1),(XX2,YY1),(XX1,YY2),(XX2,YY2)
C
          IABX1 = 2*(EFYNUM*XINT) + YINT*2 +1
          IABY1 = 2*(EFYNUM*XINT) + YINT*2 +2
          IABX2 = 2*(EFYNUM*XINT) + YINT*2 +3
          IABY2 = 2*(EFYNUM*XINT) + YINT*2 +4
          IABX3 = 2*(EFYNUM*(XINT+1)) + YINT*2 +1
          IABY3 = 2*(EFYNUM*(XINT+1)) + YINT*2 +2
          IABX4 = 2*(EFYNUM*(XINT+1)) + YINT*2 +3
          IABY4 = 2*(EFYNUM*(XINT+1)) + YINT*2 +4
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


          GRID=EFREMA

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
  999     CONTINUE

          IF(DMUO.EQ.3)THEN
            F(1)=FIELD(1)
            F(2)=FIELD(2)
          ELSEIF(DMUO.EQ.4)THEN 
C North
            lmmah = gzmmah (0)
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


        ELSE IF(NUMED .EQ. 1) THEN                               !CF

C    -- CF variables have the same meaning as ef variables

          LMMAP=GZMMAP(1)

          CFXNUM=IC(LMMAP+11)
          CFYNUM=IC(LMMAP+12)
          CFXLOC=C(LMMAP+13)
          CFYLOC=C(LMMAP+14)
          CFXDIS=C(LMMAP+15)
          CFYDIS=C(LMMAP+16)
          SYMMETRY=1.0-ABS(CFXLOC)/ABS(CFXLOC+((CFXNUM-1)*CFXDIS))
          IF (SYMMETRY .LT. 0.001) THEN
            CFXCOR=(CFXNUM/2)-1
          ELSE
            CFXCOR=-NINT(CFXLOC/CFXDIS)
          ENDIF
          SYMMETRY=1.0-ABS(CFYLOC)/ABS(CFYLOC+((CFYNUM-1)*CFYDIS))
          IF (SYMMETRY .LT. 0.001) THEN
            CFYCOR=(CFYNUM/2)-1
          ELSE
            CFYCOR=-NINT(CFYLOC/CFYDIS)
          ENDIF
          CFREMA=100*CFXDIS*ABS((CFXLOC/CFXDIS)-INT(CFXLOC/CFXDIS))
          CFXDIS=CFXDIS*100
          CFYDIS=CFYDIS*100



CC   GIVEN A POSITION (X(1),X(2),X(3)) = (X,Y,Z) WHERE FIELD IS TO BE FOUND
C
C     SEE IF SYMMETRY REFLECTION WILL BE NEEDED
          IF (X(1) .NE. 0.0) THEN
            SIX=X(1)/ABS(X(1))
          END IF

          X(1)=ABS(X(1))

C     CALCULATE INDECES OF  MAP ARRAY IN ZEBRA FILE
          XX = X(1)/CFXDIS
          XINT = NINT(XX)+CFXCOR
          YY = X(2)/CFYDIS
          YINT = NINT(YY)+CFYCOR

C     DETERMINE FOUR NEAREST NEIGHBORS AROUND POINT TO BE CALCULATED
          XX1 = (XINT-CFXCOR)*CFXDIS
          YY1 = (YINT-CFYCOR)*CFYDIS
          XX2 = XX1 + CFXDIS
          YY2 = YY1 + CFYDIS
C
CCC   NOW THE 4 NEW VALUES OF X,Y FOR WHICH THE BX,BY ARE KNOWN
CCC   ARE (XX1,YY1),(XX2,YY1),(XX1,YY2),(XX2,YY2)
C
          IABX1 = 2*(CFYNUM*XINT) + YINT*2 +1
          IABY1 = 2*(CFYNUM*XINT) + YINT*2 +2
          IABX2 = 2*(CFYNUM*XINT) + YINT*2 +3
          IABY2 = 2*(CFYNUM*XINT) + YINT*2 +4
          IABX3 = 2*(CFYNUM*(XINT+1)) + YINT*2 +1
          IABY3 = 2*(CFYNUM*(XINT+1)) + YINT*2 +2
          IABX4 = 2*(CFYNUM*(XINT+1)) + YINT*2 +3
          IABY4 = 2*(CFYNUM*(XINT+1)) + YINT*2 +4

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

          GRID=CFREMA

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
  998     CONTINUE

C     SYMMETRY REFLECTION FOR OTHER HALF OF CF
          IF(SIX .LT. 0.0)THEN
            FIELD(2) = -FIELD(2)
          ENDIF

          IF(DMUO.EQ.3)THEN
            F(1)=FIELD(1)
            F(2)=FIELD(2)
          ELSEIF(DMUO.EQ.4)THEN
            lmmah = gzmmah (0)
            DO 203 I=1,3
              DO 203 J=1,3
  203           MAGROT(I,J)=C(LMMAH+34+I+3*J)    ! Rotation matrix
            F1(1)=FIELD(1)
            F1(2)=FIELD(2)
            F1(3)=0.
            CALL VMATR( F1, MAGROT, F, 3, 3 )
          ENDIF


        ELSE IF(NUMED .EQ. 3) THEN                       !SAMUS

C  -- SAMUS variables have the same meanings as the EF amd CF
C  -- variable, only it is assumed the the X and Y parameters of
C  -- the SAMUS field map are the same.

          LMMAP=GZMMAP(2)

          EFXNUM=IC(LMMAP+11)   !Number of points in the X direction
          EFYNUM=IC(LMMAP+12)   !Number of points in the Y direction

          SAMLOC=C(LMMAP+18)
          SAMNUM=IC(LMMAP+19)
          SAMDIS=C(LMMAP+20)
          SYMMETRY=1.0-ABS(SAMLOC)/ABS(SAMLOC+((SAMNUM-1)*SAMDIS))
          IF (SYMMETRY .LT. 0.001) THEN
            SAMCOR=(SAMNUM/2)-1
          ELSE
            SAMCOR=-NINT(SAMLOC/SAMDIS)
          ENDIF
          SAMREMA=100*SAMDIS*ABS((SAMLOC/SAMDIS)-INT(SAMLOC/SAMDIS))
          SAMDIS=SAMDIS*100

C     FIND THE ARAY INDECES OF THE GRID POSITIONS IN THE ZEBRA MAP
          XX = X(1)/SAMDIS
          XINT = NINT(XX)+SAMCOR
          YY = X(2)/SAMDIS
          YINT = NINT(YY)+SAMCOR

C     CALCULATE THE GRID POINTS
          XX1 = (XINT-SAMCOR)*SAMDIS
          YY1 = (YINT-SAMCOR)*SAMDIS
          XX2 = XX1 + SAMDIS
          YY2 = YY1 + SAMDIS
C
CCC   NOW THE 4 NEW VALUES OF X,Y FOR WHICH THE BX,BY IS KNOWN
CCC   ARE (XX1,YY1),(XX2,YY1),(XX1,YY2),(XX2,YY2)
C
          IABX1 = 2*(SAMNUM*XINT) + YINT*2 +1
          IABY1 = 2*(SAMNUM*XINT) + YINT*2 +2
          IABX2 = 2*(SAMNUM*XINT) + YINT*2 +3
          IABY2 = 2*(SAMNUM*XINT) + YINT*2 +4
          IABX3 = 2*(SAMNUM*(XINT+1)) + YINT*2 +1
          IABY3 = 2*(SAMNUM*(XINT+1)) + YINT*2 +2
          IABX4 = 2*(SAMNUM*(XINT+1)) + YINT*2 +3
          IABY4 = 2*(SAMNUM*(XINT+1)) + YINT*2 +4

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

          BX1 = C(LMMAP+(EFXNUM*EFYNUM*2)+20+IABX1)
          BY1 = C(LMMAP+(EFXNUM*EFYNUM*2)+20+IABY1)
          BX2 = C(LMMAP+(EFXNUM*EFYNUM*2)+20+IABX2)
          BY2 = C(LMMAP+(EFXNUM*EFYNUM*2)+20+IABY2)
          BX3 = C(LMMAP+(EFXNUM*EFYNUM*2)+20+IABX3)
          BY3 = C(LMMAP+(EFXNUM*EFYNUM*2)+20+IABY3)
          BX4 = C(LMMAP+(EFXNUM*EFYNUM*2)+20+IABX4)
          BY4 = C(LMMAP+(EFXNUM*EFYNUM*2)+20+IABY4)

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

          GRID=SAMREMA

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
  997     CONTINUE

          F(1)=FIELD(1)
          F(2)=FIELD(2)

        ENDIF
      ENDIF
C
      CALL MU_MAG_POL( 0, POL_WAM, POL_SAM, IMERR )
      IF ( IMERR.EQ.0 ) THEN
        IF ( POL_WAM.LT.0.0 .OR. POL_SAM.LT. 0.0 ) THEN
          IF ( DMUO.EQ.1 ) THEN
            CALL MAGLOC(VECT,NUMED,X) 
          END IF
          IF ( NUMED.EQ.3 ) THEN
            F(1) = POL_SAM*F(1)
            F(2) = POL_SAM*F(2)
            F(3) = POL_SAM*F(3)
          ELSE 
            F(1) = POL_WAM*F(1)
            F(2) = POL_WAM*F(2)
            F(3) = POL_WAM*F(3)
          END IF
        END IF
      END IF
C
      RETURN
      END
