      SUBROUTINE MODULE_CORNERS( LCLGA, PHIMD, X, Y, Z, NC, IERR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : TO SUPPLY THE COORDINATES OF THE CORNERS
C-           OF A MODULE.  X, Y, AND Z ARE ARRAYS GIVING THE CORNER 
C-           POINTS.   THE MODULE 
C-           IS ADDRESSED BY THE POINTER TO THE 'CLGA' BANK DESCRIBING 
C-           THE MODULE.  
C-
C-   Inputs  :    LCLGA     POINTER TO MODULE BANK
C-                PHIMD     AZIMUTHAL ROTATION TO ROTATE THE MODULE
C-                          POINTED BY 'LCLGA' TO DESCRIBE THE MODULE
C-                          OF INTEREST.  (IF 'LCLGA' IS MODULE OF 
C-                          INTEREST, 'PHIMD' IS ZERO.)
C-   Outputs :    X         X COORDINATE CORNER VECTOR
C-                Y         Y COORDINATE CORNER VECTOR 
C-                Z         Z COORDINATE CORNER VECTOR
C-                NC        NUMBER OF SIDES OF THE CELL
C-                IERR      ERROR FLAG -- 0: OK
C-                                        1: NO CLGA BANK FOR GIVEN INDICES
C-   Controls: 
C-
C-   Created   11-JAN-1991   Stephen Kahn
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:CLINKS.INC'
      INCLUDE 'D0$PARAMS:CLGA.PARAMS'
      INCLUDE 'D0$INC:CSHA.DEF'
      INCLUDE 'D0$INC:REGION.DEF'
C
      INTEGER MCORNS
      PARAMETER (MCORNS = 20)          ! maximum number of corners
      INTEGER IETA, ILAYER, IPHI, MPHI, JPHI, NSBLAY, JBYT, JSBLAY
      INTEGER JETA, KPHI, JLAYER, JERR, IERR, NS, I, J, K, NC, N1
      INTEGER LCLGA, IDEN
      REAL XC, YC, ZC, SGN, PSI, COSPSI, SINPSI, X1
      REAL PHI, TANALF, HI, TL, BL, RI, RO, DZ, TOL
      REAL    RC, TANBETA, BETA, COSBETA, SINBETA, R1, R2, Z1, Z2
      REAL COSBEG, COSEND, SINBEG, SINEND, PHIMD, CARTES
      REAL X(MCORNS), Y(MCORNS), Z(MCORNS)
      LOGICAL FIRST
      SAVE FIRST
      DATA FIRST /.TRUE./
      DATA TOL / 4.0 /
C
      IERR =  1
      IF( FIRST) THEN                  ! initialize PSI
        FIRST = .FALSE.
      END IF
C
      LQCLGA = LCLGA
      SGN = 1.
      IERR = 0
      XC = CARTES(C(LQCLGA+IGRCEN), 1, IC(LQCLGA+IGCOOR))  ! mean cell X
      YC = CARTES(C(LQCLGA+IGRCEN), 2, IC(LQCLGA+IGCOOR))  ! mean cell Y
      ZC = CARTES(C(LQCLGA+IGRCEN), 3, IC(LQCLGA+IGCOOR))*SGN  ! mean cell Z
      LQCSHA = LC(LQCLGA-IXCSHA)      ! pointer to shape bank
C
      IF( IC(LQCLGA + IGSHAP) .EQ. 'TRAP' ) THEN      ! CC cells
        NS = 12              ! 12 side
        NC = 8               ! 8 corner
        DO 210 I = 0, 4, 4
        TANALF = TAND( C( LQCSHA + IGPAR7 + I))     ! tan(alpha_i)
        HI = C(LQCSHA + IGPAR4 + I)
        BL = C(LQCSHA + IGPAR5 + I)
        TL = C(LQCSHA + IGPAR6 + I)
C
        X(I+1) = -HI
        X(I+2) = -HI
        X(I+3) = HI
        X(I+4) = HI
C
        Y(I+1) = - BL - HI*TANALF
        Y(I+2) =   BL - HI*TANALF
        Y(I+3) = - TL + HI*TANALF
        Y(I+4) =   TL + HI*TANALF
C
        DO 200 J = 1, 4
  200   Z(I+J) = (-1.)**(I/4+1)*C(LQCSHA + IGPAR1)
  210   CONTINUE
C
        COSPSI = SIN(C(LQCLGA + IGOMGE))    ! PSI = HALFPI - OMEGA
        SINPSI = -COS(C(LQCLGA + IGOMGE)) 
        DO 220 I = 1, 8
        X1 = X(I)
        X( I) = X1*COSPSI - Y(I)*SINPSI + XC
        Y( I) = X1*SINPSI + Y(I)*COSPSI + YC
        Z( I) = SGN*Z( I) + ZC
  220   CONTINUE
C
      ELSE IF( IC(LQCLGA + IGSHAP) .EQ. 'TRD1' ) THEN      ! CC modules
        NS = 12              ! 12 side
        NC = 8               ! 8 corner
        DO 240 I = 0, 4, 4
        HI = C(LQCSHA + IGPAR4)
        BL = C(LQCSHA + IGPAR1)
        TL = C(LQCSHA + IGPAR2)
        DZ = C(LQCSHA + IGPAR3)
C
        X(I+1) = -HI
        X(I+2) = -HI
        X(I+3) = HI
        X(I+4) = HI
C
        Y(I+1) = - BL 
        Y(I+2) =   BL 
        Y(I+3) = - TL 
        Y(I+4) =   TL 
C
        IDEN = 100*(IC(LQCLGA+IGIDEN)/100)
        IF( IDEN .EQ. ICCFHA) THEN
          RC = SQRT( XC**2 + YC**2)
          RI = RC - HI
          RO = SQRT((RC+HI)**2 + TL**2)
          TANBETA = BL/RI
          BETA = ATAN(TANBETA)
          SINBETA = SIN(BETA)
          COSBETA = COS(BETA)
C
          X(I+1) = -HI - RI*(1-COSBETA)
          X(I+2) = -HI - RI*(1-COSBETA)
          X(I+3) = HI
          X(I+4) = HI
C
          Y(I+1) = -RI*SINBETA
          Y(I+2) = RI*SINBETA
          Y(I+3) = -TL
          Y(I+4) = TL
        END IF
C
        DO 230 J = 1, 4
  230   Z(I+J) = (-1.)**(I/4+1)* DZ
  240   CONTINUE
C
        COSPSI = COS(C(LQCLGA + IGOMGE))
        SINPSI = SIN(C(LQCLGA + IGOMGE))
        COSPSI = XC/SQRT(YC**2+XC**2)           ! temporary fix 
        SINPSI = YC/SQRT(YC**2+XC**2)           ! there is an error in OMEGA
        DO 250 I = 1, 8
        X1 = X(I)
        X( I) = X1*COSPSI - Y(I)*SINPSI + XC
        Y( I) = X1*SINPSI + Y(I)*COSPSI + YC
        Z( I) = SGN*Z( I) + ZC
  250   CONTINUE
C
      ELSE IF( IC(LQCLGA + IGSHAP) .EQ. 'TRD2' ) THEN      ! CC/CH modules
        NS = 12              ! 12 side
        NC = 8               ! 8 corner
        DO 260 I = 0, 4, 4
        HI = C(LQCSHA + IGPAR5)
        BL = C(LQCSHA + IGPAR1)
        TL = C(LQCSHA + IGPAR2)
        RC = SQRT(XC**2 + YC**2)
        RI = RC - HI         ! inner radius
        RO = SQRT((RC+HI)**2 + TL**2)     ! outer radius
        TANBETA = BL/RI
        BETA = ATAN(TANBETA)
        SINBETA = SIN(BETA)
        COSBETA = COS(BETA)
C
C       X(I+1) = -HI
C       X(I+2) = -HI
C       X(I+3) = HI
C       X(I+4) = HI
C
C       Y(I+1) = - BL 
C       Y(I+2) =   BL 
C       Y(I+3) = - TL 
C       Y(I+4) =   TL 
C
        X(I+1) = -HI - RI*(1-COSBETA)    ! CC/CH is not described by
        X(I+2) = -HI - RI*(1-COSBETA)    ! trapezoid.  It is sort of
        X(I+3) = HI                      ! a slanted TUBS.  This 
        X(I+4) = HI                      ! calculates its corners
C
        Y(I+1) = -RI*SINBETA
        Y(I+2) = RI*SINBETA
        Y(I+3) = -TL
        Y(I+4) = TL
C
        DO 270 J = 1, 4
        DZ = C(LQCSHA + IGPAR3)          
        Z2 = C(LQCSHA + IGPAR3)          ! patch to handle Z positions  
        Z1 = C(LQCSHA + IGPAR4)          ! of the odd shaped CC/CH end
        R2 = RI                          ! plate
        R1 = RO
        DZ = Z2 
        IF (J.GE.3) DZ = Z1 
  270   Z(I+J) = (-1.)**(I/4+1)*DZ
  260   CONTINUE
C
        COSPSI = COS(C(LQCLGA + IGOMGE))
        SINPSI = SIN(C(LQCLGA + IGOMGE))
        COSPSI = XC/SQRT(YC**2+XC**2)           ! temporary fix 
        SINPSI = YC/SQRT(YC**2+XC**2)           ! there is an error in OMEGA
        DO 280 I = 1, 8
        X1 = X(I)
        X( I) = X1*COSPSI - Y(I)*SINPSI + XC
        Y( I) = X1*SINPSI + Y(I)*COSPSI + YC
        Z( I) = SGN*Z( I) + ZC
  280   CONTINUE
C
      ELSE IF( IC( LQCLGA + IGSHAP) .EQ. 'TUBS') THEN      ! EE lay 1,2
        NS = 12
        NC = 8
C
        DO 310 I = 0, 4, 4
        SINBEG = SIND(C(LQCSHA + IGPAR4))
        COSBEG = COSD(C(LQCSHA + IGPAR4))
        SINEND = SIND(C(LQCSHA + IGPAR5))
        COSEND = COSD(C(LQCSHA + IGPAR5))
        RI = C(LQCSHA + IGPAR1)
        RO = C(LQCSHA + IGPAR2)
C
        X(I+1) = RI*COSEND
        X(I+2) = RI*COSBEG
        X(I+3) = RO*COSEND
        X(I+4) = RO*COSBEG
C
        Y(I+1) = RI*SINEND
        Y(I+2) = RI*SINBEG
        Y(I+3) = RO*SINEND
        Y(I+4) = RO*SINBEG
C
        DO 300 J = 1, 4
  300   Z(I+J) = ZC + (-1.)**(I/4+1)*C(LQCSHA+IGPAR3)*SGN
  310   CONTINUE
C
      ELSE IF( IC( LQCLGA + IGSHAP) .EQ. 'CONS') THEN      ! EE lay 3,4;
C                                        ! IH and MH calorimeter cells
        NS = 12
        NC = 8
C
        DO 410 I = 0, 4, 4
        SINBEG = SIND(C(LQCSHA + IGPAR6))
        COSBEG = COSD(C(LQCSHA + IGPAR6))
        SINEND = SIND(C(LQCSHA + IGPAR7))
        COSEND = COSD(C(LQCSHA + IGPAR7))
        RI = C(LQCSHA + IGPAR2 + I/2)
        RO = C(LQCSHA + IGPAR3 + I/2)
C
        X(I+1) = RI*COSEND
        X(I+2) = RI*COSBEG
        X(I+3) = RO*COSEND
        X(I+4) = RO*COSBEG
C
        Y(I+1) = RI*SINEND
        Y(I+2) = RI*SINBEG
        Y(I+3) = RO*SINEND
        Y(I+4) = RO*SINBEG
C
        DO 400 J = 1, 4
  400   Z(I+J) = ZC + (-1.)**(I/4+1)*C(LQCSHA+IGPAR1)*SGN
  410   CONTINUE
C
      ELSE IF(IC(LQCSHA + IGSHAP) .EQ. 'PCON') THEN        
        SINBEG = SIND(C(LQCSHA+IGPAR1))   ! sin of begin angle
        COSBEG = COSD(C(LQCSHA+IGPAR1))   ! cos "    "     "
        SINEND = SIND(C(LQCSHA+IGPAR2))   ! sin of end angle
        COSEND = COSD(C(LQCSHA+IGPAR2))   ! cos  "  "    "
C
        IF( C(LQCSHA+IGPAR3) .EQ. 4.) THEN  ! quadrilateral like shape
C 
        NC = 8                           ! number of corners
        NS = 12                          ! number of edges
C
        DO 520 I = 0, 6, 2
        J = I/2
        RI = C(LQCSHA + IGPAR5 + 3*J)
        IF ( I.EQ.4) RI = C(LQCSHA+IGPA12)
C
        X(I+1) = RI*COSBEG
        X(I+2) = RI*COSEND
C
        Y(I+1) = RI*SINBEG
        Y(I+2) = RI*SINEND
C
        Z(I+1) = C(LQCSHA+IGPAR4+3*J)*SGN
        Z(I+2) = Z(I+1)
  520 CONTINUE
C
        END IF
      ELSE IF(IC(LQCSHA + IGSHAP) .EQ. 'PGON') THEN        
        SINBEG = SIND(C(LQCSHA+IGPAR1))   ! sin of begin angle
        COSBEG = COSD(C(LQCSHA+IGPAR1))   ! cos "    "     "
        SINEND = SIND(C(LQCSHA+IGPAR2))   ! sin of end angle
        COSEND = COSD(C(LQCSHA+IGPAR2))   ! cos  "  "    "
C
        IF( C(LQCSHA+IGPAR4) .EQ. 4.) THEN  ! quadrilateral like shape
C 
        NC = 8                           ! number of corners
        NS = 12                          ! number of edges
C
        DO 560 I = 0, 6, 2
        J = I/2
        RI = C(LQCSHA + IGPAR6 + 3*J)
        IF ( I.EQ.4) RI = C(LQCSHA+IGPA13)
C
        X(I+1) = RI*COSBEG
        X(I+2) = RI*COSEND
C
        Y(I+1) = RI*SINBEG
        Y(I+2) = RI*SINEND
C
        Z(I+1) = C(LQCSHA+IGPAR5+3*J)*SGN
        Z(I+2) = Z(I+1)
  560 CONTINUE
C
        END IF
      END IF
C
C     ROTATE TO PROPER IPHI
C
      IF (PHIMD .NE. 0.) THEN              ! no rotation necessary
      COSPSI = COS( PHIMD)
      SINPSI = SIN( PHIMD)
      DO 600 I = 1, NC
      X1 = X(I)
      X(I) = X1 * COSPSI - Y(I) * SINPSI
      Y(I) = X1 * SINPSI + Y(I) * COSPSI
  600 CONTINUE
      END IF
C
C----------------------------------------------------------------------
  999 RETURN
      END
