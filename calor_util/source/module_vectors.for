      SUBROUTINE MODULE_VECTORS( LCLGA, PHIMD, XS, YS, ZS, NS, IERR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : TO SUPPLY THE VECTORS THAT REPRESENT THE 
C-           SIDES OF A DESCRIPTION OF A MODULE. 
C-           ZS ARE (2, NS) ARRAYS GIVING THE BEGINNING AND END POINTS
C-           OF LINES REPRESENTING THE SIDES OF A MODULE.  THE MODULE 
C-           IS ADDRESSED BY THE POINTER TO THE 'CLGA' BANK DESCRIBING 
C-           THE MODULE.  
C-
C-   Inputs  :    LCLGA     POINTER TO MODULE BANK
C-                PHIMD     AZIMUTHAL ROTATION TO ROTATE THE MODULE
C-                          POINTED BY 'LCLGA' TO DESCRIBE THE MODULE
C-                          OF INTEREST.  (IF 'LCLGA' IS MODULE OF 
C-                          INTEREST, 'PHIMD' IS ZERO.)
C-   Outputs :    XS        X COORDINATE SIDE VECTOR
C-                YS        Y COORDINATE SIDE VECTOR 
C-                ZS        Z COORDINATE SIDE VECTOR
C-                NS        NUMBER OF SIDES OF THE CELL
C-                IERR      ERROR FLAG -- 0: OK
C-                                        1: NO CLGA BANK FOR GIVEN INDICES
C-   Controls: 
C-
C-   Created   10-JAN-1990   Stephen Kahn
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:CLINKS.INC'
      INCLUDE 'D0$PARAMS:CLGA.PARAMS'
      INCLUDE 'D0$INC:CSHA.DEF'
C
      INTEGER MSIDES, MCORNS
      PARAMETER (MSIDES = 32)          ! maximum number of sides
      PARAMETER (MCORNS = 20)          ! maximum number of corners
      REAL RAD
      PARAMETER (RAD=0.017453293)
      INTEGER IETA, ILAYER, IPHI, MPHI, JPHI, NSBLAY, JBYT, JSBLAY
      INTEGER JETA, KPHI, JLAYER, JERR, IERR, NS, I, J, K, NC, N1
      INTEGER LCLGA
      REAL XC, YC, ZC, SGN, PSI, COSPSI, SINPSI, X1, Y1, Z1
      REAL PHI, TANALF, HI, TL, BL, RI, RO, DZ, TOL
      REAL COSBEG, COSEND, SINBEG, SINEND, PHIMD, CARTES
      REAL X(MCORNS), Y(MCORNS), Z(MCORNS)
      REAL XS(2,MSIDES), YS(2,MSIDES), ZS(2,MSIDES) 
      REAL*8 ROTM(9)
      LOGICAL FIRST
      SAVE FIRST
      CHARACTER*4 CHAR41,CHAR42,CHAR43,CHAR44,CHAR45,CHAR46
      INTEGER ICHAR41,ICHAR42,ICHAR43,ICHAR44,ICHAR45,ICHAR46
      EQUIVALENCE (ICHAR41,CHAR41)
      EQUIVALENCE (ICHAR42,CHAR42)
      EQUIVALENCE (ICHAR43,CHAR43)
      EQUIVALENCE (ICHAR44,CHAR44)
      EQUIVALENCE (ICHAR45,CHAR45)
      EQUIVALENCE (ICHAR46,CHAR46)
      DATA     CHAR41 /'TRAP'/
      DATA     CHAR42 /'TRD1'/
      DATA     CHAR43 /'TRD2'/
      DATA     CHAR44 /'TUBE'/
      DATA     CHAR45 /'PCON'/
      DATA     CHAR46 /'PGON'/

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
      IF( IC(LQCLGA + IGSHAP) .EQ. ICHAR41 ) THEN      ! CC cells
        NS = 12              ! 12 side
        NC = 8               ! 8 corner
        DO 210 I = 0, 4, 4
        TANALF = TAN( C( LQCSHA + IGPAR7 + I)*RAD)     ! tan(alpha_i)
        HI = C(LQCSHA + IGPAR4 + I)
        BL = C(LQCSHA + IGPAR5 + I)
        TL = C(LQCSHA + IGPAR6 + I)
C
        Z(I+1) = -HI
        Z(I+2) = -HI
        Z(I+3) = HI
        Z(I+4) = HI
C
        Y(I+1) = - BL - HI*TANALF
        Y(I+2) =   BL - HI*TANALF
        Y(I+3) = - TL + HI*TANALF
        Y(I+4) =   TL + HI*TANALF
C
        DO 200 J = 1, 4
        Z(I+J) = Z(I+J) + (-1.)**(I/4)*SIN(C(LQCSHA+IGPAR2)*RAD) *  
     &    C(LQCSHA+IGPAR1)
  200   X(I+J) = (-1.)**(I/4+1)*C(LQCSHA + IGPAR1) *                    
     &     COS(C(LQCSHA+IGPAR2)*RAD)
  210   CONTINUE
C
        CALL PAXROT(C(LQCLGA+IGOMGE),ROTM)     ! get rotation matrix
                                        ! from angles
        DO 220 I = 1, 8
        X1 = X(I)
        Y1 = Y(I)
        Z1 = Z(I)
        X( I) = X1*ROTM(2) + Y1*ROTM(1) + Z1*ROTM(3) + XC
        Y( I) = X1*ROTM(8) + Y1*ROTM(7) + Z1*ROTM(9) + YC
        Z( I) = (X1*ROTM(5) + Y1*ROTM(4) + Z1*ROTM(6))*SGN +ZC
  220   CONTINUE
C
      ELSE IF( IC(LQCLGA + IGSHAP) .EQ. ICHAR42 ) THEN      ! CC modules
        NS = 12              ! 12 side
        NC = 8               ! 8 corner
        DO 240 I = 0, 4, 4
        HI = C(LQCSHA + IGPAR4)
        BL = C(LQCSHA + IGPAR1)
        TL = C(LQCSHA + IGPAR2)
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
        DO 230 J = 1, 4
  230   Z(I+J) = (-1.)**(I/4+1)*C(LQCSHA + IGPAR3)
  240   CONTINUE
C
        CALL PAXROT(C(LQCLGA+IGOMGE),ROTM)     ! get rotation matrix
                                        ! from angles
        DO 250 I = 1, 8
        X1 = X(I)
        Y1 = Y(I)
        Z1 = Z(I)
        X( I) = X1*ROTM(2) + Y1*ROTM(1) + Z1*ROTM(3) + XC
        Y( I) = X1*ROTM(8) + Y1*ROTM(7) + Z1*ROTM(9) + YC
        Z( I) = (X1*ROTM(5) + Y1*ROTM(4) + Z1*ROTM(6))*SGN +ZC
  250   CONTINUE
C
      ELSE IF( IC(LQCLGA + IGSHAP) .EQ. ICHAR43 ) THEN      ! CC modules
        NS = 12              ! 12 side
        NC = 8               ! 8 corner
        DO 260 I = 0, 4, 4
        HI = C(LQCSHA + IGPAR5)
        BL = C(LQCSHA + IGPAR1)
        TL = C(LQCSHA + IGPAR2)
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
        DO 270 J = 1, 4
        DZ = C(LQCSHA + IGPAR3)
        IF (J.GE.3) DZ=C(LQCSHA + IGPAR4)
  270   Z(I+J) = (-1.)**(I/4+1)*DZ
  260   CONTINUE
C
        CALL PAXROT(C(LQCLGA+IGOMGE),ROTM)     ! get rotation matrix
                                        ! from angles
        DO 280 I = 1, 8
        X1 = X(I)
        Y1 = Y(I)
        Z1 = Z(I)
        X( I) = X1*ROTM(2) + Y1*ROTM(1) + Z1*ROTM(3) + XC
        Y( I) = X1*ROTM(8) + Y1*ROTM(7) + Z1*ROTM(9) + YC
        Z( I) = (X1*ROTM(5) + Y1*ROTM(4) + Z1*ROTM(6))*SGN +ZC
  280   CONTINUE
C
      ELSE IF( IC( LQCLGA + IGSHAP) .EQ. ICHAR44) THEN      ! EE and IH,IC
        NS = 4
        NC = 4
C
        RI = C(LQCSHA + IGPAR1)
        RO = C(LQCSHA + IGPAR2)
C
        Y(1) = RI
        Y(2) = RO
        Y(3) = RO
        Y(4) = RI
C
        X(1) = 0.
        X(2) = 0.
        X(3) = 0.
        X(4) = 0.
C
        DO 300 J = 1, 4
  300   Z(J) = ZC + (-1.)**(J/3+1)*C(LQCSHA+IGPAR3)*SGN
  310   CONTINUE
C
        X(5) = XC
        Y(5) = YC
C
      ELSE IF(IC(LQCSHA + IGSHAP) .EQ. ICHAR45) THEN        
        SINBEG = SIN(C(LQCSHA+IGPAR1)*RAD)   ! sin of begin angle
        COSBEG = COS(C(LQCSHA+IGPAR1)*RAD)   ! cos "    "     "
        SINEND = SIN(C(LQCSHA+IGPAR2)*RAD)   ! sin of end angle
        COSEND = COS(C(LQCSHA+IGPAR2)*RAD)   ! cos  "  "    "
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
      ELSE IF(IC(LQCSHA + IGSHAP) .EQ. ICHAR46) THEN        
        SINBEG = SIN(C(LQCSHA+IGPAR1)*RAD)   ! sin of begin angle
        COSBEG = COS(C(LQCSHA+IGPAR1)*RAD)   ! cos "    "     "
        SINEND = SIN(C(LQCSHA+IGPAR2)*RAD)   ! sin of end angle
        COSEND = COS(C(LQCSHA+IGPAR2)*RAD)   ! cos  "  "    "
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
C     NOW FORM VECTORS
C
      IF(NC .EQ. 8) THEN
C
      DO 700 I = 1, 4
C
      XS(1,N1+I) = X(I)
      YS(1,N1+I) = Y(I)
      ZS(1,N1+I) = Z(I)
C
      J = MOD(2*I,5) 
      XS(2,N1+I) = X(J)
      YS(2,N1+I) = Y(J)
      ZS(2,N1+I) = Z(J)
C
      XS(1,N1+I+4) = X(I+4)
      YS(1,N1+I+4) = Y(I+4)
      ZS(1,N1+I+4) = Z(I+4)
C
      XS(2,N1+I+4) = X(J+4)
      YS(2,N1+I+4) = Y(J+4)
      ZS(2,N1+I+4) = Z(J+4)
C
      XS(1,N1+I+8) = X(I)
      YS(1,N1+I+8) = Y(I)
      ZS(1,N1+I+8) = Z(I)
C
      XS(2,N1+I+8) = X(I+4)
      YS(2,N1+I+8) = Y(I+4)
      ZS(2,N1+I+8) = Z(I+4)
C
  700 CONTINUE
C
      ELSE IF (NC .EQ. 6 .OR. NC .EQ.10) THEN    ! triangle  or pentagon
                                        ! type shape
      K = NC/2
C
      DO 720 I = 1, K
C
      XS(1,N1+I) = X(2*I-1)
      XS(2,N1+I) = X(2*I)
C
      YS(1,N1+I) = Y(2*I-1)
      YS(2,N1+I) = Y(2*I)
C
      ZS(1,N1+I) = Z(2*I-1)
      ZS(2,N1+I) = Z(2*I)
C
      J = MOD(2*I+1,NC)
C
      XS(1,N1+I+K) = X(2*I-1)
      XS(2,N1+I+K) = X(J)
C
      YS(1,N1+I+K) = Y(2*I-1)
      YS(2,N1+I+K) = Y(J)
C
      ZS(1,N1+I+K) = Z(2*I-1)
      ZS(2,N1+I+K) = Z(J)
C
      XS(1,N1+I+NC) = X(2*I)
      XS(2,N1+I+NC) = X(J+1)
C
      YS(1,N1+I+NC) = Y(2*I)
      YS(2,N1+I+NC) = Y(J+1)
C
      ZS(1,N1+I+NC) = Z(2*I)
      ZS(2,N1+I+NC) = Z(J+1)
C
  720 CONTINUE
C
      ELSE IF (NC .EQ. 4 )THEN    ! cylinder shape
C
      DO 740 I=1,NC
C
      K = MOD(I,NC)+1
C
      XS(1,I) = X(I)
      XS(2,I) = X(K)
C
      YS(1,I) = Y(I)
      YS(2,I) = Y(K)
C
      ZS(1,I) = Z(I)
      ZS(2,I) = Z(K)
C
  740 CONTINUE
C
      XS(1,5) = X(5)        ! Discrepencies for cylinders off axis.
      XS(2,5) = X(5)        ! These should be done correctly with
C                           ! survey information.
      YS(1,5) = Y(5)
      YS(2,5) = Y(5)
C
      ELSE
      END IF
C
C----------------------------------------------------------------------
  999 RETURN
      END
