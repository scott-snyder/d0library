      SUBROUTINE CELVEC(IETA, IPHI, ILAYER, XS, YS, ZS, NS, IERR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : TO SUPPLY THE VECTORS THAT REPRESENT THE 
C-           SIDES OF A COARSE DESCRIPTION OF A CELL.  XS, YS, AND
C-           ZS ARE (2, NS) ARRAYS GIVING THE BEGINNING AND END POINTS
C-           OF LINES REPRESENTING THE SIDES OF A CELL.  THE CELL 
C-           IS ADDRESSED BY THE PHYSICS VARIABLES.  THIS ROUTINE
C-           LOOKS FOR THE 'CLYR' BANK DESCRIBING THE COARSE DESCRIPTION.
C-           SUB-CELL BANKS ARE IGNORED.  
C-
C-   Inputs  :    IETA      PHYSICS ETA NUMBER
C-                IPHI      PHYSICS PHI NUMBER
C-                ILAYER    PHYSICS LAYER NUMBER
C-   Outputs :    XS        X COORDINATE SIDE VECTOR
C-                YS        Y COORDINATE SIDE VECTOR 
C-                ZS        Z COORDINATE SIDE VECTOR
C-                NS        NUMBER OF SIDES OF THE CELL
C-                IERR      ERROR FLAG -- 0: OK
C-                                        1: NO CLYR BANK FOR GIVEN INDICES
C-   Controls: 
C-
C-   Created   18-APR-1989   Stephen Kahn
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:CLINKS.INC'
      INCLUDE 'D0$INC:SINTBL.INC'
      INCLUDE 'D0$INC:PI.DEF'
      INCLUDE 'D0$LINKS:IZCEDP.LINK'
      INCLUDE 'D0$PARAMS:CEDP.PARAMS'
      INCLUDE 'D0$PARAMS:CETA.PARAMS'
      INCLUDE 'D0$LINKS:IZCETA.LINK'
      INCLUDE 'D0$LINKS:IZCLYR.LINK'
      INCLUDE 'D0$LINKS:IZSCLR.LINK'
      INCLUDE 'D0$PARAMS:CLYR.PARAMS'
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
C
      INTEGER MSIDES, MCORNS
      PARAMETER (MSIDES = 32)          ! maximum number of sides
      PARAMETER (MCORNS = 20)          ! maximum number of corners
      INTEGER IETA, ILAYER, IPHI, MPHI, JPHI, NSBLAY, JBYT, JSBLAY
      INTEGER JETA, KPHI, JLAYER, JERR, IERR, NS, I, J, K, NC, N1
      REAL XC, YC, ZC, SGN, COSPSI, SINPSI, X1
      REAL PHI, TANALF, HI, TL, BL, RI, RO, DZ, TOL
      REAL COSBEG, COSEND, SINBEG, SINEND
      REAL X(MCORNS), Y(MCORNS), Z(MCORNS)
      REAL XS(2,MSIDES), YS(2,MSIDES), ZS(2,MSIDES) 
      INTEGER IUPPER(5), ILOWER(5), IMIDDLE(5)
      LOGICAL FIRST
      SAVE FIRST
      CHARACTER*4 CHAR41,CHAR42,CHAR43,CHAR44,CHAR45
      INTEGER ICHAR41,ICHAR42,ICHAR43,ICHAR44,ICHAR45
      EQUIVALENCE (ICHAR41,CHAR41)
      EQUIVALENCE (ICHAR42,CHAR42)
      EQUIVALENCE (ICHAR43,CHAR43)
      EQUIVALENCE (ICHAR44,CHAR44)
      EQUIVALENCE (ICHAR45,CHAR45)
      DATA     CHAR41 /'TRAP'/
      DATA     CHAR42 /'TRD9'/
      DATA     CHAR43 /'TUBS'/
      DATA     CHAR44 /'CONS'/
      DATA     CHAR45 /'PCON'/
      DATA FIRST /.TRUE./
      DATA TOL / 4.0 /
      DATA IUPPER / 1, 5, 2, 3, 4 /    ! renumbering for upper pentagon
      DATA ILOWER / 1, 5, 4, 2, 3 /    ! renumbering for lower pentagon
      DATA IMIDDLE / 5, 1, 2, 4, 3 /   ! renumbering for EH pentagons
C
      IERR =  1
      IF( FIRST) THEN                  ! initialize PSI
        CALL INIT_SINTBL( JERR)
        IF( JERR .NE. 0) THEN          ! error from INIT_SINTBL 
          IERR = 2                       ! should not occur
          RETURN
        END IF
        FIRST = .FALSE.
      END IF
C
      JETA = ABS(IETA)                 ! magnitude of eta
      SGN = SIGN(1, IETA)              ! sign of eta
      IF( SGN.LT. 0. .AND. ILAYER .GE. LYEM3A .AND. ILAYER .LE. LYEM3D
     &  .AND. JETA .LE. 26)THEN
        JLAYER = MOD(ILAYER-1,4) + 3   ! symmetry for negative FLOOR 3
      ELSE
        JLAYER = ILAYER
      END IF
C
      LQCEDP = LC(LCGEH - IZCEDP)      ! pointer to tower dispatching
C                                      ! bank
      LQCETA = LC(LQCEDP - IZCETA - JETA + 1)    ! pointer to constant
C                                        ! eta bank
      LQCLYR = LC(LQCETA - IZCLYR - JLAYER + 1)  ! pointer to first
C
      IF(LQCEDP .EQ. 0 .OR. LQCETA .EQ. 0 .OR. LQCLYR .EQ.0) GO TO 999
      IERR = 3
C                                        ! appropriate layer bank
      MPHI = IC(LQCETA + IGMPHI)         ! number of phi's present for
C                                        ! this eta
      JPHI = MOD( IPHI-1, MPHI) + 1      ! index to representative phi
      NSBLAY = JBYT( C(LQCLYR), JBNSBC, NBNSBC)  ! number of sublayers
C
      N1 = 0
      IF( NSBLAY .LE. 1) THEN
   30   IF( LQCLYR .EQ. 0) GO TO 999
          KPHI = JBYT( C(LQCLYR+ICELID), JBIPHI, NBIPHI)
          IF (KPHI .NE. JPHI) GO TO 50
          GO TO 170
   50   LQCLYR = LC(LQCLYR)
        GO TO 30
      ELSE
  100   IF(LQCLYR .EQ. 0) GO TO 150
          KPHI = JBYT( C(LQCLYR+ICELID), JBIPHI, NBIPHI)
          IF (KPHI .NE. JPHI) GO TO 130
          JSBLAY = JBYT( C(LQCLYR), JBSBCL, NBSBCL)
          IF( JSBLAY .EQ. 0) GO TO 170
  130   LQCLYR = LC(LQCLYR)
        GO TO 100
      END IF
C
C     NOW WE HAVE THE APPROPRIATE 'CLYR' BANK
C
  170 CONTINUE
      IF(LQCLYR .EQ. 0) GO TO 150
      IERR = 0
      XC = C( LQCLYR + ICX)           ! mean cell X
      YC = C( LQCLYR + ICY)           ! mean cell Y
      ZC = C( LQCLYR + ICZ)*SGN       ! mean cell Z
C
      IF( IC(LQCLYR + ICSHAP) .EQ. ICHAR41 .OR. IC(LQCLYR + ICSHAP) 
     +   .EQ. ICHAR42 ) THEN      ! CC cells
        NS = 12              ! 12 side
        NC = 8               ! 8 corner
        DO 210 I = 0, 4, 4
C&IF IBMAIX
C&        TANALF = TAN( C( LQCLYR + ICPAR7 + I)*RADIAN)     ! tan(alpha_i)
C&ELSE
        TANALF = TAND( C( LQCLYR + ICPAR7 + I))     ! tan(alpha_i)
C&ENDIF
        HI = C(LQCLYR + ICPAR4 + I)
        BL = C(LQCLYR + ICPAR5 + I)
        TL = C(LQCLYR + ICPAR6 + I)
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
  200   Z(I+J) = (-1.)**(I/4+1)*C(LQCLYR + ICPAR1)
  210   CONTINUE
        IF(IC(LQCLYR+ICSHAP) .EQ. ICHAR42) THEN  ! last cell of Cen Cal
                                        ! Coarse Hadronic
          DO 230 J = 1, 4
          Z(J) = -0.5*(C(LQCLYR+ICPAR1)+C(LQCLYR+ICPA12))
          IF( J.LE. 2) THEN
            Z(J+4) = Z(J) + 2.*C(LQCLYR+ICPAR1)
          ELSE
            Z(J+4) = Z(J) + 2.*C(LQCLYR+ICPA12)
          END IF
  230     CONTINUE
        END IF
C
        COSPSI = SIN(C(LQCLYR + ICOMGE))    ! PSI = HALFPI - OMEGA
        SINPSI = -COS(C(LQCLYR + ICOMGE)) 
        DO 220 I = 1, 8
        X1 = X(I)
        X( I) = X1*COSPSI - Y(I)*SINPSI + XC
        Y( I) = X1*SINPSI + Y(I)*COSPSI + YC
        Z( I) = SGN*Z( I) + ZC
  220   CONTINUE
C
      ELSE IF( IC( LQCLYR + ICSHAP) .EQ. ICHAR43) THEN      ! EE lay 1,2
        NS = 12
        NC = 8
C
        DO 310 I = 0, 4, 4
C&IF IBMAIX
C&        SINBEG = SIN(C(LQCLYR + ICPAR4)*RADIAN)
C&        COSBEG = COS(C(LQCLYR + ICPAR4)*RADIAN)
C&        SINEND = SIN(C(LQCLYR + ICPAR5)*RADIAN)
C&        COSEND = COS(C(LQCLYR + ICPAR5)*RADIAN)
C&ELSE
        SINBEG = SIND(C(LQCLYR + ICPAR4))
        COSBEG = COSD(C(LQCLYR + ICPAR4))
        SINEND = SIND(C(LQCLYR + ICPAR5))
        COSEND = COSD(C(LQCLYR + ICPAR5))
C&ENDIF
        RI = C(LQCLYR + ICPAR1)
        RO = C(LQCLYR + ICPAR2)
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
  300   Z(I+J) = ZC + (-1.)**(I/4+1)*C(LQCLYR+ICPAR3)*SGN
  310   CONTINUE
C
      ELSE IF( IC( LQCLYR + ICSHAP) .EQ. ICHAR44) THEN      ! EE lay 3,4;
C                                        ! IH and MH calorimeter cells
        NS = 12
        NC = 8
C
        DO 410 I = 0, 4, 4
C&IF IBMAIX
C&        SINBEG = SIN(C(LQCLYR + ICPAR6)*RADIAN)
C&        COSBEG = COS(C(LQCLYR + ICPAR6)*RADIAN)
C&        SINEND = SIN(C(LQCLYR + ICPAR7)*RADIAN)
C&        COSEND = COS(C(LQCLYR + ICPAR7)*RADIAN)
C&ELSE
        SINBEG = SIND(C(LQCLYR + ICPAR6))
        COSBEG = COSD(C(LQCLYR + ICPAR6))
        SINEND = SIND(C(LQCLYR + ICPAR7))
        COSEND = COSD(C(LQCLYR + ICPAR7))
C&ENDIF
        RI = C(LQCLYR + ICPAR2 + I/2)
        RO = C(LQCLYR + ICPAR3 + I/2)
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
  400   Z(I+J) = ZC + (-1.)**(I/4+1)*C(LQCLYR+ICPAR1)*SGN
  410   CONTINUE
C
      ELSE IF(IC(LQCLYR + ICSHAP) .EQ. ICHAR45) THEN        ! OH cells
C&IF IBMAIX
C&        SINBEG = SIN(C(LQCLYR+ICPAR1)*RADIAN)   ! sin of begin angle
C&        COSBEG = COS(C(LQCLYR+ICPAR1)*RADIAN)   ! cos "    "     "
C&        SINEND = SIN(C(LQCLYR+ICPAR2)*RADIAN)   ! sin of end angle
C&        COSEND = COS(C(LQCLYR+ICPAR2)*RADIAN)   ! cos  "  "    "
C&ELSE
        SINBEG = SIND(C(LQCLYR+ICPAR1))   ! sin of begin angle
        COSBEG = COSD(C(LQCLYR+ICPAR1))   ! cos "    "     "
        SINEND = SIND(C(LQCLYR+ICPAR2))   ! sin of end angle
        COSEND = COSD(C(LQCLYR+ICPAR2))   ! cos  "  "    "
C&ENDIF
C
        IF( C(LQCLYR+ICPAR3) .EQ. 3.) THEN  ! trinagular shapes
C
        NC = 6
        NS = 9
C
        DO 500 I = 0, 4, 2
        J = I/2
        RI = C(LQCLYR + ICPAR5 + 3*J)
        IF( I.EQ.2 .AND. RI.EQ.C(LQCLYR+ICPAR5)) RI = C(LQCLYR+ICPAR9)
C
        X(I+1) = RI*COSBEG
        X(I+2) = RI*COSEND
C
        Y(I+1) = RI*SINBEG
        Y(I+2) = RI*SINEND
C
        Z(I+1) = C(LQCLYR+ICPAR4+ 3*J)*SGN
        Z(I+2) = Z(I+1)
  500   CONTINUE
C
        ELSE IF( C(LQCLYR+ICPAR3) .EQ. 4.) THEN  ! quadrilateral like shape
C 
        NC = 8                           ! number of corners
        NS = 12                          ! number of edges
C
        DO 520 I = 0, 6, 2
        J = I/2
        RI = C(LQCLYR + ICPAR5 + 3*J)
        IF ( I.EQ.4) RI = C(LQCLYR+ICPA12)
C
        X(I+1) = RI*COSBEG
        X(I+2) = RI*COSEND
C
        Y(I+1) = RI*SINBEG
        Y(I+2) = RI*SINEND
C
        Z(I+1) = C(LQCLYR+ICPAR4+3*J)*SGN
        Z(I+2) = Z(I+1)
  520 CONTINUE
C
        ELSE IF (C(LQCLYR+ICPAR3) .EQ. 5.) THEN  ! pentagonal type shape
C
        NC = 10
        NS = 15
C
        DO 540 I = 0, 8, 2
        J = I/2
        IF( C(LQCLYR+ICPAR4) .EQ. C(LQCLYR+ICPAR7)) THEN   ! EH
                                        ! pentagons
          RI = C(LQCLYR + ICPAR6 + 3*J)     ! Rmin
          IF( I.EQ. 0 .OR. I.EQ.6 ) RI = C(LQCLYR + ICPAR5 + 3*J)     !
                                        ! Rmax for 1st and 4th point
          K = 2 * (IMIDDLE(J+1) - 1)
        ELSE IF( ABS(C(LQCLYR+ICPA12) - C(LQCLYR+ICPA15)) .GT. TOL) THEN
C                           !lower  pentagon
          RI = C(LQCLYR + ICPAR5 + 3*J)     ! Rmax
          IF( I.EQ. 6) RI = C(LQCLYR + ICPA15)   ! Rmin for 4th point
          K = 2 * (ILOWER(J+1) - 1)
        ELSE
          RI = C(LQCLYR + ICPAR6 + 3*J)
          IF( I.EQ.2 ) RI = C(LQCLYR+ICPAR8)     ! Rmin for 2nd point
          K = 2 * (IUPPER(J+1) - 1)
        END IF
C
        X(K+1) = RI*COSBEG
        X(K+2) = RI*COSEND
C
        Y(K+1) = RI*SINBEG
        Y(K+2) = RI*SINEND
C
        Z(K+1) = C(LQCLYR+ICPAR4+ 3*J)*SGN
        Z(K+2) = Z(K+1)
  540   CONTINUE
C
        END IF
      END IF
C
C     ROTATE TO PROPER IPHI
C
      MPHI = IC(LQCETA + IGMPHI) 
      KPHI = ((IPHI - 1)/MPHI) * MPHI    ! rotation by KPHI*PSI
C                                        ! needs to be done
      IF (KPHI .NE. 0) THEN              ! no rotation necessary
      DO 600 I = 1, NC
      X1 = X(I)
      X(I) = X1 * COSROT(KPHI) - Y(I) * SINROT(KPHI)
      Y(I) = X1 * SINROT(KPHI) + Y(I) * COSROT(KPHI)
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
      ELSE
      END IF
C
      LQCLYR = LC(LQCLYR-IZSCLR)
      IF( LQCLYR .NE. 0 ) THEN
        N1 = N1 + NS
        GO TO 170
      END IF
C
      IF( N1 .GE. 0) NS = NS + N1 
C----------------------------------------------------------------------
C
      GO TO 999
  150 CONTINUE
      IERR = 4
  999 RETURN
      END
