      LOGICAL FUNCTION CINCEL(XS, YS, ZS, IETA, IPHI, ILAYER, IERR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : TO DETERMINE WHETHER POINT IN SPACE IS
C-           INSIDE A CELL.  THE SUBROUTINE LOOPS THROUGH SUB-CELLS
C-           IN THE END CAP REGION RETURNING A .TRUE. IF IT IS IN 
C-           ANY OF THE SUB-CELLS.  
C-
C-       
C-   Inputs  :    XS        X COORDINATE SIDE VECTOR
C-                YS        Y COORDINATE SIDE VECTOR 
C-                ZS        Z COORDINATE SIDE VECTOR
C-                IETA      PHYSICS ETA NUMBER
C-                IPHI      PHYSICS PHI NUMBER
C-                ILAYER    PHYSICS LAYER NUMBER
C-   Outputs :    CINCEL    .TRUE. IF INSIDE CELL
C-                IERR      ERROR FLAG -- 0: OK
C-                                        1: NO CLYR BANK FOR GIVEN INDICES
C-   Controls: 
C-
C-   Created    8-JUN-1989   Stephen Kahn
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:CLINKS.INC'
      INCLUDE 'D0$LINKS:IZCEDP.LINK'
      INCLUDE 'D0$PARAMS:CEDP.PARAMS'
      INCLUDE 'D0$PARAMS:CETA.PARAMS'
      INCLUDE 'D0$LINKS:IZCETA.LINK'
      INCLUDE 'D0$LINKS:IZCLYR.LINK'
      INCLUDE 'D0$PARAMS:CLYR.PARAMS'
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$INC:PI.DEF'
C
      INTEGER IETA, ILAYER, IPHI, MPHI, JPHI, NSBLAY, JBYT, JSBLAY
      INTEGER JETA, KPHI, JLAYER, JERR, IERR, I, J, K, LERR
      INTEGER NZ, JQCLYR
      REAL XC, YC, ZC, SGN, PSI, COSPSI, SINPSI, X1
      REAL PHI, TANALF, HI, TL, BL, RI, RO, DZ
      REAL COSBEG, COSEND, SINBEG, SINEND, ANGLE, R, RMIN, RMAX
      REAL X, Y, Z, XE, YE, XS, YS, ZS
      LOGICAL FIRST
      SAVE FIRST
      CHARACTER*4 CHAR41,CHAR42,CHAR43,CHAR44
      INTEGER ICHAR41,ICHAR42,ICHAR43,ICHAR44
      EQUIVALENCE (ICHAR41,CHAR41)
      EQUIVALENCE (ICHAR42,CHAR42)
      EQUIVALENCE (ICHAR43,CHAR43)
      EQUIVALENCE (ICHAR44,CHAR44)
      DATA     CHAR41 /'TRAP'/
      DATA     CHAR42 /'TUBS'/
      DATA     CHAR43 /'CONS'/
      DATA     CHAR44 /'PCON'/
      DATA FIRST /.TRUE./
      DATA LERR / 0 /
C
      CINCEL = .FALSE.
      IERR =  1
      IF( FIRST) THEN                  ! initialize PSI
        CALL CALPHI( 1, 1, PHI, PSI, JERR)
        IF( JERR .NE. 0) THEN          ! error from CALPHI 
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
      IF( NSBLAY .LE. 1) THEN
   30   IF( LQCLYR .EQ. 0) GO TO 999
        KPHI = JBYT( C(LQCLYR+ICELID), JBIPHI, NBIPHI)
        IF (KPHI .NE. JPHI) GO TO 50
        GO TO 170
   50   LQCLYR = LC(LQCLYR)
        GO TO 30
      ENDIF
  100 IF(LQCLYR .EQ. 0) GO TO 150
      KPHI = JBYT( C(LQCLYR+ICELID), JBIPHI, NBIPHI)
      IF (KPHI .NE. JPHI) GO TO 130
      JSBLAY = JBYT( C(LQCLYR), JBSBCL, NBSBCL)
      IF( JSBLAY .NE. 0) GO TO 170
  130 LQCLYR = LC(LQCLYR)
      GO TO 100
C
  150 CONTINUE
      IERR = 4
      GO TO 999
C
C     PUT X, Y INTO FRAME OF CLYR (FIRST MPHI PHI CELLS)
C
  170 CONTINUE
      KPHI = ((IPHI - 1)/MPHI) * MPHI       ! rotation by -KPHI*PSI is
C                                        ! necessary
      IF( KPHI .EQ. 0) THEN       ! no rotation necessary
        X = XS
        Y = YS
        Z = ZS
      ELSE
        SINPSI = SIN(KPHI*PSI)
        COSPSI = COS(KPHI*PSI)
        X = XS*COSPSI + YS*SINPSI           ! perform rotation
        Y = -XS*SINPSI + YS*COSPSI
        Z = ZS
      END IF
C
C     NOW WE HAVE THE APPROPRIATE 'CLYR' BANK
C
      IERR = 0
      XC = C( LQCLYR + ICX)           ! mean cell X
      YC = C( LQCLYR + ICY)           ! mean cell Y
      ZC = C( LQCLYR + ICZ)*SGN       ! mean cell Z
C
      IF( IC(LQCLYR + ICSHAP) .EQ. ICHAR41 ) THEN      ! CC cells
        IF( ABS(Z-ZC) .GT. C(LQCLYR+ICPAR1)) GO TO 900     ! failed Z
C&IF IBMAIX
C&        TANALF = TAN( C( LQCLYR + ICPAR7)*RADIAN)     ! tan(alpha_i)
C&ELSE
        TANALF = TAND( C( LQCLYR + ICPAR7))     ! tan(alpha_i)
C&ENDIF
        HI = C(LQCLYR + ICPAR4)
        BL = C(LQCLYR + ICPAR5)
        TL = C(LQCLYR + ICPAR6)
C
        COSPSI = SIN(C(LQCLYR + ICOMGE))    ! PSI = HALFPI - OMEGA
        SINPSI = -COS(C(LQCLYR + ICOMGE)) 
C
        XE = (X-XC)*COSPSI + (Y-YC)*SINPSI
        YE = -(X-XC)*SINPSI + (Y-YC)*COSPSI
C
        IF( ABS(XE) .GT. HI) GO TO 900      ! failed X
C
        IF( YE .LT. 0.5*((HI-XE)*(-BL-HI*TANALF) +
     &    (XE+HI)*(-TL+HI*TANALF)) .OR. YE .GT.
     &    0.5*((HI-XE)*(BL-HI*TANALF) + (XE+HI)*(TL+HI*TANALF))) GO TO
     &    900      ! failed Y
C
      ELSE IF( IC( LQCLYR + ICSHAP) .EQ. ICHAR42) THEN      ! EE, MH, IH 
        IF( ABS(Z-ZC) .GT. C(LQCLYR+ICPAR3)) GO TO 900     ! failed Z
C
        RI = C(LQCLYR + ICPAR1)
        RO = C(LQCLYR + ICPAR2)
        IF( RI.LE.RO ) THEN       ! normal order of RMIN and RMAX
          RMIN = RI
          RMAX = RO
        ELSE                      ! flipped order of RMIN and RMAX
          RMIN = RO
          RMAX = RI
        END IF
C
        IF((X**2 + Y**2).LT.RMIN**2 .OR. (X**2 + Y**2).GT.RMAX**2) 
     +     GO TO 900                                       ! failed R
        ANGLE = ATAN2(Y,X)/RADIAN
C
        IF( ANGLE .LT. C( LQCLYR+ICPAR4) .OR. ANGLE .GT.
     &    C(LQCLYR+ICPAR5)) GO TO 900       ! failed angle
C
      ELSE IF( IC( LQCLYR + ICSHAP) .EQ. ICHAR43) THEN      
        IF(LERR .NE. 0) WRITE( LERR,*) ' CINCEL : CONS not used ',  
     &      IETA, IPHI, ILAYER
        IERR = 5
        CINCEL = .FALSE.  
        RETURN
C
      ELSE IF(IC(LQCLYR + ICSHAP) .EQ. ICHAR44) THEN        ! OH cells
        ANGLE = ATAN2(Y, X)/RADIAN
        IF( ANGLE .LT. C(LQCLYR+ICPAR1) .OR. ANGLE .GE. C(LQCLYR+ICPAR2)
     &    ) GO TO 900               ! failed angle range
        NZ = C(LQCLYR + ICPAR3)
        R = SQRT( X**2 + Y**2)
C
        DO 210 I = 1, NZ-1
          JQCLYR = LQCLYR + 3*I - 3
          IF( Z .LT.C(JQCLYR+ICPAR4) .OR. Z .GE. C(JQCLYR+ICPAR7)) 
     &      GO TO 210
          RMIN = C(JQCLYR+ICPAR5) + (Z-C(JQCLYR+ICPAR4)) *
     &      (C(JQCLYR+ICPAR8)-C(JQCLYR+ICPAR5))/(C(JQCLYR+ICPAR7) -
     &      C(JQCLYR+ICPAR4))
          RMAX = C(JQCLYR+ICPAR6) + (Z-C(JQCLYR+ICPAR4)) *
     &      (C(JQCLYR+ICPAR9)-C(JQCLYR+ICPAR6))/(C(JQCLYR+ICPAR7) -     
     &      C(JQCLYR+ICPAR4))
          IF(R .LT. RMIN .OR. R .GE. RMAX) GO TO 210
          GO TO 220
  210   CONTINUE
        GO TO 900               ! not inside any Z band
  220   CONTINUE                ! inside the cell
      END IF
      CINCEL = .TRUE.
      RETURN
C
  900 CONTINUE
C
      LQCLYR = LC(LQCLYR)
      GO TO 100
C----------------------------------------------------------------------
  999 RETURN
      END
