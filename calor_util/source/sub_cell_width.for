      SUBROUTINE SUB_CELL_WIDTH(IETA, IPHI, ILAYER, ISUB, RC, DR, 
     +    ZC, DZ, AZI, DAZI, IERR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : TO SUPPLY THE VECTORS WIDTH AND DEPTH OF A 
C-           CALORIMETER SUB-CELL.  THE SUB-CELL IS ADDRESSED BY THE 
C-           PHYSICS VARIABLES PLUS "ISUB".  THIS ROUTINE
C-           LOOKS FOR THE 'CLYR' BANK DESCRIBING THE SUB-CELL. 
C-           IF ISUB=0, THEN ISUB=1 IS ASSUMED.  
C-
C-   Inputs  :    IETA      PHYSICS ETA NUMBER
C-                IPHI      PHYSICS PHI NUMBER
C-                ILAYER    PHYSICS LAYER NUMBER
C-                ISUB      sub-cell number
C-   Outputs :    RC        radial distance of cell center
C-                DR        full radial width of cell 
C-                ZC        longitudinal position of cell center 
C-                DZ        full longitudinal width of cell 
C-                AZI       azimuth position of cell center
C-                DAZI      full azimuthal width of cell
C-                IERR      ERROR FLAG -- 0: OK
C-                                        1: NO CLYR BANK FOR GIVEN INDICES
C-                                        6: Sub-cell .gt. 1 when only 1 is present
C-                                        7: Shape not yet coded (effects EC/OH)
C-   Controls: 
C-
C-   Created   25-MAR-1992   Stephen Kahn
C-   Updated   27-Jan-1996  sss - compile with g77.
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
      INTEGER MSIDES, MCORNS, ISUB, JSUB
      INTEGER IETA, ILAYER, IPHI, MPHI, JPHI, NSBLAY, JBYT, JSBLAY
      INTEGER JETA, KPHI, JLAYER, JERR, IERR, NS, I, J, K, NC, N1
      REAL XC, YC, ZC, SGN, COSPSI, SINPSI, X1, Y1, X2, Y2, RC, DR
      REAL TANALF, HI, TL, BL, RI, RO, DZ, TOL, AZI, DAZI
      REAL COSBEG, COSEND, SINBEG, SINEND, PHI1, PHI2
      INTEGER IUPPER(5), ILOWER(5), IMIDDLE(5)
      LOGICAL FIRST
      SAVE FIRST
      DATA FIRST /.TRUE./
      DATA TOL / 4.0 /
      DATA IUPPER / 1, 5, 2, 3, 4 /    ! renumbering for upper pentagon
      DATA ILOWER / 1, 5, 4, 2, 3 /    ! renumbering for lower pentagon
      DATA IMIDDLE / 5, 1, 2, 4, 3 /   ! renumbering for EH pentagons
C
      IERR =  1
      IF( FIRST) THEN                  ! initialize PSI
        FIRST = .FALSE.
      END IF
C
      JSUB = ISUB
      IF (JSUB .EQ. 0) JSUB = 1        ! JSUB must not be zero
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
        IF( JSUB .GT. 1) THEN            ! asked for subcell >2 when only 1 exists
          IERR = 6
          RC = 0.
          ZC = 0.
          DZ = 0.
          AZI = 0.
          DAZI = 0.
          RETURN
        END IF
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
          IF( JSBLAY .EQ. 0) GO TO 170        ! no sub-cell designation 
                                              ! CC with only one sub-cell
          IF( JSBLAY .EQ. JSUB) GO TO 170     ! matched sub-cell w/ CLYR
  130   LQCLYR = LC(LQCLYR)
        GO TO 100
C
  150   CONTINUE
        IERR = 4
        GO TO 999
      END IF
C
C     NOW WE HAVE THE APPROPRIATE 'CLYR' BANK
C
  170 CONTINUE
      IF(LQCLYR .EQ. 0) THEN
        IERR = 4
        GO TO 999 
      END IF
      IERR = 0
      XC = C( LQCLYR + ICX)           ! mean cell X
      YC = C( LQCLYR + ICY)           ! mean cell Y
      ZC = C( LQCLYR + ICZ)*SGN       ! mean cell Z
      RC = SQRT(XC*XC + YC*YC)
      AZI = ATAN2(YC, XC)
C
      IF( IC(LQCLYR + ICSHAP) .EQ. 4HTRAP .OR. IC(LQCLYR + ICSHAP) 
     +   .EQ. 4HTRD9) THEN      ! CC cells
        HI = C(LQCLYR+ICPAR4)   ! half hight of cell
        BL = C(LQCLYR+ICPAR5)   ! half base length of cell
        DR = 2.* HI
        DZ = 2.*C(LQCLYR + ICPAR1)
C&IF LINUX
C&        TANALF = TAN ( C(LQCLYR + ICPAR7) * RADIAN)
C&ELSE
        TANALF = TAND( C(LQCLYR + ICPAR7))
C&ENDIF
        X1 = XC - HI
        X2 = X1
        Y1 = YC - BL - HI*TANALF
        Y2 = YC + BL - HI*TANALF
        PHI1 = ATAN2( Y1, X1)
        PHI2 = ATAN2( Y2, X2)
        DAZI = ABS( PHI2- PHI1)
        IF( DAZI .GT. PI) DAZI = TWOPI - DAZI
        IF(IC(LQCLYR+ICSHAP) .EQ. 4HTRD9) THEN  ! last cell of Cen Cal
                                        ! Coarse Hadronic
          DZ = 2.*MAX(C(LQCLYR+ICPAR1), C(LQCLYR+ICPA12))
        END IF
C
C
      ELSE IF( IC( LQCLYR + ICSHAP) .EQ. 4HTUBS) THEN      ! EE lay 1,2
        RI = C(LQCLYR + ICPAR1)
        RO = C(LQCLYR + ICPAR2)
        DR = RO - RI
        DZ = 2.*C(LQCLYR+ICPAR3)
        DAZI = ABS(C(LQCLYR+ICPAR5) - C(LQCLYR+ICPAR4))
        IF( DAZI .GT. PI) DAZI = TWOPI - DAZI
C
C
      ELSE IF( IC( LQCLYR + ICSHAP) .EQ. 4HCONS) THEN      ! EE lay 3,4;
C                                        ! IH and MH calorimeter cells
C
        RI = 0.5*(C(LQCLYR+ICPAR2)+C(LQCLYR+ICPAR4))
        RO = 0.5*(C(LQCLYR+ICPAR3)+C(LQCLYR+ICPAR5))
        DR = RO - RI
        DZ = 2.*C(LQCLYR+ICPAR1)
        DAZI = ABS(C(LQCLYR+ICPAR7) - C(LQCLYR+ICPAR6))
        IF( DAZI .GT. PI) DAZI = TWOPI - DAZI
C
      ELSE IF(IC(LQCLYR + ICSHAP) .EQ. 4HPCON) THEN        ! OH cells
C
        IF( C(LQCLYR+ICPAR3) .EQ. 3.) THEN  ! trinagular shapes
C
          IERR = 7
          RC = 0.
          ZC = 0.
          DZ = 0.
          AZI = 0.
          DAZI = 0.
C
        ELSE IF( C(LQCLYR+ICPAR3) .EQ. 4.) THEN  ! quadrilateral like shape
C 
          IERR = 7
          RC = 0.
          ZC = 0.
          DZ = 0.
          AZI = 0.
          DAZI = 0.
C
        ELSE IF (C(LQCLYR+ICPAR3) .EQ. 5.) THEN  ! pentagonal type shape
C
          IERR = 7
          RC = 0.
          ZC = 0.
          DZ = 0.
          AZI = 0.
          DAZI = 0.
C
        END IF
      END IF
C
C----------------------------------------------------------------------
  999 RETURN
      END
