      SUBROUTINE CRSCEL_OH( IETA, IPHI, IDEPTH)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : To create and fill a CLYR bank corresponding
C-                 to a single coarse representation of a cell for the
C-                 OH calorimeter where there are several sub cells.
C-
C-   Inputs  :     IETA      physics variable ETA
C-                 IPHI      physics variable PHI
C-                 IDEPTH    physics variable LAYER
C-   Outputs : 
C-   Controls: 
C-   Zebra Banks Modified:  CLYR
C-
C-   Created   14-OCT-1989   Stephen Kahn
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:CLINKS.INC'
      INCLUDE 'D0$INC:CLAY.DEF'
      INCLUDE 'D0$PARAMS:CLYR.PARAMS'
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$INC:LCLYR.INC'
C
      REAL  Z1, Z2, Z3, RMIN1, RMIN2
      REAL RMIN3, RMAX1, RMAX2, PHI, RMAX4, RMAX5
      REAL REGRSS, R1, R2, Z, ZINCEP, R3, R4, Z4, Z5, RMAX3, RMIN4
      REAL ZC1(4), ZC2(4), ZC3(4), ZC4(4), THETA, ETA
      REAL RMN(4), RMX(4)
      INTEGER NGROUP, JBYT, NSC, LERR, I
      INTEGER IETA, IPHI, IDEPTH, LUNIT
      REAL RAD
      PARAMETER (RAD=0.017453293)
      INTEGER ICHARR
      CHARACTER*4 CHARR
      EQUIVALENCE (ICHARR,CHARR)
C
C ... REGRSS GIVES POINT ON LINE AT Z FOR A LINE DESCRIBED BY Z1,R1,Z2,R2
      REGRSS( Z, Z1, Z2, R1, R2) = R1 + (R2 - R1)*(Z - Z1)/(Z2 - Z1)
C
C ... ZINCEP GIVES Z OF POINT FOR INTERSECTION OF 2 LINES
      ZINCEP( Z1, Z2, Z3, Z4, R1, R2, R3, R4) = (R1 - R3 + 
     +  Z3*(R4-R3)/(Z4-Z3) - Z1*(R2-R1)/(Z2-Z1)) / ((R4-R3)/(Z4-Z3)
     +  - (R2-R1)/(Z2-Z1))
C
C ... THETA GIVES POLAR ANGLE CORRESPONDING TO ETA
      THETA( ETA) = 2.0 * ATAN(EXP(-ETA))
C
      DATA LUNIT / 0 /, LERR / 6 /
C
      LSCLYR = LQCLYR
C
      CALL VZERO(RMN,4)
      CALL VZERO(RMX,4)
      CALL VZERO(ZC1,4)
      CALL VZERO(ZC2,4)
      CALL VZERO(ZC3,4)
      CALL VZERO(ZC4,4)
      NSC = 0
      KQCLYR = JQCLYR
C
  200 IF(KQCLYR .EQ. 0) GO TO 250     ! loop over sub-cells and fill arrays
        IF( IC(KQCLYR + ICELID) .NE. IC(LQCLYR + ICELID)) GO TO 230
        IF( KQCLYR .EQ. LQCLYR) GO TO 230
        IF( JBYT( IC(KQCLYR), JBSBCL, NBSBCL) .EQ. 0) GO TO 230
        CHARR='PCON'
        IF( IC(KQCLYR + ICSHAP) .NE. ICHARR) THEN
          WRITE ( LERR, 210) IC(KQCLYR + ICSHAP)
  210     FORMAT(' shape should be PCON : ',A4)
          GO TO 230
        END IF
C
        NSC = NSC + 1
        RMN( NSC) = C(KQCLYR + ICPAR8)
        RMX( NSC) = C(KQCLYR + ICPAR5)
        ZC1( NSC) = C(KQCLYR + ICPAR4)
        ZC2( NSC) = C(KQCLYR + ICPAR7)
        ZC3( NSC) = C(KQCLYR + ICPA10)
        ZC4( NSC) = C(KQCLYR + ICPA13)
        KSCLYR = KQCLYR
  230 KQCLYR = LC(KQCLYR)
      GO TO 200
C
  250 CONTINUE
      C(LQCLYR + ICPAR1) = C(KSCLYR+ ICPAR1)    ! begin angle
      C(LQCLYR + ICPAR2) = C(KSCLYR+ ICPAR2)    ! end angle
      PHI = 0.5*(C(LQCLYR + ICPAR1) + C(LQCLYR + ICPAR2))
C
      IF ((IDEPTH .EQ. MNLYCH .AND.  IETA .EQ. 10 )
     +   .OR. (IDEPTH .EQ. MNLYCH+1 .AND. (IETA .EQ. 11 .OR.
     +    IETA .EQ. 12 .OR. IETA .EQ. 13)) .OR. (IDEPTH .EQ. MXLYCH 
     +    .AND.  IETA .EQ. 13))  THEN       ! normal case
        IF( NSC .NE. 4) WRITE( LERR,*) ' NSC SHOULD BE 4 ',NSC, IETA, 
     +    IDEPTH
C
      Z1 = ZINCEP( 0.5*(ZC1(1)+ZC3(1)), 0.5*(ZC1(NSC)+ZC3(NSC)), ZC1(1)
     +    , ZC2(1), RMX(1), RMX(NSC), RMX(1), RMN(1))
      Z3 = ZINCEP( 0.5*(ZC1(1)+ZC3(1)), 0.5*(ZC1(NSC)+ZC3(NSC)), 
     +    ZC3(NSC), ZC4(NSC), RMX(1), RMX(NSC), RMX(NSC), RMN(NSC))
      Z2 = ZINCEP( 0.5*(ZC1(1)+ZC3(1)), 0.5*(ZC1(NSC)+ZC3(NSC)),
     +    ZC1(1), ZC2(1), RMN(1), RMN(NSC), RMX(1), RMN(2))
      Z4 = ZINCEP( 0.5*(ZC2(1)+ZC4(1)), 0.5*(ZC2(NSC)+ZC4(NSC)),
     +    ZC3(NSC), ZC4(NSC), RMN(1), RMN(NSC), RMX(NSC), RMN(NSC))
C
      RMAX1 = REGRSS( Z1,  0.5*(ZC1(1)+ZC3(1)), 0.5*(ZC1(NSC)+ZC3(NSC))
     +     , RMX(1), RMX(NSC))
      RMAX3 = REGRSS( Z3, 0.5*(ZC1(NSC)+ZC3(NSC)),
     +    0.5*(ZC1(1)+ZC3(1)), RMX(NSC), RMX(1))
      RMAX2 = REGRSS( Z2, 0.5*(ZC1(1)+ZC3(1)),
     +    0.5*(ZC1(NSC)+ZC3(NSC)), RMX(1), RMX(NSC))
      RMIN2 = REGRSS( Z2, 0.5*(ZC2(1)+ZC4(1)),
     +    0.5*(ZC2(NSC)+ZC4(NSC)), RMN(1), RMN(NSC))
      RMIN4 = REGRSS( Z4, 0.5*(ZC2(NSC)+ZC4(NSC)),
     +    0.5*(ZC2(1)+ZC4(1)), RMN(NSC), RMN(1))
      RMIN3 = REGRSS( Z3, 0.5*(ZC2(1)+ZC4(1)),
     +    0.5*(ZC2(NSC)+ZC4(NSC)), RMN(1), RMN(NSC))
C
      CHARR= 'PCON'
      IC(LQCLYR + ICSHAP) = ICHARR
      IC(LQCLYR + ICNPAR) = 15
      CALL MZPUSH( IXSTP, LQCLYR, 0, IC(LQCLYR + ICNPAR)-7, 'I')
      C(LQCLYR + ICPAR3) = 4.
      C(LQCLYR + ICPAR4) = Z1
      C(LQCLYR + ICPAR5) = RMAX1
      C(LQCLYR + ICPAR6) = RMAX1
      C(LQCLYR + ICPAR7) = Z2
      C(LQCLYR + ICPAR8) = RMIN2
      C(LQCLYR + ICPAR9) = RMAX2
      C(LQCLYR + ICPA10) = Z3
      C(LQCLYR + ICPA11) = RMIN3
      C(LQCLYR + ICPA12) = RMAX3
      C(LQCLYR + ICPA13) = Z4
      C(LQCLYR + ICPA14) = RMIN4
      C(LQCLYR + ICPA15) = RMIN4
C
      ELSE IF ((IDEPTH .EQ. MNLYCH .AND. IETA .EQ. 12) .OR.
     +    (IDEPTH .EQ. MNLYCH+1 .AND. IETA .EQ. 13) .OR.
     +    (IDEPTH .EQ. MXLYCH .AND. IETA .EQ. 15)) THEN  ! lower triangle
        RMIN1 = RMN(1)
        Z1 = REGRSS(RMIN1, RMX(1), RMX(NSC), 0.5*(ZC1(1)+ZC3(1)),
     +       0.5*(ZC1(NSC)+ZC3(NSC)))
        Z2 = ZINCEP(0.5*(ZC1(1)+ZC3(1)), 0.5*(ZC1(NSC)+ZC3(NSC)),
     +       ZC3(NSC), ZC4(NSC), RMX(1), RMX(NSC), RMX(NSC), RMN(NSC))
        RMAX2 = REGRSS( Z2, 0.5*(ZC1(1)+ZC3(1)),
     +       0.5*(ZC1(NSC)+ZC3(NSC)), RMX(1), RMX(NSC))
        Z3 = ZC4(NSC)
C
        CHARR= 'PCON'
        IC(LQCLYR + ICSHAP) = ICHARR
        IC(LQCLYR + ICNPAR) = 12
        CALL MZPUSH( IXSTP, LQCLYR, 0, IC(LQCLYR + ICNPAR)-7, 'I')
        C(LQCLYR + ICPAR3) = 3.
        C(LQCLYR + ICPAR4) = Z1
        C(LQCLYR + ICPAR5) = RMIN1
        C(LQCLYR + ICPAR6) = RMIN1
        C(LQCLYR + ICPAR7) = Z2
        C(LQCLYR + ICPAR8) = RMIN1 
        C(LQCLYR + ICPAR9) = RMAX2
        C(LQCLYR + ICPA10) = Z3
        C(LQCLYR + ICPA11) = RMIN1
        C(LQCLYR + ICPA12) = RMIN1
C
      ELSE IF( (IDEPTH .EQ. MNLYCH .AND. IETA .EQ. 8) .OR. 
     +    (IDEPTH .EQ. MNLYCH+1 .AND. IETA .EQ. 9) .OR. 
     +    (IDEPTH .EQ. MXLYCH .AND. IETA .EQ. 11)) THEN  ! upper triangle
        RMAX1 = RMX(1)
        Z1 = ZC1(1)
        IF (NSC .EQ. 1) THEN           ! IETA=9, IDEPTH=16
          Z3 = RMAX1/TAN(THETA(0.1*IETA))
          Z2 = ZINCEP( 0.5*(ZC2(1)+ZC4(1)), Z3, ZC1(1), ZC2(1),
     +      RMN(1), RMAX1, RMX(1), RMN(1))
          RMIN2 = REGRSS( Z2, 0.5*(ZC2(1)+ZC4(1)), Z3, RMN(1), RMAX1)
        ELSE
          Z2 = ZINCEP( 0.5*(ZC2(1)+ZC4(1)), 0.5*(ZC2(NSC)+ZC4(NSC)),
     +      ZC1(1), ZC2(1), RMN(1), RMN(NSC), RMX(1), RMN(1))
          RMIN2 = REGRSS( Z2, 0.5*(ZC2(1)+ZC4(1)),
     +      0.5*(ZC2(NSC)+ZC4(NSC)), RMN(1), RMN(NSC))
          Z3 = REGRSS(RMAX1, RMN(NSC), RMN(1), 0.5*(ZC2(NSC)+ZC4(NSC)), 
     +      0.5*(ZC2(1)+ZC4(1)))
        END IF
C
        CHARR= 'PCON'
        IC(LQCLYR + ICSHAP) = ICHARR
        IC(LQCLYR + ICNPAR) = 12
        CALL MZPUSH( IXSTP, LQCLYR, 0, IC(LQCLYR+ICNPAR)-7,'I')
        C(LQCLYR + ICPAR3) = 3.
        C(LQCLYR + ICPAR4) = Z1
        C(LQCLYR + ICPAR5) = RMAX1
        C(LQCLYR + ICPAR6) = RMAX1
        C(LQCLYR + ICPAR7) = Z2
        C(LQCLYR + ICPAR8) = RMIN2
        C(LQCLYR + ICPAR9) = RMAX1
        C(LQCLYR + ICPA10) = Z3
        C(LQCLYR + ICPA11) = RMAX1
        C(LQCLYR + ICPA12) = RMAX1
C
      ELSE IF( (IDEPTH .EQ. MNLYCH .AND. IETA .EQ. 11) .OR.
     +    (IDEPTH .EQ. MNLYCH+2 .AND. IETA .EQ. 14)) THEN  ! lower pent
        Z1 = ZINCEP( 0.5*(ZC1(1)+ZC3(1)), 0.5*(ZC1(NSC)+ZC3(NSC)),
     +    ZC1(1), ZC2(1), RMX(1), RMX(NSC), RMX(1), RMN(1))
        RMIN1 = REGRSS( Z1, 0.5*(ZC1(1)+ZC3(1)),
     +    0.5*(ZC1(NSC)+ZC3(NSC)), RMX(1), RMX(NSC))
        Z2 = ZC2(1)
        RMIN2 = RMN(1)
        RMAX2 = REGRSS( Z2, 0.5*(ZC1(1)+ZC3(1)),
     +    0.5*(ZC1(NSC)+ZC3(NSC)), RMX(1), RMX(NSC))
        Z3 = REGRSS(RMN(1), RMN(3), RMN(4), 0.5*(ZC2(3)+ZC4(3)), 
     +    0.5*(ZC2(4)+ZC4(4)))
        RMAX3 = REGRSS( Z3, 0.5*(ZC1(1)+ZC3(1)),
     +    0.5*(ZC1(NSC)+ZC3(NSC)), RMX(1), RMX(NSC))
        Z4 = ZINCEP( 0.5*(ZC1(1)+ZC3(1)), 0.5*(ZC1(4)+ZC3(4)), ZC3(4),
     +    ZC4(4), RMX(1), RMX(4), RMX(4), RMN(4))
        RMIN4 = REGRSS( Z4, 0.5*(ZC2(3)+ZC4(3)),
     +    0.5*(ZC2(4)+ZC4(4)), RMN(3), RMN(4))
        RMAX4 = REGRSS( Z4, 0.5*(ZC1(1)+ZC3(1)),
     +    0.5*(ZC1(4)+ZC3(4)), RMX(1), RMX(NSC))
        Z5 = ZINCEP( 0.5*(ZC2(3)+ZC4(3)), 0.5*(ZC2(4)+ZC4(4)), ZC3(4),
     +    ZC4(4), RMN(3), RMN(4), RMX(4), RMN(4))
        RMAX5 = REGRSS( Z5, 0.5*(ZC2(3)+ZC4(3)),
     +    0.5*(ZC2(4)+ZC4(4)), RMN(3), RMN(4))
C
        CHARR= 'PCON'
        IC(LQCLYR + ICSHAP) = ICHARR
        IC(LQCLYR + ICNPAR) = 18
        CALL MZPUSH( IXSTP, LQCLYR, 0, IC(LQCLYR+ICNPAR)-7, 'I')
        C(LQCLYR + ICPAR3) = 5.
        C(LQCLYR + ICPAR4) = Z1
        C(LQCLYR + ICPAR5) = RMIN1
        C(LQCLYR + ICPAR6) = RMIN1
        C(LQCLYR + ICPAR7) = Z2
        C(LQCLYR + ICPAR8) = RMIN2
        C(LQCLYR + ICPAR9) = RMAX2
        C(LQCLYR + ICPA10) = Z3
        C(LQCLYR + ICPA11) = RMIN2
        C(LQCLYR + ICPA12) = RMAX3
        C(LQCLYR + ICPA13) = Z4
        C(LQCLYR + ICPA14) = RMIN4
        C(LQCLYR + ICPA15) = RMAX4
        C(LQCLYR + ICPA16) = Z5
        C(LQCLYR + ICPA17) = RMAX5
        C(LQCLYR + ICPA18) = RMAX5
C
      ELSE IF ((IDEPTH .EQ. MNLYCH+1 .AND. IETA .EQ. 10) .OR. 
     +   (IDEPTH .EQ. MXLYCH .AND. IETA .EQ. 12) .OR.
     +   (IDEPTH .EQ. MNLYCH .AND. IETA .EQ. 9)) THEN ! upper pentagon
        Z1 = ZINCEP(0.5*(ZC1(1)+ZC3(1)), 0.5*(ZC1(2)+ZC3(2)),
     +    ZC1(1), ZC2(1), RMX(1), RMX(2), RMX(1), RMN(1))
        RMIN1 = REGRSS(Z1, 0.5*(ZC1(1)+ZC3(1)),
     +    0.5*(ZC1(2)+ZC3(2)), RMX(1), RMX(2))
        Z2 = ZINCEP(0.5*(ZC2(1)+ZC4(1)),0.5*(ZC2(NSC)+ZC4(NSC)),
     +    ZC1(1), ZC2(1), RMN(1), RMN(NSC), RMX(1), RMN(1))
        RMIN2 = REGRSS(Z2, 0.5*(ZC2(1)+ZC4(1)),
     +    0.5*(ZC2(NSC)+ZC4(NSC)), RMN(1), RMN(NSC))
        RMAX2 = REGRSS(Z2, 0.5*(ZC1(1)+ZC3(1)),
     +    0.5*(ZC1(2)+ZC3(2)), RMX(1), RMX(2))
        RMAX3 = RMX(NSC)
        Z3 = REGRSS(RMAX3, RMX(1), RMX(2), 0.5*(ZC1(1)+ZC3(1)), 
     +    0.5*(ZC1(2)+ZC3(2)))
        RMIN3 = REGRSS(Z3, 0.5*(ZC2(1)+ZC4(1)),
     +    0.5*(ZC2(NSC)+ZC4(NSC)), RMN(1), RMN(NSC))
        Z4 = ZC3(NSC)
        RMAX4 = RMX(NSC)
        RMIN4 = REGRSS(Z4, 0.5*(ZC2(1)+ZC4(1)),
     +    0.5*(ZC2(NSC)+ZC4(NSC)), RMN(1), RMN(NSC))
        Z5 = ZINCEP( 0.5*(ZC2(1)+ZC4(1)), 0.5*(ZC2(NSC)+ZC4(NSC)),
     +    ZC3(NSC), ZC4(NSC), RMN(1), RMN(NSC), RMX(NSC), RMN(NSC))
        RMAX5 = REGRSS(Z5, 0.5*(ZC2(1)+ZC4(1)),
     +    0.5*(ZC2(NSC)+ZC4(NSC)), RMN(1), RMN(NSC))
C
        CHARR= 'PCON'
        IC(LQCLYR + ICSHAP) = ICHARR
        IC(LQCLYR + ICNPAR) = 18
        IF(Z5 .LT. Z4) IC(LQCLYR + ICNPAR) = 15
        CALL MZPUSH( IXSTP, LQCLYR, 0, IC(LQCLYR+ICNPAR)-7, 'I')
        C(LQCLYR + ICPAR3) = 5.
        C(LQCLYR + ICPAR4) = Z1
        C(LQCLYR + ICPAR5) = RMIN1
        C(LQCLYR + ICPAR6) = RMIN1
        C(LQCLYR + ICPAR7) = Z2
        C(LQCLYR + ICPAR8) = RMIN2
        C(LQCLYR + ICPAR9) = RMAX2
        C(LQCLYR + ICPA10) = Z3
        C(LQCLYR + ICPA11) = RMIN3
        C(LQCLYR + ICPA12) = RMAX3
        IF(IC(LQCLYR +ICNPAR) .NE. 15) THEN
        C(LQCLYR + ICPA13) = Z4
        C(LQCLYR + ICPA14) = RMIN4
        C(LQCLYR + ICPA15) = RMAX4
        C(LQCLYR + ICPA16) = Z5
        C(LQCLYR + ICPA17) = RMAX5
        C(LQCLYR + ICPA18) = RMAX5
        ELSE
        C(LQCLYR + ICPAR3) = 4.
        C(LQCLYR + ICPA13) = Z5
        C(LQCLYR + ICPA14) = RMAX5
        C(LQCLYR + ICPA15) = RMAX5
        END IF
C       
      END IF
C
      Z1 = 0.
      Z2 = 0.
      Z3 = 0.
C
      DO 270 I = 1, NSC
        Z1 = Z1 + 0.25*(ZC1(I)+ZC2(I)+ZC3(I)+ZC4(I))*(RMX(I)-RMN(I))
        Z2 = Z2 + RMX(I) - RMN(I)
        Z3 = Z3 + (RMX(I) + RMN(I))/NSC
  270 CONTINUE
C
      C(LQCLYR + ICZ) = Z1/Z2
      C(LQCLYR + ICX) = Z3 * COS(PHI*RAD)
      C(LQCLYR + ICY) = Z3 * SIN(PHI*RAD)
C
C----------------------------------------------------------------------
  999 RETURN
      END

