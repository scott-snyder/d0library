      SUBROUTINE CRSCEL_EH(IETA, IPHI, IDEPTH)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : To create and fill a CLYR bank corresponding
C-                 to a single coarse representation of a cell for the
C-                 IFH, ICH, IMF, ICH where there are several sub cells.
C-
C-   Inputs  :     IETA      physics variable ETA
C-                 IPHI      physics variable PHI
C-                 IDEPTH    physics variable LAYER
C-   Outputs : 
C-   Controls: 
C-   Zebra Banks Modified:    CLYR
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
      REAL RMN(4), RMX(4), ZCN(4), HALFDZ(4), Z1, Z2, Z3, RMIN1, RMIN2
      REAL RMIN3, RMAX1, RMAX2, PHI, RMAX4, RMAX5
      REAL REGRSS, R1, R2, Z, ZINCEP, R3, R4, Z4, Z5, RMAX3, RMIN4
      REAL THETA, ETA, TAN_THETA
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
      LSCLYR = LQCLYR
C
      CALL VZERO(RMN, 4)
      CALL VZERO(RMX, 4)
      CALL VZERO(ZCN, 4)
      CALL VZERO(HALFDZ, 4)
      NSC = 0
      KQCLYR = JQCLYR
  100 IF( KQCLYR .EQ. 0) GO TO 150
        IF( IC(KQCLYR + ICELID) .NE. IC(LQCLYR + ICELID)) GO TO 130
        IF( KQCLYR .EQ. LQCLYR) GO TO 130
        IF(JBYT(IC(KQCLYR), JBSBCL, NBSBCL) .EQ. 0) GO TO 130
        CHARR='TUBS'
        IF( IC(KQCLYR + ICSHAP) .NE. ICHARR) THEN
          WRITE( LERR, 110) IC(KQCLYR + ICSHAP)
  110     FORMAT(' shape should be TUBS : ',A4)
          GO TO 130
        END IF
        NSC = NSC + 1
        RMN( NSC) = C(KQCLYR + ICPAR1)
        RMX( NSC) = C(KQCLYR + ICPAR2)
        ZCN( NSC) = C(KQCLYR + ICZ)
        HALFDZ( NSC) = C(KQCLYR + ICPAR3)
        KSCLYR = KQCLYR
  130 KQCLYR = LC(KQCLYR)
      GO TO 100
C
  150 IF( NSC .EQ. 0) THEN
        WRITE( LERR, *) ' NO SUB CELLS FOUND ', IETA, IPHI, IDEPTH
        LQCLYR = LC(LQCLYR+2)          ! change to preceeding address in
                                       ! linear bank stream
        CALL MZDROP( IXSTP, LSCLYR, ' ')
      ELSE IF (NSC .EQ. 1) THEN
        CHARR= 'CONS'
        IC(LQCLYR + ICSHAP) = ICHARR
        IC(LQCLYR + ICNPAR) = 7
        C(LQCLYR + ICPAR6) = C(KSCLYR + ICPAR4)  ! begin angle
        C(LQCLYR + ICPAR7) = C(KSCLYR + ICPAR5)  ! end angle
        IF( RMN(1) .GT. RMX(1)) THEN             ! insure proper order
          RMIN3 = RMN(1)
          RMN(1) = RMX(1)
          RMX(1) = RMIN3                         ! switch order
        END IF
        TAN_THETA = TAN(THETA(0.1*IETA))
        Z1 = ZCN(1) - HALFDZ(1)
        PHI = 0.5*(C(LQCLYR + ICPAR6) + C(LQCLYR + ICPAR7))
        RMAX1 = RMX(1)
        RMAX2 = RMX(1)
        RMIN2 = RMX(1)
        RMIN1 = RMN(1) + (Z1-ZCN(1))*TAN_THETA
        Z2 = ZCN(1) + (RMX(1)-RMN(1))/TAN_THETA
        C(LQCLYR + ICPAR1) = 0.5 * (Z2-Z1)
        C(LQCLYR + ICPAR2) = RMIN1
        C(LQCLYR + ICPAR3) = RMAX1 
        C(LQCLYR + ICPAR4) = RMIN2
        C(LQCLYR + ICPAR5) = RMAX2
        C(LQCLYR + ICZ) = 0.5 * (Z1 + Z2)
        C(LQCLYR + ICX) = 0.25 *(RMIN1 + RMAX1 + RMIN2 + RMAX2) *   
     &     COS(PHI*RAD)
        C(LQCLYR + ICY) = 0.25 * (RMIN1 + RMAX1 + RMIN2 + RMAX2) * 
     &     SIN(PHI*RAD)
      ELSE IF (NSC .EQ. 2 .AND. ((IETA.EQ.12 .AND. IDEPTH .EQ. MNLYFH)
     +     .OR. (IETA.EQ.14 .AND. IDEPTH .EQ. MXLYFH) 
     +     .OR. (IETA.GE.18 .AND. IETA.LE.20 .AND. IETA.EQ.(IDEPTH+7))))
     +     THEN              ! upper edge pentagons
        CHARR= 'PCON'
        IC(LQCLYR + ICSHAP) = ICHARR
        IC(LQCLYR + ICNPAR) = 18
        CALL MZPUSH(IXSTP, LQCLYR, 0, IC(LQCLYR+ICNPAR)-7,'I')
        C(LQCLYR+ICPAR1) = C(KSCLYR+ICPAR4)      ! beg angle
        C(LQCLYR+ICPAR2) = C(KSCLYR+ICPAR5)      ! end angle
        C(LQCLYR+ICPAR3) = 5.                    ! number z positions
        PHI = 0.5*(C(LQCLYR+ICPAR1) + C(LQCLYR+ICPAR2))
        Z3 = ZCN(2) + HALFDZ(2)
        Z1 = ZCN(1) - HALFDZ(1) 
        RMAX1 = REGRSS(Z1, ZCN(1), ZCN(2), RMX(1), RMX(2)) 
        RMIN1 = REGRSS(Z1, ZCN(1), ZCN(2), RMN(1), RMN(2))
        RMAX2 = RMX(2)
        RMIN3 = REGRSS(Z3, ZCN(2), ZCN(1), RMN(2), RMN(1)) 
        Z2 = REGRSS(RMAX2, RMX(2), RMX(1), ZCN(2), ZCN(1))
        RMIN2 = REGRSS(Z2, ZCN(2), ZCN(1), RMN(2), RMN(1)) 
        C(LQCLYR + ICPAR4) = Z1
        C(LQCLYR + ICPAR5) = RMIN1
        C(LQCLYR + ICPAR6) = RMIN1
        C(LQCLYR + ICPAR7) = Z1
        C(LQCLYR + ICPAR8) = RMIN1
        C(LQCLYR + ICPAR9) = RMAX1
        C(LQCLYR + ICPA10) = Z2 
        C(LQCLYR + ICPA11) = RMIN2
        C(LQCLYR + ICPA12) = RMAX2
        C(LQCLYR + ICPA13) = Z3
        C(LQCLYR + ICPA14) = RMIN3
        C(LQCLYR + ICPA15) = RMAX2
        C(LQCLYR + ICPA16) = Z3
        C(LQCLYR + ICPA17) = RMAX2
        C(LQCLYR + ICPA18) = RMAX2
        Z1 = 0.
        Z2 = 0.
        Z3 = 0.
        DO 160 I = 1,2
        Z1 = Z1 + ZCN(I)*(RMX(I)-RMN(I))
        Z2 = Z2 + RMX(I) - RMN(I)
        Z3 = Z3 + (RMX(I) + RMN(I))/4.
  160   CONTINUE
C
        C(LQCLYR + ICZ) = Z1/Z2
        C(LQCLYR + ICX) = Z3 * COS(PHI*RAD)
        C(LQCLYR + ICY) = Z3 * SIN(PHI*RAD)        
      ELSE IF (NSC .EQ. 2 .AND. (IETA.EQ.15 .AND. IDEPTH .EQ. MNLYCH))
     +     THEN              ! upper edge quad
        CHARR= 'PCON'
        IC(LQCLYR + ICSHAP) = ICHARR
        IC(LQCLYR + ICNPAR) = 15
        CALL MZPUSH(IXSTP, LQCLYR, 0, IC(LQCLYR+ICNPAR)-7,'I')
        C(LQCLYR+ICPAR1) = C(KSCLYR+ICPAR4)      ! beg angle
        C(LQCLYR+ICPAR2) = C(KSCLYR+ICPAR5)      ! end angle
        C(LQCLYR+ICPAR3) = 4.                    ! number z positions
        PHI = 0.5*(C(LQCLYR+ICPAR1) + C(LQCLYR+ICPAR2))
        Z1 = ZCN(1) - HALFDZ(1) 
        RMAX1 = REGRSS(Z1, ZCN(1), ZCN(2), RMX(1), RMX(2)) 
        RMIN1 = REGRSS(Z1, ZCN(1), ZCN(2), RMN(1), RMN(2))
        RMAX2 = RMX(2)
        TAN_THETA = TAN(THETA(0.1*(IETA-1)))
        Z2 = ZCN(1) + (RMX(2)-RMX(1))/TAN_THETA
        Z3 = REGRSS(RMAX2, RMN(2), RMN(1), ZCN(2), ZCN(1))
        RMIN2 = REGRSS(Z2, ZCN(2), ZCN(1), RMN(2), RMN(1)) 
        C(LQCLYR + ICPAR4) = Z1
        C(LQCLYR + ICPAR5) = RMAX1
        C(LQCLYR + ICPAR6) = RMAX1
        C(LQCLYR + ICPAR7) = Z1
        C(LQCLYR + ICPAR8) = RMIN1
        C(LQCLYR + ICPAR9) = RMAX1
        C(LQCLYR + ICPA10) = Z2 
        C(LQCLYR + ICPA11) = RMIN2
        C(LQCLYR + ICPA12) = RMAX2
        C(LQCLYR + ICPA13) = Z3
        C(LQCLYR + ICPA14) = RMAX2
        C(LQCLYR + ICPA15) = RMAX2
        Z1 = 0.
        Z2 = 0.
        Z3 = 0.
        DO 260 I = 1,2
        Z1 = Z1 + ZCN(I)*(RMX(I)-RMN(I))
        Z2 = Z2 + RMX(I) - RMN(I)
        Z3 = Z3 + (RMX(I) + RMN(I))/4.
  260   CONTINUE
C
        C(LQCLYR + ICZ) = Z1/Z2
        C(LQCLYR + ICX) = Z3 * COS(PHI*RAD)
        C(LQCLYR + ICY) = Z3 * SIN(PHI*RAD) 
      ELSE IF (NSC .EQ. 2) THEN
        CHARR= 'CONS'
        IC(LQCLYR + ICSHAP) = ICHARR
        IC(LQCLYR + ICNPAR) = 7
        C(LQCLYR + ICPAR6) = C(KSCLYR + ICPAR4)  ! begin angle
        C(LQCLYR + ICPAR7) = C(KSCLYR + ICPAR5)  ! end angle
        Z1 = ZCN(1) - HALFDZ(1)
        Z2 = ZCN(2) + HALFDZ(2)
        PHI = 0.5*(C(LQCLYR + ICPAR6) + C(LQCLYR + ICPAR7))
        RMIN1 = REGRSS(Z1, ZCN(1), ZCN(2), RMN(1), RMN(2)) 
        RMAX1 = REGRSS(Z1, ZCN(1), ZCN(2), RMX(1), RMX(2)) 
        RMIN2 = REGRSS(Z2, ZCN(2), ZCN(1), RMN(2), RMN(1)) 
        RMAX2 = REGRSS(Z2, ZCN(2), ZCN(1), RMX(2), RMX(1)) 
        IF( RMIN1 .GT. RMAX1 .AND. RMIN2 .GE. RMAX2) THEN
          RMAX3 = RMAX1
          RMAX1 = RMIN1
          RMIN1 = RMAX3
          RMAX3 = RMAX2
          RMAX2 = RMIN2
          RMIN2 = RMAX3
        END IF
        IF( RMIN2 .GT. RMAX2) THEN     ! top of IFH -- should be
                                       ! triangles
          Z2 = ZINCEP( ZCN(1), ZCN(2), ZCN(1), ZCN(2), RMX(1), RMX(2),
     +      RMN(1), RMN(2))
          RMIN2 = RMX(2)
          RMAX2 = RMIN2
        END IF
        C(LQCLYR + ICPAR1) = 0.5 * (Z2-Z1)
        C(LQCLYR + ICPAR2) = RMIN1
        C(LQCLYR + ICPAR3) = RMAX1 
        C(LQCLYR + ICPAR4) = RMIN2
        C(LQCLYR + ICPAR5) = RMAX2
        C(LQCLYR + ICZ) = 0.5 * (Z1 + Z2)
        C(LQCLYR + ICX) = 0.25 * (RMIN1+RMIN2+RMAX1+RMAX2) *
     &    COS(PHI*RAD) 
        C(LQCLYR + ICY) = 0.25 * (RMIN1+RMIN2+RMAX1+RMAX2) *
     &    SIN(PHI*RAD) 
      ELSE IF (NSC .EQ. 3 .AND. (IETA .NE. 19 .AND. IETA .NE. 22 .AND.
     &  IETA .NE. 16 .OR.  IDEPTH .LE. MXLYFH)) THEN
        CHARR= 'CONS'
        IC(LQCLYR + ICSHAP) = ICHARR
        IC(LQCLYR + ICNPAR) = 7
        C(LQCLYR + ICPAR6) = C(KSCLYR + ICPAR4)
        C(LQCLYR + ICPAR7) = C(KSCLYR + ICPAR5)
        Z1 = ZCN(1) - HALFDZ(1)
        Z2 = ZCN(3) + HALFDZ(3)
        PHI = 0.5*(C(LQCLYR + ICPAR6) + C(LQCLYR + ICPAR7))
        RMIN1 = REGRSS(Z1, ZCN(1), ZCN(3), RMN(1), RMN(3)) 
        RMAX1 = REGRSS(Z1, ZCN(1), ZCN(3), RMX(1), RMX(3)) 
        RMIN2 = REGRSS(Z2, ZCN(3), ZCN(1), RMN(3), RMN(1)) 
        RMAX2 = REGRSS(Z2, ZCN(3), ZCN(1), RMX(3), RMX(1)) 
        C(LQCLYR + ICPAR1) = 0.5*(Z2-Z1)
        C(LQCLYR + ICPAR2) = RMIN1
        C(LQCLYR + ICPAR3) = RMAX1
        C(LQCLYR + ICPAR4) = RMIN2
        C(LQCLYR + ICPAR5) = RMAX2
        C(LQCLYR + ICZ) = 0.5 * (Z1+Z2)
        C(LQCLYR + ICX) = 0.25 * (RMIN1+RMIN2+RMAX1+RMAX2) *
     &    COS(PHI*RAD) 
        C(LQCLYR + ICY) = 0.25 * (RMIN1+RMIN2+RMAX1+RMAX2) *
     &    SIN(PHI*RAD) 
      ELSE IF (NSC.EQ.3 .AND. IDEPTH .EQ. MNLYCH .AND. IETA .EQ. 19)
     &  THEN                 ! MCH special case
        CHARR= 'PCON'
        IC(LQCLYR + ICSHAP) = ICHARR
        IC(LQCLYR + ICNPAR) = 15
        CALL MZPUSH(IXSTP, LQCLYR, 0, IC(LQCLYR+ICNPAR)-7,'I')
        C(LQCLYR+ICPAR1) = C(KSCLYR+ICPAR4)      ! beg angle
        C(LQCLYR+ICPAR2) = C(KSCLYR+ICPAR5)      ! end angle
        C(LQCLYR+ICPAR3) = 4.                    ! number z positions
        PHI = 0.5*(C(LQCLYR+ICPAR1) + C(LQCLYR+ICPAR2))
        RMIN1 = RMN(1)
        Z3 = ZCN(3) + HALFDZ(3)
        Z1 = REGRSS(RMN(1), RMX(1), RMX(3), ZCN(1), ZCN(3)) 
        Z2 = REGRSS(RMN(1), RMN(2), RMN(3), ZCN(2), ZCN(3)) 
        RMAX1 = REGRSS(Z2, ZCN(1), ZCN(3), RMX(1), RMX(3)) 
        RMAX2 = REGRSS(Z3, ZCN(3), ZCN(1), RMX(3), RMX(1)) 
        RMIN2 = REGRSS(Z3, ZCN(3), ZCN(2), RMN(3), RMN(2)) 
        C(LQCLYR + ICPAR4) = Z1
        C(LQCLYR + ICPAR5) = RMIN1
        C(LQCLYR + ICPAR6) = RMIN1
        C(LQCLYR + ICPAR7) = Z2
        C(LQCLYR + ICPAR8) = RMIN1
        C(LQCLYR + ICPAR9) = RMAX1
        C(LQCLYR + ICPA10) = Z3 
        C(LQCLYR + ICPA11) = RMIN2
        C(LQCLYR + ICPA12) = RMAX2
        C(LQCLYR + ICPA13) = Z3
        C(LQCLYR + ICPA14) = RMIN2
        C(LQCLYR + ICPA15) = RMIN2
        Z1 = 0.
        Z2 = 0.
        Z3 = 0.
        DO 170 I = 1,3
        Z1 = Z1 + ZCN(I)*(RMX(I)-RMN(I))
        Z2 = Z2 + RMX(I) - RMN(I)
        Z3 = Z3 + (RMX(I) + RMN(I))/6.
  170   CONTINUE
C
        C(LQCLYR + ICZ) = Z1/Z2
        C(LQCLYR + ICX) = Z3 * COS(PHI*RAD)
        C(LQCLYR + ICY) = Z3 * SIN(PHI*RAD)        

      ELSE IF (NSC.EQ.3 .AND. IDEPTH .EQ. MNLYCH .AND. (IETA .EQ. 22 
     +     .OR. IETA .EQ. 16)) THEN    ! far corner special case --
                                       ! pentagon
        CHARR= 'PCON'
        IC(LQCLYR + ICSHAP) = ICHARR
        IC(LQCLYR + ICNPAR) = 18
        CALL MZPUSH(IXSTP, LQCLYR, 0, IC(LQCLYR+ICNPAR)-7,'I')
        C(LQCLYR+ICPAR1) = C(KSCLYR+ICPAR4)      ! beg angle
        C(LQCLYR+ICPAR2) = C(KSCLYR+ICPAR5)      ! end angle
        C(LQCLYR+ICPAR3) = 5.                    ! number z positions
        PHI = 0.5*(C(LQCLYR+ICPAR1) + C(LQCLYR+ICPAR2))
        RMIN1 = RMN(1)
        Z3 = ZCN(3) + HALFDZ(3)
        Z1 = ZCN(1) - HALFDZ(1) 
        RMAX1 = REGRSS(Z1, ZCN(1), ZCN(2), RMX(1), RMX(2)) 
        RMIN1 = REGRSS(Z1, ZCN(1), ZCN(3), RMN(1), RMN(3))
        RMAX2 = RMX(3)
        RMIN3 = REGRSS(Z3, ZCN(3), ZCN(1), RMN(3), RMN(1)) 
        Z2 = REGRSS(RMAX2, RMX(2), RMX(1), ZCN(2), ZCN(1))
        RMIN2 = REGRSS(Z2, ZCN(3), ZCN(1), RMN(3), RMN(1)) 
        C(LQCLYR + ICPAR4) = Z1
        C(LQCLYR + ICPAR5) = RMIN1
        C(LQCLYR + ICPAR6) = RMIN1
        C(LQCLYR + ICPAR7) = Z1
        C(LQCLYR + ICPAR8) = RMIN1
        C(LQCLYR + ICPAR9) = RMAX1
        C(LQCLYR + ICPA10) = Z2 
        C(LQCLYR + ICPA11) = RMIN2
        C(LQCLYR + ICPA12) = RMAX2
        C(LQCLYR + ICPA13) = Z3
        C(LQCLYR + ICPA14) = RMIN3
        C(LQCLYR + ICPA15) = RMAX2
        C(LQCLYR + ICPA16) = Z3
        C(LQCLYR + ICPA17) = RMAX2
        C(LQCLYR + ICPA18) = RMAX2
        Z1 = 0.
        Z2 = 0.
        Z3 = 0.
        DO 180 I = 1,3
        Z1 = Z1 + ZCN(I)*(RMX(I)-RMN(I))
        Z2 = Z2 + RMX(I) - RMN(I)
        Z3 = Z3 + (RMX(I) + RMN(I))/6.
  180   CONTINUE
C
        C(LQCLYR + ICZ) = Z1/Z2
        C(LQCLYR + ICX) = Z3 * COS(PHI*RAD)
        C(LQCLYR + ICY) = Z3 * SIN(PHI*RAD)
      ELSE
        WRITE( LERR, *) ' SHOULD NOT GET HERE ', IETA, IPHI, IDEPTH
        STOP 180
      END IF
C
C----------------------------------------------------------------------
  999 RETURN
      END

