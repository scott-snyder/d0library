      SUBROUTINE CELPOS( IETA, IPHI, IDEPTH, IGROUP)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : To supply position and orientation 
C-        information of calorimeter cells for CLYR banks.
C-
C-   Inputs  :     IETA      physics variable eta
C-                 IPHI      physics variable phi
C-                 IDEPTH    physics variable layer
C-                 IGROUP    identification of "sub layer"
C-   Outputs : 
C-   Controls: 
C-   Zebra Banks Altered:    CLYR 
C-
C-   Created   9-DEC-1988   Stephen Kahn, Esq.
C-   Revised   5-MAY-1989   Stephen Kahn
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:CLINKS.INC'
      INCLUDE 'D0$INC:REGION.DEF'
      INCLUDE 'D0$PARAMS:CEDP.PARAMS'
      INCLUDE 'D0$PARAMS:CETA.PARAMS'
      INCLUDE 'D0$LINKS:IZCETA.LINK'
      INCLUDE 'D0$LINKS:IZCLYR.LINK'
      INCLUDE 'D0$PARAMS:CLYR.PARAMS'
      INCLUDE 'D0$PARAMS:CLGA.PARAMS'
      INCLUDE 'D0$INC:CLAY.DEF'
      INCLUDE 'D0$INC:CSHA.DEF'
      INCLUDE 'D0$INC:PI.DEF'
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$INC:LCLYR.INC'
      INCLUDE 'D0$INC:RMINMAX.INC'
      INTEGER JQCLAY, IERR
C
      REAL PHI, DELPHI, ETA, DELETA, PHITOW, PSI
      REAL RMIN, RMAX, ZMIN, ZMAX, R, Z, RCEN, CYLIND 
      REAL A, B, RR, PSEUDO, PAX1( 3), PAX2( 3)
      INTEGER META, IETA, IDEPTH, IGROUP, IPHI, JPHI, NPHI, NETA
      LOGICAL EQ 
      EQ(A, B) = ABS(A-B) .LT. 1.0E-05
      PSEUDO(Z, R) = -ALOG(TAN(0.5*ATAN(R/(Z+0.00001))))    ! Pseudo
C                                        ! Rapidity  from R, Z
C
C     NOMINAL VALUES FOR NON EDGE CELLS
C
      JPHI = MOD( IPHI-1, IC(LQCETA+IGMPHI)) + 1
      CALL CALPHI( IPHI, IETA, PHITOW, DELPHI, IERR)
      IF ( IDEPTH .EQ. LYEM3A  ) THEN
        PHI = PHITOW - 0.25 * DELPHI
        ETA = 0.1*IETA -0.075 
      ELSE IF (IDEPTH .EQ. LYEM3B ) THEN
        PHI = PHITOW + 0.25 * DELPHI
        ETA = 0.1*IETA - 0.075 
      ELSE IF (IDEPTH .EQ. LYEM3C ) THEN
        PHI = PHITOW - 0.25 * DELPHI
        ETA = 0.1*IETA  -0.025
      ELSE IF (IDEPTH .EQ. LYEM3D ) THEN
        PHI = PHITOW + 0.25 * DELPHI
        ETA = 0.1*IETA  -0.025
      ELSE
        PHI = PHITOW
        ETA = 0.1*IETA -0.05
      END IF
C
      IF( IC(LQCLAY+ILIDEN) .GE. ICEC3L .AND. IC(LQCLAY+ILIDEN) .LE.
     &  ICEC3L+2*ICLINC ) ETA = ETA -0.05       ! correct for the fact
C                      ! that the first EC/EM3 pad is LYEM3C,3D
C
C     EDGE CELLS
C
      IF (IC(LQCLAY + ILETPH) .EQ. 1) THEN       ! one eta/phi zone
        META = IC(LQCLAY + ILNETA)       ! number of etas on plate
        JETA = (ETA - C(LQCLAY + ILETA0))/C(LQCLAY + ILDETA) + 0.01
      ELSE IF (IC(LQCLAY + ILETPH) .EQ. 2) THEN  ! two eta/phi zones
        META = IC(LQCLAY+ILNETA) + IC(LQCLAY+ILNETA+NWZONE)
        JETA = (ETA - C(LQCLAY + ILETA0))/C(LQCLAY + ILDETA) + 0.01
        IF( JETA .GE. IC(LQCLAY+ILNETA)) THEN    ! eta in zone 2
          JQCLAY = LQCLAY + NWZONE
          JETA = IC(LQCLAY+ILNETA) + (ETA - C(JQCLAY + ILETA0))/
     &      C(JQCLAY + ILDETA) + 0.01
        END IF
      ELSE IF (IC(LQCLAY + ILETPH) .EQ. 3) THEN  ! three eta/phi zones
        META = IC(LQCLAY+ILNETA) + IC(LQCLAY+ILNETA+NWZONE) +
     &    IC(LQCLAY+ILNETA+2*NWZONE)
        JETA = (ETA - C(LQCLAY + ILETA0))/C(LQCLAY + ILDETA) + 0.01
        IF( JETA .GE. IC(LQCLAY+ILNETA)) THEN    ! eta in zone 2
          JQCLAY = LQCLAY + NWZONE
          JETA = IC(LQCLAY+ILNETA) + (ETA - C(JQCLAY + ILETA0))/
     &      C(JQCLAY + ILDETA) + 0.01 + 1   ! "+1" is patch for change
                                        ! in D_ETA
          PHI = PHITOW
        IF( JETA .GE. IC(LQCLAY+ILNETA+NWZONE) +
     &    IC(LQCLAY+ILNETA))THEN       ! eta in zone 3
          JQCLAY = LQCLAY + 2*NWZONE
          JETA = IC(LQCLAY+ILNETA) + IC(LQCLAY+ILNETA+NWZONE)
     &      + (ETA - C(JQCLAY + ILETA0))/C(JQCLAY + ILDETA) + 0.01      
     &      + 1              ! "+1" is patch for change in D_ETA
        END IF
        END IF
      END IF
      JQCLAY = LQCLAY + (IC(LQCLAY+ILETPH) - 1) * NWZONE   ! correct for
C                                        ! added zones
C
        IF( IC(LQCLAY + ILPERP) .EQ. 3) THEN     ! layers in Z dirn
          JCLGA = (IC(LQCLAY + ILIDEN)/100) * 100 + 100    ! module code
          IF( JCLGA .EQ. ICMFHA .OR. JCLGA .EQ. ICMCHA) THEN    !
C                       ! special case for MH pentagonal shape
            CALL RMIN_MAX_MH( IETA, IPHI, RMIN, RMAX)
          ELSE          ! IH or EC/EM
            RMIN = C(JQCLAY + ILRPD1 + JETA)
            RMAX = C(JQCLAY + ILRPD0 + JETA)
          END IF
C
          ZMIN = C(LQCLAY + ILZCEN)
          ZMAX = ZMIN
        ELSE IF (IC(LQCLAY + ILPERP) .EQ. 4) THEN     ! layers in r dirn
          IF( IDEPTH .GE. MNLYEM .AND. IDEPTH .LE. MXLYEM .AND. IPHI
     &      .GT. IC(LQCETA+IGMPHI)/2) THEN
            PSI = PHI - C(LQCLAY+ILPHIC) - C(LQCLGA+IGDPHI)
          ELSE IF (IDEPTH .GE. MNLYCH .AND. JPHI .EQ. IC(LQCETA+IGMPHI) 
     &      ) THEN
            PSI = PHI - C(LQCLAY+ILPHIC) - C(LQCLGA+IGDPHI)
          ELSE IF (IDEPTH .GE. MNLYFH .AND. IDEPTH .LE. MXLYFH .AND.
     &      JPHI .EQ. 1) THEN
            PSI = PHI - C(LQCLAY+ILPHIC) + C(LQCLGA+IGDPHI)
          ELSE
            PSI = PHI - C(LQCLAY+ILPHIC)
          END IF
          IF( C(LQCLAY+ILPHIC) .LT. 0.) PSI = PSI - C(LQCLGA+IGDPHI)
          RMIN = C(LQCLAY + ILRCEN)/COS(PSI)
          RMAX = RMIN
          ZMIN = C(JQCLAY + ILZPD0 + JETA)
          ZMAX = C(JQCLAY + ILZPD1 + JETA)
          NETA = IC(JQCLAY+ILNETA)
          IF( IETA .EQ.NETA .AND. IDEPTH .EQ. MNLYCH) THEN
            NPHI = IC(JQCLAY+ILNPHI)
            ZMAX = 0.5*(C(JQCLAY+ILZPD1+JETA) + C(LQCLAY+ILZPD1+NETA+
     &        NPHI+1))
          END IF
        ELSE IF ( IC(LQCLAY + ILPERP) .EQ. 9) THEN    ! layers in
C                                        ! slanted dirn
          CALL RMIN_MAX_OH( IETA, IPHI, RMIN, RMAX)
          LQCSHA = LC(LQCLAY-IZLSHA)
          RCEN = CYLIND(C(LQCLAY+ILRCEN), 1, IC(LQCLAY+ILCOOR))
          ZMIN = C(LQCLAY + ILZCEN) - (RMIN -
     &      RCEN) * TAN(C(LQCSHA+IGPAR2)*RADIAN)
          ZMAX = C(LQCLAY + ILZCEN) - ( RMAX -
     &      RCEN) * TAN(C(LQCSHA+IGPAR2)*RADIAN)
        END IF
        ETA = 0.5*(PSEUDO(ZMIN, RMIN) + PSEUDO(ZMAX, RMAX))
C
C     NOW CALCULATE X, Y, Z AND STORE
C
      Z = 0.5*(ZMIN+ZMAX)
C     R = Z*TAN(2.*ATAN(EXP(-ETA)))     
      R = 0.5*(RMIN+RMAX)
      C(LQCLYR + ICX) = R*COS(PHI)
      C(LQCLYR + ICY) = R*SIN(PHI)
      C(LQCLYR + ICZ) = Z
C
C----------------------------------------------------------------------
  999 RETURN
      END
