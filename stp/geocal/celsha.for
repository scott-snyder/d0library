      SUBROUTINE CELSHA( IETA, IPHI, IDEPTH, IGROUP)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : To supply cell shape information
C-        of calorimeter cells for CLYR banks.
C-
C-   Inputs  :     IETA      physics variable eta
C-                 IPHI      physics variable phi
C-                 IDEPTH    physics variable layer
C-                 IGROUP    identification of "sub layer"
C-   Outputs : 
C-   Controls: 
C-   Zebra Banks Altered:    CLYR 
C-
C-   Created   22-DEC-1988   Stephen Kahn, Esq.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:CLINKS.INC'
      INCLUDE 'D0$INC:REGION.DEF'
      INCLUDE 'D0$PARAMS:CEDP.PARAMS'
      INCLUDE 'D0$PARAMS:CETA.PARAMS'
      INCLUDE 'D0$PARAMS:CLYR.PARAMS'
      INCLUDE 'D0$PARAMS:CLGA.PARAMS'
      INCLUDE 'D0$INC:CLAY.DEF'
      INCLUDE 'D0$INC:CSHA.DEF'
      INCLUDE 'D0$INC:PI.DEF'
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$INC:LCLYR.INC'
      INCLUDE 'D0$INC:RMINMAX.INC'
      INTEGER  JQCLAY, LERR
C
      REAL PSI, DTAN, DELR, DPSI, CELPSI
      REAL RMIN, RMAX, ZMIN, ZMAX, R, Z, RR
      REAL PHI, DPHI, DELZ, ETA, CYLIND, RCEN
      REAL COS30, TAN30, ZMIN1, ZMAX1, ZMIN2, ZMAX2
      INTEGER META, IETA, IDEPTH, IGROUP, IPHI, JPHI, NETA, NPHI
C
      REAL RCHAR
      CHARACTER*4 CHAR4
      EQUIVALENCE (RCHAR,CHAR4)
      DATA CHAR4/'TRAP'/
      DATA LERR / 6 /
C
      JCLGA = (IC(LQCLAY + ILIDEN)/100) * 100 + 100   ! module code
      IF (JCLGA .EQ. 6000) JCLGA = ICOCHA             ! special case
      IF( JREGN .EQ. ICCAL   .AND. IDEPTH .NE. MNLYMG)
     &  THEN                                   ! central calorimeter
        PSI = CELPSI( IETA, IDEPTH, IGROUP, IPHI)
        DELR = C(LQCLAY + ILDELR)
        RMIN = C(LQCLAY + ILRCEN) - 0.5*DELR 
        RMAX = C(LQCLAY + ILRCEN) + 0.5*DELR 
        DPSI = C(LQCLAY + ILDPHI)
        DTAN = TAN(PSI + 0.5*DPSI) - TAN(PSI - 0.5*DPSI)   ! difference
C                                        ! of tangents
        IF ( IDEPTH .EQ. LYEM3A .OR. IDEPTH .EQ. LYEM3B) THEN
          JETA = 2 * IETA - 2
        ELSE IF (IDEPTH .EQ. LYEM3C .OR. IDEPTH .EQ. LYEM3D) THEN
          JETA = 2 * IETA - 1
        ELSE
          JETA = IETA - 1
        END IF
C
        IC(LQCLYR + ICNPAR) = 11       ! 11 parameters
        C(LQCLYR + ICSHAP) = RCHAR     ! description
        C(LQCLYR + ICPAR1) = 0.5*(C(LQCLAY+ILZPD1+JETA) -
     &    C(LQCLAY+ILZPD0+JETA))       ! half length in z
        C(LQCLYR + ICPAR2) = 0.
        C(LQCLYR + ICPAR3) = 0.
        C(LQCLYR + ICPAR4) = 0.5*DELR  ! H1
        C(LQCLYR + ICPAR5) = 0.5*RMIN*DTAN       ! LB1
        C(LQCLYR + ICPAR6) = 0.5*RMAX*DTAN       ! LH1
        C(LQCLYR + ICPAR7) = PSI/RADIAN       ! TH1
        C(LQCLYR + ICPAR8) = C(LQCLYR + ICPAR4)  ! H2
        C(LQCLYR + ICPAR9) = C(LQCLYR + ICPAR5)  ! LB2
        C(LQCLYR + ICPA10) = C(LQCLYR + ICPAR6)  ! LH2
        C(LQCLYR + ICPA11) = PSI/RADIAN       ! TH2
        IF(IDEPTH .EQ. MNLYCH .AND. IETA .EQ.6 ) THEN
          IC(LQCLYR+ICNPAR) = 12
          C(LQCLYR+ICSHAP) = 4HTRD9
          NETA = IC(LQCLAY+ILNETA)
          NPHI = IC(LQCLAY+ILNPHI)
          C(LQCLYR+ICPA12) = 0.5*(  C(LQCLAY+ILZPD1+NETA+NPHI+1)
     &         - C(LQCLAY+ILZPD0+JETA))
        END IF
        CALL SBIT1(C(LQCLYR),JBLOCL)   ! transform to global
C ...   ORIENTATION OF TRAPEZOID
        C(LQCLYR + ICTHTE) = 0.
        C(LQCLYR + ICPHIE) = 0.
        C(LQCLYR + ICOMGE) = HALFPI + C(LQCLAY+ILPHIC)
C
        LQCLGA = LC(LQCLAY - IZLLGA)          ! module block
        IF( C(LQCLAY+ILPHIC) .LT. 0.) C(LQCLYR + ICOMGE) =              
     &      HALFPI + C(LQCLAY+ILPHIC) + C(LQCLGA+IGDPHI)
        JPHI = MOD( IPHI-1, IC(LQCETA+IGMPHI)) + 1
        IF( IDEPTH .LE. MXLYEM .AND. JPHI .GT. 
     +    IC(LQCETA + IGMPHI)/2) THEN
          C(LQCLYR + ICOMGE) = C(LQCLYR + ICOMGE) + C(LQCLGA + IGDPHI)
        ELSE IF( IDEPTH .GE. MNLYFH .AND. IDEPTH .LE. MXLYFH .AND.      
     &    JPHI .EQ. 1) THEN
          C(LQCLYR + ICOMGE) = C(LQCLYR + ICOMGE) - C(LQCLGA + IGDPHI)
        ELSE IF( IDEPTH .GE. MNLYCH .AND. JPHI .EQ. IC(LQCETA+IGMPHI)) 
     +    THEN
          C(LQCLYR + ICOMGE) = C(LQCLYR + ICOMGE) + C(LQCLGA + IGDPHI)
        END IF
C
      ELSE IF ( IDEPTH .EQ. MNLYMG ) THEN            
     &             ! CC/Massless Gaps 
        ETA = 0.1*IETA - 0.05
C
        JETA = (ETA - C(LQCLAY + ILETA0))/C(LQCLAY + ILDETA) + 0.01
        JQCLAY = LQCLAY
C                                        
        DELZ = C(LQCLAY + ILDELZ)
        RMIN = C(JQCLAY + ILRPD0 +  JETA)
        RMAX = C(JQCLAY + ILRPD1 +  JETA)
        PHI = ATAN2(C(LQCLYR + ICY),C(LQCLYR + ICX))
        DPHI = C(LQCLAY + ILDPHI)
C
        IC(LQCLYR + ICNPAR) = 5        ! number of parameters
        C(LQCLYR + ICSHAP) = 4HTUBS    ! description
        C(LQCLYR + ICPAR1) = RMIN
        C(LQCLYR + ICPAR2) = RMAX
        C(LQCLYR + ICPAR3) = 0.5*DELZ
        C(LQCLYR + ICPAR4) = (PHI - 0.5*DPHI)/RADIAN
        C(LQCLYR + ICPAR5) = (PHI + 0.5*DPHI)/RADIAN
        CALL SBIT1(C(LQCLYR),JBZTRN)   ! flag to transform in Z to global
C
      ELSE IF (JREGN .EQ. INEMCL .OR. JREGN .EQ. ISEMCL) THEN   ! EC/EM 
C
        IF( IDEPTH .EQ. LYEM3A .OR. IDEPTH .EQ. LYEM3B) THEN
          ETA = 0.1*IETA -0.075
        ELSE IF( IDEPTH .EQ. LYEM3C .OR. IDEPTH .EQ. LYEM3D) THEN
          ETA = 0.1*IETA -0.025
        ELSE
          ETA = 0.1*IETA - 0.05
        END IF
C
        IF(IC(LQCLAY+ILIDEN) .GE. ICEC3L .AND. IC(LQCLAY+ILIDEN) .LE.
     &    ICEC3L+2*ICLINC) ETA = ETA-0.05
C
        IF(IC(LQCLAY + ILETPH) .EQ. 2) THEN      ! two eta/phi zones
          JETA = (ETA - C(LQCLAY + ILETA0))/C(LQCLAY + ILDETA) + 0.01
          IF( JETA .GE. IC(LQCLAY+ILNETA)) THEN    ! eta in zone 2
            JQCLAY = LQCLAY + NWZONE
            JETA = IC(LQCLAY+ILNETA) + (ETA - C(JQCLAY + ILETA0))/
     &        C(JQCLAY + ILDETA) + 0.01
          END IF
        ELSE IF (IC(LQCLAY + ILETPH) .EQ. 3) THEN  ! three eta/phi zones
          JETA = (ETA - C(LQCLAY + ILETA0))/C(LQCLAY + ILDETA) + 0.01
          IF( JETA .GE. IC(LQCLAY+ILNETA)) THEN    ! eta in zone 2
            JQCLAY = LQCLAY + NWZONE
            JETA = IC(LQCLAY+ILNETA) + (ETA - C(JQCLAY + ILETA0))/
     &        C(JQCLAY + ILDETA) + 0.01 + 1      ! "+1" is patch for
                                        ! D_ETA change
          IF( JETA .GE. IC(LQCLAY+ILNETA+NWZONE) +
     &      IC(LQCLAY+ILNETA))THEN       ! eta in zone 3
            JQCLAY = LQCLAY + 2*NWZONE
            JETA = IC(LQCLAY+ILNETA) + IC(LQCLAY+ILNETA+NWZONE)
     &        + (ETA - C(JQCLAY + ILETA0))/C(JQCLAY + ILDETA) + 0.01
     +        + 1            ! "+1" is patch for D_ETA change
          END IF
          END IF          
        ELSE                                     ! error situation
          WRITE(LERR,*) 
     &      ' EC/EM CELL DOES NOT HAVE CORRECT NUMB OF ZONES :', IETA,
     &      IPHI, IDEPTH, IGROUP
        END IF
        JQCLAY = LQCLAY + (IC(LQCLAY+ILETPH) - 1) * NWZONE      !
C                                        ! correct for added zones
        DELZ = C(LQCLAY + ILDELZ)
        RMIN = C(JQCLAY + ILRPD1 +  JETA)
        RMAX = C(JQCLAY + ILRPD0 +  JETA)
        PHI = ATAN2(C(LQCLYR + ICY),C(LQCLYR + ICX))
        DPHI = C(LQCLAY + ILDPHI)
C
        IC(LQCLYR + ICNPAR) = 5        ! number of parameters
        C(LQCLYR + ICSHAP) = 4HTUBS    ! description
        C(LQCLYR + ICPAR1) = RMIN
        C(LQCLYR + ICPAR2) = RMAX
        C(LQCLYR + ICPAR3) = 0.5*DELZ
        C(LQCLYR + ICPAR4) = (PHI - 0.5*DPHI)/RADIAN
        C(LQCLYR + ICPAR5) = (PHI + 0.5*DPHI)/RADIAN
        CALL SBIT1(C(LQCLYR),JBZTRN)   ! flag to transform in Z to global
C
      ELSE IF ((JREGN .EQ. INECAL .OR. JREGN .EQ. ISECAL) .AND.
     +  (JCLGA .EQ. ICMFHA .OR. JCLGA .EQ. ICMCHA .OR. 
     +   JCLGA .EQ. ICIFHA .OR. JCLGA .EQ. ICICHA )) THEN  ! EC/IH OR MH
        ETA = 0.1*IETA - 0.05
C
        IF(IC(LQCLAY + ILETPH) .EQ. 1) THEN           ! one eta/phi zone
          JETA = (ETA - C(LQCLAY + ILETA0))/C(LQCLAY + ILDETA) + 0.01
        ELSE IF(IC(LQCLAY + ILETPH) .EQ. 2) THEN      ! two eta/phi zones
          JETA = (ETA - C(LQCLAY + ILETA0))/C(LQCLAY + ILDETA) + 0.01
          IF( JETA .GE. IC(LQCLAY+ILNETA)) THEN    ! eta in zone 2
            JQCLAY = LQCLAY + NWZONE
            JETA = IC(LQCLAY+ILNETA) + (ETA - C(JQCLAY + ILETA0))/
     &        C(JQCLAY + ILDETA) + 0.01
          END IF
        ELSE IF (IC(LQCLAY + ILETPH) .EQ. 3) THEN  ! three eta/phi zones
          JETA = (ETA - C(LQCLAY + ILETA0))/C(LQCLAY + ILDETA) + 0.01
          IF( JETA .GE. IC(LQCLAY+ILNETA)) THEN    ! eta in zone 2
            JQCLAY = LQCLAY + NWZONE
            JETA = IC(LQCLAY+ILNETA) + (ETA - C(JQCLAY + ILETA0))/
     &        C(JQCLAY + ILDETA) + 0.01
          IF( JETA .GE. IC(LQCLAY+ILNETA+NWZONE) +
     &      IC(LQCLAY+ILNETA))THEN       ! eta in zone 3
            JQCLAY = LQCLAY + 2*NWZONE
            JETA = IC(LQCLAY+ILNETA) + IC(LQCLAY+ILNETA+NWZONE)
     &        + (ETA - C(JQCLAY + ILETA0))/C(JQCLAY + ILDETA) + 0.01
          END IF
          END IF
        END IF
        JQCLAY = LQCLAY + (IC(LQCLAY+ILETPH) - 1) * NWZONE      !
C                                        ! correct for added zones
        DELZ = C(LQCLAY + ILDELZ)
C
        IF( JCLGA .EQ. ICMFHA .OR. JCLGA .EQ. ICMCHA) THEN
          CALL RMIN_MAX_MH( IETA, IPHI, RMIN, RMAX)   ! special case for
                                        ! MH because of pentagonal shape
        ELSE       ! IH or EC/EM
          RMIN = C(JQCLAY + ILRPD1 +  JETA)
          RMAX = C(JQCLAY + ILRPD0 +  JETA)
        END IF
C
        IF( RMIN .GT. RMAX) THEN
          WRITE( LERR, *) ' CELSHA -- RMIN > RMAX: ', IETA, IPHI,
     &      IDEPTH, IGROUP, RMIN, RMAX
        END IF
C
        PHI = ATAN2(C(LQCLYR + ICY),C(LQCLYR + ICX))
        DPHI = C(LQCLAY + ILDPHI)
C
        IC(LQCLYR + ICNPAR) = 5        ! number of parameters
        C(LQCLYR + ICSHAP) = 4HTUBS    ! description
        C(LQCLYR + ICPAR1) = RMIN
        C(LQCLYR + ICPAR2) = RMAX
        C(LQCLYR + ICPAR3) = 0.5*DELZ
        C(LQCLYR + ICPAR4) = (PHI - 0.5*DPHI)/RADIAN
        C(LQCLYR + ICPAR5) = (PHI + 0.5*DPHI)/RADIAN
        CALL SBIT1(C(LQCLYR),JBZTRN)   ! flag to transform in Z to global
C
      ELSE IF ((JREGN .EQ. INECAL .OR. JREGN .EQ. ISECAL) .AND. 
     +    (JCLGA .EQ. ICOCHA)) THEN    ! outer coarse hadronic
        JETA = ABS(IETA - INT(ABS(C(LQCLAY+ILETA0)/C(LQCLAY+ILDETA)))-1)
        JQCLAY = LQCLAY + (IC(LQCLAY+ILETPH) - 1) * NWZONE      !
C                                        ! correct for added zones
        CALL RMIN_MAX_OH( IETA, IPHI, RMIN, RMAX)     ! special handling
                                        ! for edges of trapezoidal shape
        IF( RMIN .GT. RMAX) THEN
          WRITE( LERR, *) ' CELSHA -- RMIN > RMAX: ', IETA, IPHI,
     &      IDEPTH, IGROUP, RMIN, RMAX
        END IF
C
        LQCSHA = LC(LQCLAY-IZLSHA)
        TAN30 = TAN(C(LQCSHA+IGPAR2)*RADIAN)
        COS30 = COS(C(LQCSHA+IGPAR2)*RADIAN)
        RCEN = CYLIND(C(LQCLAY+ILRCEN),1,IC(LQCLAY+ILCOOR))
        ZMAX1 = C(LQCLAY+ILZCEN)-(RMAX-RCEN) * TAN30 
     +    - 0.5*C(LQCLAY+ILDELZ)/COS30
        ZMIN1 = C(LQCLAY+ILZCEN)-(RMIN-RCEN) * TAN30 
     +    - 0.5*C(LQCLAY+ILDELZ)/COS30
        ZMAX2 = C(LQCLAY+ILZCEN)-(RMAX-RCEN) * TAN30 
     +    + 0.5*C(LQCLAY+ILDELZ)/COS30
        ZMIN2 = C(LQCLAY+ILZCEN)-(RMIN-RCEN) * TAN30 
     +    + 0.5*C(LQCLAY+ILDELZ)/COS30
        PHI = ATAN2(C(LQCLYR + ICY), C(LQCLYR + ICX))
        DPHI = C(LQCLAY + ILDPHI)
C
        IC(LQCLYR + ICNPAR) = 15                 ! number of parameters
        C(LQCLYR + ICSHAP) = 4HPCON              ! shape identifier
        C(LQCLYR + ICPAR1) = (PHI - 0.5*DPHI)/RADIAN       ! begin angle
        C(LQCLYR + ICPAR2) = (PHI + 0.5*DPHI)/RADIAN       ! end angle
        C(LQCLYR + ICPAR3) = 4.                  ! number of z positions
        C(LQCLYR + ICPAR4) = ZMAX1               ! Z1
        C(LQCLYR + ICPAR5) = RMAX
        C(LQCLYR + ICPAR6) = RMAX
        C(LQCLYR + ICPAR7) = ZMIN1               ! Z2
        C(LQCLYR + ICPAR8) = RMIN
        C(LQCLYR + ICPAR9) = RMAX
        C(LQCLYR + ICPA10) = ZMAX2               ! Z3
        C(LQCLYR + ICPA11) = RMIN
        C(LQCLYR + ICPA12) = RMAX
        C(LQCLYR + ICPA13) = ZMIN2               ! Z4
        C(LQCLYR + ICPA14) = RMIN
        C(LQCLYR + ICPA15) = RMIN
      END IF
C
      RETURN
      END
