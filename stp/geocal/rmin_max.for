      SUBROUTINE RMIN_MAX( IETA, IPHI, RMIN, RMAX)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : To return the minimun and maximum R
C-       for a cell in the EC/MH calorimeter.  It accounts for the
C-       pentagonal edge by supplying an average radius on the edge.
C-       For cells that are not on the edge it passes the RMIN and
C-       Rmax from the CLAY bank.  Alternate entry RMIN_MAX_OH does
C-       the same for the EC/OH.
C-
C-   Inputs  :     IETA      Physics ETA
C-                 IPHI      Physics PHI
C-   Outputs :     RMIN      Minimun edge radius of cell
C-                 RMAX      Maximum edge radius of cell
C-   Controls:     JETA      Difference betw IETA and IETA0 (first eta 
C-                             on plate)
C-                 JCLGA     Region identification of module
C-                 LQCLAY    pointer to CLAY bank
C-
C-   Entry points: RMIN_MAX_MH
C-                 RMIN_MAX_OH
C-
C-   Created  28-OCT-1989   Stephen Kahn
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:CLINKS.INC'
      INCLUDE 'D0$INC:REGION.DEF'
      INCLUDE 'D0$PARAMS:CLGA.PARAMS'
      INCLUDE 'D0$INC:CSHA.DEF'
      INCLUDE 'D0$INC:CLAY.DEF'
      INCLUDE 'D0$LINKS:IZCLGA.LINK'
      INCLUDE 'D0$INC:RMINMAX.INC'
C
      INTEGER IETA, IPHI, JPHI, JQCLAY, LQCLGB, JCLGB, LZFIND, LERR
      REAL RMIN, RMAX, RC
C
      DATA LERR / 6 /
C
      ENTRY RMIN_MAX_MH( IETA, IPHI, RMIN, RMAX)
      JQCLAY = LQCLAY
C
C ...   FIRST MH CELL
      IF((JCLGA .EQ. ICMFHA .OR. JCLGA .EQ. ICMCHA).AND.                
     &    JETA .EQ. IC(JQCLAY+ILNETA)-1 ) THEN   ! first MH cell special case
        JCLGB = JCLGA + 50
        LQCLGB = LZFIND(IDVSTP, LC(LQCREG - IZCLGA), JCLGB, IGIDEN)
        IF( LQCLGB .EQ. 0 ) THEN
          WRITE(LERR, *) ' CELSHA -- LQCLGB NOT FOUND: ',JCLGB
          STOP 57
        END IF
        LQCSHA = LC(LQCLGB-IXCSHA)
        RC = SQRT(C(LQCLGB+IGXCEN)**2+C(LQCLGB+IGYCEN)**2) +
     &      C(LQCSHA+IGPAR4)
        RMIN = C(JQCLAY + ILRPD0 + JETA)
        JPHI = MOD(IPHI-1,4) + 1
        IF( JPHI .EQ. 1 .OR. JPHI .EQ. 4) THEN
          RMAX = 0.99358685 * RC      ! RC * SIN(PI/32)/(PI/32)
        ELSE                          ! JPHI = 2,3
          RMAX = 0.9887793 * RC       ! RC * (SIN(PI/16)-SIN(PI/32))/
                                      ! (PI/32)
        END IF
C ...    LAST MH CELL 
      ELSE IF((JCLGA .EQ. ICMFHA .OR. JCLGA .EQ. ICMCHA) .AND. 
     &  JETA .EQ. 0 ) THEN    ! first MH cell special case
        LQCLGA = LC(LQCLAY-IZLLGA)
        LQCSHA = LC(LQCLGA-IXCSHA)
        RC = SQRT(C(LQCLGA+IGXCEN)**2+C(LQCLGA+IGYCEN)**2) -
     &      C(LQCSHA+IGPAR4)
        RMAX = C(JQCLAY + ILRPD1 + JETA)
          JPHI = MOD(IPHI-1,4) + 1
        IF( JPHI .EQ. 1 .OR. JPHI .EQ. 4) THEN
          RMIN = 1.00323147 * RC      ! RC * LN((1+PI/32)/(1-PI/32))/
                                      ! (PI/16)
        ELSE                          ! JPHI = 2,3
          RMIN = 1.023082  * RC       ! RC * LN((1+PI/16)*(1-PI/32)/
                                      ! (1-PI/16)/(1+PI/16))/(PI/16)
        END IF
C ... 
      ELSE                            ! EC/IH and interior EC/MH 
        RMIN = C(JQCLAY + ILRPD0 +  JETA)
        RMAX = C(JQCLAY + ILRPD1 +  JETA)
      END IF
C
      RETURN
C
C
      ENTRY RMIN_MAX_OH( IETA, IPHI, RMIN, RMAX)
      JQCLAY = LQCLAY
C
C ...   FIRST OH CELL
      IF( JETA .EQ. 0 ) THEN           ! first OH cell special case
        LQCLGA = LC(LQCLAY-IZLLGA)
        LQCSHA = LC(LQCLGA-IXCSHA)
        RC = SQRT(C(LQCLGA+IGXCEN)**2+C(LQCLGA+IGYCEN)**2) -
     &      C(LQCSHA+IGPAR5)
        RMAX = C(JQCLAY + ILRPD1)
        JPHI = MOD(IPHI-1,4) + 1
        IF( JPHI .EQ. 1 .OR. JPHI .EQ. 4) THEN
          RMIN = 1.00323147 * RC      ! RC * LN((1+PI/32)/(1-PI/32)) /
                                      ! (PI/16) 
        ELSE                          ! JPHI = 2,3
          RMIN = 1.023082 * RC        ! RC * LN((1+PI/16)*(1-PI/32))/
                                      ! (1-PI/16)/(1+PI/32))/(PI/16)
        END IF
C ...    LAST OH CELL 
      ELSE IF( JETA .EQ. IC(JQCLAY+ILNETA)-1 ) THEN ! last OH cell special case
         LQCLGA = LC(LQCLAY-IZLLGA)
         LQCSHA = LC(LQCLGA-IXCSHA)
         RC = SQRT(C(LQCLGA+IGXCEN)**2+C(LQCLGA+IGYCEN)**2) +
     &      C(LQCSHA+IGPAR5)
         RMIN = C(JQCLAY + ILRPD0 + JETA)
         JPHI = MOD(IPHI-1,4) + 1
         IF( JPHI .EQ. 1 .OR. JPHI .EQ. 4) THEN
           RMAX = 1.00323147 * RC      ! RC * LN((1+PI/32)/(1-PI/32))/
                                       ! (PI/16)
         ELSE                          ! JPHI = 2,3
           RMAX = 1.023082  * RC       ! RC * LN((1+PI/16)*(1-PI/32)/
                                       ! (1-PI/16)/(1+PI/16))/(PI/16)
         END IF
C ... 
      ELSE                            ! interior EC/OH 
        RMIN = C(JQCLAY + ILRPD0 +  JETA)
        RMAX = C(JQCLAY + ILRPD1 +  JETA)
      END IF
C
C----------------------------------------------------------------------
  999 RETURN
      END
