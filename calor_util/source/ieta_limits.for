      SUBROUTINE IETA_LIMITS( ETA_PHYSICS, PHI_PHYSICS, CONE_SIZE,
     &  ZVERTEX_IN, IETAMAX, IETAMIN, IPHIMAX, IPHIMIN )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Calculate the range ( [IETAMIN,IETAMAX] ),
C-      in readout tower space of a cone of size CONE_SIZE which
C-      extends from a point x=0., y=0., z=ZVERTEX and has a psuedorapidity
C-      equal to ETA_PHYSICS.  This range tells us which readout towers
C-      may contain energy from a jet of such a cone size.
C-
C-   Inputs  : ETA_PHYSICS    [R]   : Physics eta of the cone (jet)
C-             PHI_PHYSICS    [R]   : Physics phi of the cone (jet)
C-             CONE_SIZE      [R]   : cone size in eta-phi space
C-             ZVERTEX        [R]   : Z vertex position  from which this
C-                                    eta was derived.
C-   Outputs : IETAMAX        [I]   : Maximum index of readout tower in eta
C-             IETAMIN        [I]   : Minimum index of readout tower in eta
C-             IPHIMAX        [I]   : Maximum index of readout tower in phi
C-             IPHIMIN        [I]   : Minimum index of readout tower in phi
C-                                    NOTE: IPHIMAX can be greater than 64
C-                                      and IPHIMIN can be less than 1 due
C-                                      to the finite conesize, the caller
C-                                      must remap them to 1-64.
C-   Controls:
C-
C-   Created  17-OCT-1993   Richard V. Astur
C-   Modified 10-OCT-1994   R. Astur "Prevent large vertex values which might
C-                                    lead to too large eta's. and map
C-                                    maximum theta to eta=4.5"
C-   Updated   7-AUG-1995   R. Astur "Bug fix: distance to near and far
C-                                    points of cal WAS 80/150. This is
C-                                    not right: should be 80/360. Since
C-                                    distance is measured from the vertex;
C-                                    now use abs(80-abs(z)) and abs(360+abs(z))
C-                                      Also, IETAMIN/IETAMAX WERE defined as
C-                                    10*eta+sign(eta), this is ok for the
C-                                    side closest to the beam, but not for
C-                                    the other side:fix. Further, IETAMAX
C-                                    was protected from going above 37, but
C-                                    IETAMIN was not:fix. "
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$INC:PI.DEF'
      REAL ETA_PHYSICS, CONE_SIZE, ZVERTEX_IN, ZVERTEX, PHI_PHYSICS
      INTEGER IETAMAX, IETAMIN, IPHIMAX, IPHIMIN
C
      REAL Z1, Z2, R1, R2, D1, D2, DMIN, DMAX
      REAL THETA_D1, THETA_D2, THETA_E1, ETA_D1, ETA_D2, PHI_D1, PHI_D2
      REAL THETA_FROM_ETA, RETA, ETA_FROM_THETA, RTHETA
      DATA DMIN /80./               ! Assumed minimum distance to EM1
      DATA DMAX /360./              ! Assumed maximum distance to CH4
C----------------------------------------------------------------------
C
C---Convert ETA to THETA
C
      THETA_FROM_ETA(RETA ) = 2*ATAN(EXP(-(RETA)))
C
C---Convert THETA to ETA
C
      ETA_FROM_THETA(RTHETA) =  -ALOG(TAN((RTHETA)/2.))
C-----------------------------------------------------------------------
C
C: Find the maximum angles subtended by this cone assuing the calorimeter
C: is located in a radial distance from DMIN to DMAX (cm)
C: The limits depend on whether the ZVERTEX (also in cm) is to the left
C: or the right of the interaction point.
C
C
C: Ignore very large vertex positions
C
      ZVERTEX   =   ZVERTEX_IN
      IF ( ABS(ZVERTEX) .GT. 200. ) ZVERTEX = 0.0
C
      D1 = ABS( DMIN - ABS(ZVERTEX))
      D2 = ABS( DMAX + ABS(ZVERTEX))
      IF ( ZVERTEX .LT. 0.0 ) THEN
        D2 = D1
        D1 = ABS( DMAX + ABS(ZVERTEX))
      ENDIF
C
C: Calculate theta of physics cone and find which IETA's this
C: intercepts a the maximum points
C
      PHI_D1   = PHI_PHYSICS + CONE_SIZE
      THETA_E1 = THETA_FROM_ETA( ETA_PHYSICS + CONE_SIZE )
      Z1 = ZVERTEX + D1 * COS( THETA_E1 )
      R1 = D1 * SIN( THETA_E1 )
C
      PHI_D2   = PHI_PHYSICS - CONE_SIZE
      THETA_E1 = THETA_FROM_ETA( ETA_PHYSICS - CONE_SIZE )
      Z2 = ZVERTEX + D2 * COS( THETA_E1 )
      R2 = D2 * SIN( THETA_E1 )
C
C: Maximum spread in detector theta
C
      THETA_D1 = ATAN2( R1, Z1 )
      THETA_D2 = ATAN2( R2, Z2 )
C
C: Make sure they stay in the calorimeter
C
      THETA_D1 = MAX( THETA_D1, .022 )  ! eta < 4.5
      THETA_D2 = MAX( THETA_D2, .022 ) 
      THETA_D1 = MIN( PI-DBLE(.022), DBLE(THETA_D1) )
      THETA_D2 = MIN( PI-DBLE(.022), DBLE(THETA_D2) )
C
      ETA_D1   = ETA_FROM_THETA( THETA_D1 )
      ETA_D2   = ETA_FROM_THETA( THETA_D2 )
C
C
C: One IETA tower per .1 of psuedorapidity is only an approximation.
C: This changes in the forward regions. See D0$DOCS:CALORIMETER_ADDRESSING.MEM
C: But since the bins coarsen, we will get at least the cone we want and maybe
C: a little bit more.
C
      

      IETAMAX = ETA_D1/.1 + SIGN( 1., ETA_D1 )    ! Most of the time
      IF ( ABS(ETA_D1) .GE. 4.45 ) THEN
        IETAMAX = 37*SIGN(1.,ETA_D1)
      ELSEIF( ABS(ETA_D1) .GE. 4.1 ) THEN
        IETAMAX = 36*SIGN(1.,ETA_D1)
      ELSEIF( ABS(ETA_D1) .GE. 3.7 ) THEN
        IETAMAX = 35*SIGN(1.,ETA_D1)
      ELSEIF( ABS(ETA_D1) .GE. 3.42 ) THEN
        IETAMAX = 34*SIGN(1.,ETA_D1)
      ELSEIF( ABS(ETA_D1) .GE. 3.2 ) THEN
        IETAMAX = 33*SIGN(1.,ETA_D1)
      ENDIF
        
      IETAMIN = ETA_D2/.1 + SIGN( 1., ETA_D2 )
      IF ( ABS(ETA_D2) .GE. 4.45 ) THEN
        IETAMIN = 37*SIGN(1.,ETA_D2)
      ELSEIF( ABS(ETA_D2) .GE. 4.1 ) THEN
        IETAMIN = 36*SIGN(1.,ETA_D2)
      ELSEIF( ABS(ETA_D2) .GE. 3.7 ) THEN
        IETAMIN = 35*SIGN(1.,ETA_D2)
      ELSEIF( ABS(ETA_D2) .GE. 3.42 ) THEN
        IETAMIN = 34*SIGN(1.,ETA_D2)
      ELSEIF( ABS(ETA_D2) .GE. 3.2 ) THEN
        IETAMIN = 33*SIGN(1.,ETA_D2)
      ENDIF

      IPHIMAX = 64*PHI_D1/TWOPI + 1
      IPHIMIN = 64*PHI_D2/TWOPI + 1
C
C: Make sure IETA is within limits. No guarantee for IPHI though.
C
      IETAMAX = MAX( -NETAL, MIN(  NETAL, IETAMAX ) )
      IETAMIN = MAX( -NETAL, MIN(  NETAL, IETAMIN ) )

  999 RETURN
      END
