      FUNCTION TB90L2_CONV_MAP_COOR_PHY_COOR(module,layer,ieta,iphi,
     + eta,phi)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : given the module and map layer returns the actual
C-   layer number of the module.
C-   Outputs eta,phi of cell.
C-       eta = eta - .5 for + eta (also subt .25 for EM3 subdivs)
C-       eta = eta + .5 for - eta
C-       phi = phi - .5
C-              This will put the eta,phi location at center of pad
C-
C-   Returned value  : pysical layer number
C-   Inputs  : module , map layer, ieta,iphi
C-   Outputs : eta,phi
C-   Controls: none
C-
C-   Created  13-JUN-1991   James Richardson
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER TB90L2_CONV_MAP_COOR_PHY_COOR
      INTEGER module, layer, ieta, iphi ! address of cel
      REAL    eta, phi
      INCLUDE 'd0$params:tb90l2_modules.def'
C----------------------------------------------------------------------
C
C ****  check all multilayer modules.
C
      IF ( module .EQ. TB90L2EM ) THEN          ! check em layers
        IF ( layer .GT. 2 .AND. layer .LT. 7 ) THEN
          TB90L2_CONV_MAP_COOR_PHY_COOR = 3
C
C ****  positive eta
C
          IF ( ieta .GE. 0 ) THEN
            IF ( layer .EQ. 3 ) THEN
              eta = float(ieta) - .75
              phi = float(iphi) - .75
            ELSEIF ( layer .EQ. 4 ) THEN
              eta = float(ieta) - .75
              phi = float(iphi) - .25
            ELSEIF ( layer .EQ. 5 ) THEN
              eta = float(ieta) - .25
              phi = float(iphi) - .75
            ELSE                  ! layer must be 6
              eta = float(ieta) - .25
              phi = float(iphi) - .25
            ENDIF
          ELSE                          ! negative eta
C
C ****  negative eta
C
            IF ( layer .EQ. 3 ) THEN
              eta = float(ieta) + .25
              phi = float(iphi) - .75
            ELSEIF ( layer .EQ. 4 ) THEN
              eta = float(ieta) + .25
              phi = float(iphi) - .25
            ELSEIF ( layer .EQ. 5 ) THEN
              eta = float(ieta) + .75
              phi = float(iphi) - .75
            ELSE                  ! layer must be 6
              eta = float(ieta) + .75
              phi = float(iphi) - .25
            ENDIF
          ENDIF
        ELSEIF ( layer .EQ. 7 ) THEN
          TB90L2_CONV_MAP_COOR_PHY_COOR = 4
        ELSE
          TB90L2_CONV_MAP_COOR_PHY_COOR = layer
        ENDIF
      ELSEIF ( module .EQ. TB90L2FH ) THEN      ! check fh layers
        TB90L2_CONV_MAP_COOR_PHY_COOR = layer - 10
      ELSEIF ( module .EQ. TB90L2OH ) THEN      ! check oh layers
        TB90L2_CONV_MAP_COOR_PHY_COOR = layer - 14
      ELSEIF ( module .EQ. TB90L2MH ) THEN      ! check mh layers
        TB90L2_CONV_MAP_COOR_PHY_COOR = layer - 10
      ELSE
        TB90L2_CONV_MAP_COOR_PHY_COOR = 1   ! retun 1 for all single layer mods
      ENDIF
      IF ( .NOT.((tb90l2_conv_map_coor_phy_coor .EQ. 3)
     &      .AND.(module .EQ. TB90L2EM )) )THEN   ! already did EM3
        eta = sign(abs(float(ieta))-.5,float(ieta))
        phi = sign(abs(float(iphi))-.5,float(iphi))
      ENDIF
      RETURN
      END
