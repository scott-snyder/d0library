      SUBROUTINE TB90L2_GET_SOFTWARE_INDX
     &    (eta,phi,mod_lyr,module,eta_indx,phi_indx,lyr_indx)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : given eta,phi,layer of module returns the software
C-   indicies of eta,phi, and layer
C-
C-   Inputs  : eta,phi and layer of module, and modules
C-   Outputs : eta,phi and layer indicies
C-   Controls: none
C-
C-   Created   8-AUG-1991   James Richardson
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:TB90L2_MODULES.DEF'
      REAL    eta,phi
      INTEGER mod_lyr, module
      INTEGER eta_indx, phi_indx, lyr_indx
      INTEGER eta_temp,phi_temp
C
C ****  statement functions
C ****  round_up  --  converts reals to integers away from 0
C ****                  (.5 -> 1, -.5 -> -1
C ****
C
      REAL    r1
      INTEGER round_up
      round_up(r1) = int(sign(abs(r1) + .9999,r1))
C----------------------------------------------------------------------
      eta_indx = round_up(eta)
      phi_indx = round_up(phi)
      IF ( module .EQ. TB90L2EM ) THEN
        IF ( mod_lyr .LT. 3 ) THEN
          lyr_indx = mod_lyr
        ELSEIF ( mod_lyr .EQ. 3 ) THEN
          eta_temp = int((eta - int(eta))*100.)
          phi_temp = int((phi - int(phi))*100.)
          IF ( eta_temp .LE. sign(50,int(eta)) ) THEN        ! must be 3 or 4
            IF ( phi_temp .LE. 50 ) THEN
              lyr_indx = 3
            ELSE
              lyr_indx = 4
            ENDIF
          ELSE                      ! only 5 or 6 left
            IF ( phi_temp .LE. 50 ) THEN
              lyr_indx = 5
            ELSE
              lyr_indx = 6
            ENDIF
          ENDIF
        ELSE
          lyr_indx = 7                  ! only on left in em
        ENDIF
      ELSEIF ( module .EQ. TB90L2FH ) THEN
        lyr_indx = 10 + mod_lyr
      ELSEIF ( module .EQ. TB90L2CH ) THEN
        lyr_indx = 15
      ELSEIF ( module .EQ. TB90L2MH ) THEN
        lyr_indx = 10 + mod_lyr
      ELSEIF ( module .EQ. TB90L2OH ) THEN
        lyr_indx = 14 + mod_lyr
      ENDIF
      RETURN
      END
