      FUNCTION TB90L2_CLUSTER_ENERGY(lyr_indx,etas,phis)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : return energy in cells of layer lyr_indx within the
C-   eta,phi range etas,phis
C-
C-   Returned value  : energy in cluster
C-   Inputs  :
C-     lyr_indx  [I]    --      software index of layer
C-     etas (2)  [R]    --      eta range eta(1) < eta(2)
C-     phis (2) [R]     --      phi range phi(1) < phi(2)
C-   Outputs : none
C-   Controls: none
C-
C-   Created   8-AUG-1991   James Richardson
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER gzcaep
      EXTERNAL gzcaep
      REAL    tb90l2_cluster_energy
      INTEGER lyr_indx,etas(2),phis(2)
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$INC:PTCAEP.INC'
      INTEGER lcaep
      INTEGER caep_chan                 ! channel in caep bank for cell
      INTEGER i,j
C----------------------------------------------------------------------
      lcaep = gzcaep()
      IF ( lcaep .EQ. 0 ) THEN          ! if caep not thre fill w/ trash
        tb90l2_cluster_energy = -99999.
        RETURN
      else
        tb90l2_cluster_energy = 0.      ! re zero it
      ENDIF
      DO i = phis(1) , phis(2)
        DO j = etas(1) , etas(2)
          IF ( j .NE. 0 ) THEN              ! skip 0 index
            caep_chan = ptcaep(j,i,lyr_indx)
            IF (caep_chan .NE. 0 ) THEN
              tb90l2_cluster_energy =
     &                  tb90l2_cluster_energy + q(lcaep+3+2*caep_chan)
            ENDIF
          ENDIF
        ENDDO
      ENDDO
      RETURN
      END
