      FUNCTION TB90L2_POSITION_CORRECTION(lyr_indx,etas,phis,etac,phic)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : computes position resolution corrections using
C-   energy ratios in neighboring cells. This is based on a 3x3 cluster with
C-   the beam hitting the center pad.
C-
C-   Returned value  : true if able to perform correction
C-                     false if not able - cluster is off of calorimter
C-   Inputs  :
C-      lyr_indx        -  software index of layer
C-      etas,phis       -  limit on cells of the cluster
C-   Outputs :
C-      etac            -  log(energy in left side / energy in right side)
C-      phic            -  log(energy below / energy above hit side
C-   Controls: none
C-
C-   Created   8-AUG-1991   James Richardson
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL tb90l2_position_correction
      REAL    tb90l2_cluster_energy
      external tb90l2_cluster_energy
      INTEGER lyr_indx,etas(2),phis(2)
      REAL    etac,phic
      INTEGER temp(2)
      REAL    numer,denom
C----------------------------------------------------------------------
      tb90l2_position_correction = .true.       ! assume good
      etac = 0.
      phic = 0.
C
C ****  do eta correction
C
      temp(1) = etas(1)
      temp(2) = etas(1)
      numer = tb90l2_cluster_energy(lyr_indx,temp,phis)
      temp(1) = etas(2)
      temp(2) = etas(2)
      denom = tb90l2_cluster_energy(lyr_indx,temp,phis)
      IF ( numer .LE. 0. .OR. denom .LE. 0 ) THEN
        etac = 0.
      ELSE
        etac = -log(numer / denom)
      ENDIF
C
C ****  do phi correction
C
      temp(1) = phis(1)
      temp(2) = phis(1)
      numer = tb90l2_cluster_energy(lyr_indx,etas,temp)
      temp(1) = phis(2)
      temp(2) = phis(2)
      denom = tb90l2_cluster_energy(lyr_indx,etas,temp)
      IF ( numer .LE. 0. .OR. denom .LE. 0 ) THEN
        phic = 0.
      ELSE
        phic = -log(numer / denom)
      ENDIF
      RETURN
      END
