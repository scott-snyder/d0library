      SUBROUTINE L2J_CL2_DECLARE(IETA,IPHI,NRING)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Declare the section of eta-phi space that
C-              we will want the l2 fast unpacking routines to provide
C-              for us. Use CL2_ROTOW_ETNOM
C-
C-   Inputs  : IETA,IPHI = center of cone in offline indices
C-             NRING     = number of rings around the center. 0=just center.
C-                       1= center + 8 nearest neighbors etc.
C-   Outputs :
C-   Controls:
C-
C-   Created  14-JAN-1991   Richard Astur
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:L1PHP.PARAMS'
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$INC:L2J_UTIL.INC'     ! eta-phi lookup
      INTEGER IPHI,IETA,NRING
      INTEGER IETA2,IPHI2,IETAMAX,IETAMIN,IPHIMAX,IPHIMIN
C----------------------------------------------------------------------
C--- We assume the indices passed to us are good. Now we want to use
C--- L2J_XXX arrays so we need to remap IETA,IPHI.
      IPHI2 = IPHI
      IETA2 = 2*IETA - SIGN(1,IETA)
C--- Find max and min of the cone.
      IETAMAX = L2J_ETA(IETA2 + 2*NRING)
      IETAMIN = L2J_ETA(IETA2 - 2*NRING)
      IPHIMAX = L2J_PHI(IPHI2 + NRING)
      IPHIMIN = L2J_PHI(IPHI2 - NRING)
C--- Handle out of bounds IETA.
      IF (IETAMAX .EQ. 0) IETAMAX = NETAL
      IF (IETAMIN .EQ. 0) IETAMIN = -NETAL
C--- Remember that PHI can 'wrap' around, so be careful of different cases:
      IF ( 2*NRING + 1 .GE. NPHIL ) THEN        ! Declare all phi space.
        CALL CL2_ROTOW_ETNOM(IETAMIN,IETAMAX,1,NPHIL)
        CALL CL2_ICDMG_ETNOM(IETAMIN,IETAMAX,1,NPHIL)
      ELSE IF (IPHI2+NRING .LE. NPHIL .AND. IPHI2-NRING .GE. 1) THEN
        CALL CL2_ROTOW_ETNOM(IETAMIN,IETAMAX,IPHIMIN,IPHIMAX)
        CALL CL2_ICDMG_ETNOM(IETAMIN,IETAMAX,IPHIMIN,IPHIMAX)
      ELSE
        CALL CL2_ROTOW_ETNOM(IETAMIN,IETAMAX,IPHIMIN,NPHIL)
        CALL CL2_ICDMG_ETNOM(IETAMIN,IETAMAX,IPHIMIN,NPHIL)
        CALL CL2_ROTOW_ETNOM(IETAMIN,IETAMAX,1,IPHIMAX)
        CALL CL2_ICDMG_ETNOM(IETAMIN,IETAMAX,1,IPHIMAX)
      END IF

  999 RETURN
      END
