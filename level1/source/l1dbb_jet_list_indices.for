      SUBROUTINE L1DBB_JET_LIST_INDICES(ADDRESS, SIGN_ETA, MAGN_ETA, 
     &  PHI)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Obtain the SIGN/ETA/PHI indices corresponding to a
C-      particular entry in the jet list from the address format used in the
C-      Level 1 Datablock. Cf. D0 Note 967.
C-
C-   Inputs  : ADDRESS  the address to unpack
C-   Outputs : SIGN_ETA POS_ETA or NEG_ETA
C-             ETA      the ETA index
C-             PHI      the PHI index
C-   Controls: none
C-
C-   Created  12-JUL-1991   Level 1 Simulator, Michigan State University,
C-                      Philippe Laurens, Steven Klocek,
C-                      L1C -> L1SIM, and major upgrade to use LSM, COOR_sim
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:LEVEL1_LOOKUP.PARAMS'
C
      INTEGER ADDRESS, SIGN_ETA, MAGN_ETA, PHI
C
      INTEGER WORK_ADDRESS
C
      INTEGER NUM_PHI
      PARAMETER (NUM_PHI = PHI_MAX - PHI_MIN + 1)
      INTEGER FIRST_NEGATIVE_ETA
      PARAMETER (FIRST_NEGATIVE_ETA = 
     &  (ETA_MAX - ETA_MIN + 1) * (PHI_MAX - PHI_MIN + 1) )
C
      WORK_ADDRESS = ADDRESS
      IF (ADDRESS .GE. FIRST_NEGATIVE_ETA) THEN
        SIGN_ETA = NEG_ETA
        WORK_ADDRESS = WORK_ADDRESS - FIRST_NEGATIVE_ETA
      ELSE
        SIGN_ETA = POS_ETA
      ENDIF
C
      MAGN_ETA = WORK_ADDRESS / NUM_PHI
      WORK_ADDRESS = WORK_ADDRESS - (NUM_PHI * MAGN_ETA)
      MAGN_ETA = MAGN_ETA + 1
C
      PHI = WORK_ADDRESS / 2
      IF (( WORK_ADDRESS - 2 * PHI) .EQ. 0) THEN
        PHI = PHI + 1
      ELSE
        PHI = PHI + 17
      END IF
C
C----------------------------------------------------------------------
  999 RETURN
      END
