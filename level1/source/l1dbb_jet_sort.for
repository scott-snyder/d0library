      SUBROUTINE L1DBB_JET_SORT ( ENERGY,     MASK,       ADDRESS,
     +                      JET_LENGTH, JET,        JET_FULL,
     +                      JET_ENERGY, JET_LIST             )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Sorts with decreasing energies the Jet candidates.
C-
C-   Inputs  : ENERGY :     Trigger Tower Energy;
C-             MASK :       Programmed and Fired Trigger Mask;
C-             ADDRESS :    Trigger Tower Relative Address;
C-             JET_LENGTH : Maximum number of entries in a Jet List;
C-             JET :        Number of already found Jet candidates;
C-             JET_FULL :   Tells that the Jet List is saturated.
C-
C-   Outputs : JET :         Number of kept in the List Jet candidates;
C-             JET_FULL :    Tells that the Jet List is saturated;
C-             JET_ENERGY :  Energy list used as reference for sorting;
C-             JET_LIST :    Ordered Jet List, contents masks and addresses.
C-
C-   Controls: None.
C-
C-   Created   9-MAR-1990   Sylvain Tisserant (MSU)
C-   Revised  18-JUN-1990
C-   Updated  18-OCT-1991   Level 1 Simulator, Michigan State University,
C-                      Philippe Laurens, Steven Klocek,
C-                      L1C -> L1SIM, and major upgrade to use LSM, COOR_sim
C-                          - Removed extra RETURN statements to meet D0
C-                            standards. 
C-                          - Changed name of routine from JET_SORT to
C-                            L1DBB_JET_SORT. 
C----------------------------------------------------------------------
C
      IMPLICIT NONE
C
      INTEGER ENERGY,        MASK,       ADDRESS,
     +        JET_LENGTH,    JET,
     +        JET_ENERGY(1), JET_LIST(2,1)
      LOGICAL  JET_FULL
C
      INTEGER  I, J
C
C----------------------------------------------------------------------
C
      DO I = 1, JET
        IF(ENERGY.GT.JET_ENERGY(I)) GOTO 10
      ENDDO
C
C     Lowest energy : at the end of the list if space is available
C     ------------------------------------------------------------
C
      IF(JET.EQ.JET_LENGTH) THEN
        JET_FULL = .TRUE.
      ELSE
        JET = JET + 1
        JET_ENERGY(JET) = ENERGY
        JET_LIST(1,JET) = MASK
        JET_LIST(2,JET) = ADDRESS
      ENDIF
      GOTO 999 
C
C     First shifts the lower entries then records the Jet candidate
C     -------------------------------------------------------------
C
   10 IF(JET.EQ.JET_LENGTH) THEN
        JET_FULL = .TRUE.
      ELSE
        JET = JET + 1
      ENDIF
      DO J = JET, I+1, -1
        JET_ENERGY(J) = JET_ENERGY(J-1)
        JET_LIST(1,J) = JET_LIST(1,J-1)
        JET_LIST(2,J) = JET_LIST(2,J-1)
      ENDDO
      JET_ENERGY(I) = ENERGY
      JET_LIST(1,I) = MASK
      JET_LIST(2,I) = ADDRESS
  999 RETURN
C
      END
