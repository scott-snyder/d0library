      SUBROUTINE L1UTIL_WHERE_WORD (ADDRESS, IW, LSB)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Gives for a 16-bit word address in the Level 1
C-                         Trigger Data Block the position of the
C-                         corresponding byte in a equivalente 32-bit
C-                         word table.
C-
C-   Inputs  : ADDRESS : address of 16-bit word in the Level 1 Trigger
C-                       Data Block. Notations are the ones used in the
C-                       D0 Note 706 Appendix A.
C-
C-   Outputs : IW :  Index pointing onto the corresponding 32-bit word;
C-             LSB : Least Significant Bit identifying the corresponding byte,
C-                   using the CernLib BITBYT package notations.
C-
C-   Controls: None.
C-
C-   Created  19-MAR-1990   Sylvain Tisserant (MSU)
C-   Updated  18-OCT-1991   Level 1 Simulator, Michigan State University,
C-                      Philippe Laurens, Steven Klocek,
C-                      L1C -> L1SIM, and major upgrade to use LSM, COOR_sim
C-                        - Split LEVEL1_TRIGGER_DATA_BLOCK.INC into a .PARAMS
C-                          file and a .INC file. 
C-                          - Changed name of routine from WHERE_IN_DATA_BLOCK
C-                            to L1UTIL_WHERE_WORD. 
C-                          - Replaced D0$PARAMS:LEVEL1_CAL_TRIG.PARAMS with
C-                            D0$PARAMS:L1_CALTRIG.PARAMS 
C-                          - Replaced D0$PARAMS:LEVEL1_FRAMEWORK.PARAMS with
C-                            D0$PARAMS:L1_FRAMEWORK.PARAMS 
C-                          - Replaced
C-                            D0$PARAMS:LEVEL1_TRIGGER_DATA_BLOCK.PARAMS with
C-                            D0$PARAMS:L1DBB_DATA_BLOCK.PARAMS 
C----------------------------------------------------------------------
C
      IMPLICIT NONE
C
      INCLUDE 'D0$PARAMS:LEVEL1_LOOKUP.PARAMS'
      INCLUDE 'D0$PARAMS:L1_CALTRIG.PARAMS'
      INCLUDE 'D0$PARAMS:L1_FRAMEWORK.PARAMS'
      INCLUDE 'D0$PARAMS:L1DBB_DATA_BLOCK.PARAMS'
C
      INTEGER    ADDRESS, IW, LSB
C
      INTEGER    I
C
C----------------------------------------------------------------------
C
      I  = ADDRESS - BASE_ADDRESS
      IW = (I/2) + 1
      IF(MOD(I,2).EQ.0) THEN
        LSB = FIRST_BYTE
      ELSE
        LSB = THIRD_BYTE
      ENDIF
      RETURN
C
      END
