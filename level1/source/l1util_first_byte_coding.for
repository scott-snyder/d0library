      SUBROUTINE L1UTIL_FIRST_BYTE_CODING (NBYTES, FIRST_ADDRESS, WORD)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Unpacks NBYTES bytes from WORD array, then
C-                         stores them into the Level 1 Trigger Data
C-                         Block starting at the 16-bit address
C-                         FIRST_ADDRESS. In the Data Block bytes are
C-                         always in the first byte of a 16-bit word
C-                         (cf : Appendix of D0 Note 706).
C-
C-   Inputs :  NBYTES :        Number of bytes to be stored;
C-             FIRST_ADDRESS : First storage 16-bit address in the Data Block;
C-             WORD :          Contains bytes to be unpacked;
C-
C-   Outputs : None.
C-   Controls: None.
C-
C-   Created  19-MAR-1990   Sylvain Tisserant (MSU)
C-   Updated  Level 1 Simulator, Michigan State University,
C-                      Philippe Laurens, Steven Klocek,
C-                      L1C -> L1SIM, and major upgrade to use LSM, COOR_sim
C-                        - Split LEVEL1_TRIGGER_DATA_BLOCK.INC into a .PARAMS
C-                          file and a .INC file. 
C----------------------------------------------------------------------
C
      IMPLICIT NONE
C
      INCLUDE 'D0$PARAMS:LEVEL1_LOOKUP.PARAMS'
      INCLUDE 'D0$PARAMS:L1_CALTRIG.PARAMS'
      INCLUDE 'D0$PARAMS:L1_FRAMEWORK.PARAMS'
      INCLUDE 'D0$PARAMS:L1DBB_DATA_BLOCK.PARAMS'
      INCLUDE 'D0$INC:L1DBB_DATA_BLOCK.INC'
C
      INTEGER    WORD(1), FIRST_ADDRESS, NBYTES
C
      INTEGER    I1, J1, I2, J2, BYTE
C
C----------------------------------------------------------------------
C
      I1 = 1
      J1 = FIRST_BYTE
      CALL L1UTIL_WHERE_WORD (FIRST_ADDRESS, I2, J2)
      DO BYTE = 1, NBYTES
        CALL CBYT (WORD(I1), J1, LVL1_DATA_BLOCK(I2), J2, BYTE_LENGTH)
        J1 = J1 + BYTE_LENGTH
        IF(J1.GT.LONG_WORD_LENGTH) THEN
          I1 = I1 + 1
          J1 = FIRST_BYTE
        ENDIF
        J2 = J2 + WORD_LENGTH
        IF(J2.GT.LONG_WORD_LENGTH) THEN
          I2 = I2 + 1
          J2 = FIRST_BYTE
        ENDIF
      ENDDO
      RETURN
C
      END
