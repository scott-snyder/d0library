      SUBROUTINE L1DMP_DBLOCK_BYTE(ADDR, LOW, VALUE)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return the byte at the specified location in the
C-      Level 1 datablock in the simulator. See also D0 Note 967.
C-
C-   Inputs  : ADDR     The location of the information as given in D0 Note 967
C-             LOW      Parameter telling whether to return the low or 
C-                      high byte. FIRST_BYTE or SECOND_BYTE.
C-                      
C-   Outputs : VALUE    The requested byte
C-   Controls: none
C-
C-   Created  15-JUL-1991   Level 1 Simulator, Michigan State University,
C-                      Philippe Laurens, Steven Klocek,
C-                      L1C -> L1SIM, and major upgrade to use LSM, COOR_sim
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:LEVEL1_LOOKUP.PARAMS'
      INCLUDE 'D0$PARAMS:L1_CALTRIG.PARAMS'
      INCLUDE 'D0$PARAMS:L1_FRAMEWORK.PARAMS'
      INCLUDE 'D0$PARAMS:L1DBB_DATA_BLOCK.PARAMS'
      INCLUDE 'D0$INC:L1DBB_DATA_BLOCK.INC'
C
      INTEGER JBYT
      EXTERNAL JBYT
      INTEGER ADDR, VALUE
      INTEGER LOW
      LOGICAL ODD
      INTEGER DBLOCK_ADDR
C
      DBLOCK_ADDR = (ADDR - 1) /2 + 1
      ODD = .FALSE.
      IF (MOD(ADDR, 2) .EQ. 1) ODD = .TRUE.
      IF (LOW .EQ. FIRST_BYTE) THEN
        IF (ODD .EQV. .TRUE.) THEN
          VALUE = JBYT(LVL1_DATA_BLOCK(DBLOCK_ADDR), FIRST_BYTE,
     &      BYTE_LENGTH)
        ELSE
          VALUE = JBYT(LVL1_DATA_BLOCK(DBLOCK_ADDR), THIRD_BYTE,
     &      BYTE_LENGTH)
        ENDIF
      ELSE
        IF (ODD .EQV. .TRUE.) THEN
          VALUE = JBYT(LVL1_DATA_BLOCK(DBLOCK_ADDR), SECOND_BYTE,
     &      BYTE_LENGTH)
        ELSE
          VALUE = JBYT(LVL1_DATA_BLOCK(DBLOCK_ADDR), FOURTH_BYTE,
     &      BYTE_LENGTH)
        ENDIF
      ENDIF
C----------------------------------------------------------------------
  999 RETURN
      END
