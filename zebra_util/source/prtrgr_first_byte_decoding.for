      SUBROUTINE PRTRGR_FIRST_BYTE_DECODING(NBYTES, FIRST_ADDRESS, 
     &                                      DBLOCK, WORD)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Extracts NBYTES from the Level 1 Trigger Data
C-                         Block, starting at the 16-bit address
C-                         FIRST_ADDRESS, then packs them into WORD array.
C-                         Bytes are always in the first byte of a 16-bit
C-                         word (cf : Appendix of D0 Note 967 )
C-
C-   Inputs :  NBYTES :        Number of bytes to be stored;
C-             FIRST_ADDRESS : First storage 16-bit address in the Data Block;
C-             DBLOCK          The first word of the Level 1 Datablock.
C-
C-   Outputs : WORD :          Byte target.
C-
C-   Controls: None.
C-
C-   Created  19-MAR-1990   Sylvain Tisserant (MSU)
C-   Updated   8-JAN-1992   Philippe Laurens, Steven Klocek  
C-                      L1C -> L1SIM, and major upgrade to use LSM, COOR_sim
C-                          Renamed from FIRST_BYTE_DECODING. 
C-                          Changed include file names to current names.
C-                          Modified to work from any array.
C-                          Fixed bug so it does not erase extra word when
C-                            unpacking a multiple of 4 bytes.
C-
C----------------------------------------------------------------------
C
      IMPLICIT NONE
C
      INCLUDE 'D0$PARAMS:BYTE_ORDER.PARAMS'
      INCLUDE 'D0$PARAMS:LEVEL1_LOOKUP.PARAMS'
      INCLUDE 'D0$PARAMS:L1_CALTRIG.PARAMS'
      INCLUDE 'D0$PARAMS:L1_FRAMEWORK.PARAMS'
      INCLUDE 'D0$PARAMS:L1DBB_DATA_BLOCK.PARAMS'
C
      INTEGER    WORD(0:*), FIRST_ADDRESS, NBYTES
      INTEGER    DBLOCK(1:*)
C
      INTEGER ADDRESS
      LOGICAL LOW_WORD
      BYTE TEMP_BYTE
      INTEGER NUM_BYTE, NUM_WORD
      INTEGER WORD_IN, WORD_OUT
      BYTE    BYTE_IN(4), BYTE_OUT(4)
      EQUIVALENCE (WORD_IN, BYTE_IN)
      EQUIVALENCE (WORD_OUT, BYTE_OUT)
C
C----------------------------------------------------------------------
C
      IF (NBYTES .LT. 1) GOTO 999
      ADDRESS = (FIRST_ADDRESS-1) / 2 + 1
      IF (MOD(FIRST_ADDRESS-1, 2) .EQ. 0) THEN
        LOW_WORD = .TRUE.
      ELSE
        LOW_WORD = .FALSE.
      ENDIF
C
      DO NUM_WORD = 0, (NBYTES-1)/4
        WORD(NUM_WORD) = 0
      END DO
C
      DO NUM_BYTE = 0, NBYTES-1
C
        WORD_IN = DBLOCK(ADDRESS)
        IF (LOW_WORD .EQV. .TRUE.) THEN
          TEMP_BYTE = BYTE_IN(BYTE1)
          LOW_WORD = .FALSE.
        ELSE
          TEMP_BYTE = BYTE_IN(BYTE3)
          LOW_WORD = .TRUE.
          ADDRESS = ADDRESS+1
        ENDIF
C
        WORD_OUT = WORD(NUM_BYTE/4)
C
        IF (MOD(NUM_BYTE, 4) .EQ. 0) THEN
          BYTE_OUT(BYTE1) = TEMP_BYTE
        ELSEIF (MOD(NUM_BYTE, 4) .EQ. 1) THEN
          BYTE_OUT(BYTE2) = TEMP_BYTE
        ELSEIF (MOD(NUM_BYTE, 4) .EQ. 2) THEN
          BYTE_OUT(BYTE3) = TEMP_BYTE
        ELSE
          BYTE_OUT(BYTE4) = TEMP_BYTE
        ENDIF
C
        WORD(NUM_BYTE/4) = WORD_OUT
C
      END DO
C
  999 RETURN
C
      END
