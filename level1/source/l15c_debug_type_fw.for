      SUBROUTINE L15C_DEBUG_TYPE_FW(INDEX,WDS_T_FLW,TYPE,ID)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Write the first word of "type blocks" in DEBUG
C-                         section of the L15CAL simulator common block
C-
C-   Inputs  : INDEX = pointer to word in common array L15CAL_DEBUG_BLOCK
C-                     WDS_T_FLW = # of words to follow. goes in most significant
C-                     two bytes (bytes 3 & 4)
C-             TYPE =  block type number; goes in byte 2
C-             ID   =  Id of the device the data is from (68K or DSP ID)
C-   Outputs :  writes packed result into L15CAL_DEBUG_BLOCK(index)
C-   Controls: 
C-
C-   Created  12-MAY-1994   Dan Owen
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:L15CALDBB_DATA_BLOCK.PARAMS'
      INCLUDE 'D0$INC:L15CALDBB_DATA_BLOCK.INC'
C     Parameters to be used with the BITBYT CernLib package
C
      INTEGER    BYTE_LENGTH,     WORD_LENGTH,
     +           LONG_WORD_LENGTH
      PARAMETER (BYTE_LENGTH      = 8,
     +           WORD_LENGTH      = 2 * BYTE_LENGTH,
     +           LONG_WORD_LENGTH = 4 * BYTE_LENGTH)
C
      INTEGER    FIRST_BYTE,   SECOND_BYTE,
     +           THIRD_BYTE,   FOURTH_BYTE,
     +           FIRST_WORD,   SECOND_WORD
      PARAMETER (FIRST_BYTE  = 1,
     +           SECOND_BYTE = FIRST_BYTE  + BYTE_LENGTH,
     +           THIRD_BYTE  = SECOND_BYTE + BYTE_LENGTH,
     +           FOURTH_BYTE = THIRD_BYTE  + BYTE_LENGTH,
     +           FIRST_WORD  = 1,
     +           SECOND_WORD = FIRST_WORD  + WORD_LENGTH)
      INTEGER INDEX, WDS_T_FLW, TYPE, ID
C----------------------------------------------------------------------
      L15CAL_DEBUG_BLOCK(INDEX) = ID
      CALL SBYT(TYPE,L15CAL_DEBUG_BLOCK(INDEX),SECOND_BYTE,BYTE_LENGTH)
      CALL SBYT(WDS_T_FLW,L15CAL_DEBUG_BLOCK(INDEX),SECOND_WORD,
     &  WORD_LENGTH)
  999 RETURN
      END
