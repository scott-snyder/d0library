      SUBROUTINE L1UTIL_GET_FOREIGN_SCALER ( L1_BLOCK,
     &                                       SCALER_NUMBER,
     &                                       SCALER_COUNT )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Extract from the Level 1 crate data in the TRGR bank
C-                         a 40 bit foreign scaler count
C-
C-   Inputs  : L1_BLOCK [I]
C-
C-                First word of the Level-1 Crate header in the TRGR bank.
C-                -> Use IQ( LTRGR_LEVEL1 )
C-                Where LTRGR_LEVEL1 = GZFIND_CRATE ( 'TRGR', GZTRGR(), 11 )
C-                cf. header of D0$ZEBRA_UTIL$SOURCE:GZFIND_CRATE.FOR for details
C-
C-             SCALER_NUMBER [I]
C-
C-                ID number of the foreign scaler to be returned.
C-                The Level 1 Data contains 44 Foreign Scalers
C-                with ID number 1 to 44
C-
C-   Outputs : SCALER_COUNT [I]
C-
C-                array of two 32-bit integers holding the 40-bit Foreign Scaler
C-                selected.
C-                SCALER_COUNT (1) has the low  24 bits
C-                SCALER_COUNT (2) has the high 16 bits,
C-                (unused bits are padded with zero)
C-
C-   Controls: none
C-
C-   Created  28-APR-1992   Philippe Laurens
C-   Updated  12-JUN-1992   Philippe Laurens, Steven Klocek  
C-                      Modified to use parameters from
C-                      L1DBB_DATA_BLOCK.PARAMS for Data Block addressing.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:BYTE_ORDER.PARAMS'
      INCLUDE 'D0$PARAMS:LEVEL1_LOOKUP.PARAMS/LIST'
      INCLUDE 'D0$PARAMS:L1_CALTRIG.PARAMS/LIST'
      INCLUDE 'D0$PARAMS:L1_FRAMEWORK.PARAMS/LIST'
      INCLUDE 'D0$PARAMS:L1DBB_DATA_BLOCK.PARAMS/LIST'
C
C *** Explicit Subroutine Arguments
C
      INTEGER        L1_BLOCK(0:DATA_BLOCK_MAX)
      INTEGER        SCALER_NUMBER
      INTEGER        LOW, HIGH
      PARAMETER    ( LOW  = 1, HIGH = 2 )
      INTEGER        SCALER_COUNT (LOW:HIGH)
C
C *** Local Variables
C
      INTEGER        SCALER_ITEM_OFFSET
      INTEGER        SCALER_WORD_OFFSET
C
      BYTE           TRGR_BYT(1:4)
      INTEGER        TRGR_INT
      EQUIVALENCE  ( TRGR_INT, TRGR_BYT )
C
      BYTE           SCALER_BYT(1:4)
      INTEGER        SCALER_INT
      EQUIVALENCE  ( SCALER_INT, SCALER_BYT )
C
      CHARACTER*75   LINE
C
C *********************************************************************
C
      IF (   ( SCALER_NUMBER .LT.  1 )
     &  .OR. ( SCALER_NUMBER .GT. 44 ) ) THEN
        WRITE ( LINE, 1000 ) SCALER_NUMBER
 1000   FORMAT ( 'Scaler Number ', I6, ' out of Range')
        CALL ERRMSG( LINE, 'L1UTIL_GET_FOREIGN_SCALER',
     &                     ' Bad Argument ', 'W')
        SCALER_COUNT(LOW) = 0
        SCALER_COUNT(HIGH)= 0
        GOTO 999
      END IF
C
C       cf D0 note 967 for explanation of item number notation
      SCALER_ITEM_OFFSET = MISC_FOREIGN 
     &                     + (44 - SCALER_NUMBER) * SCALER_L
      SCALER_WORD_OFFSET = TRGR_HEADER_LENGTH
     &                   + ( SCALER_ITEM_OFFSET + 1 ) / 2
C
C       cf D0 note 967 for explanation of level 1 "item" addressing
      IF ( IAND ( SCALER_ITEM_OFFSET, 1 ) .EQ. 0 ) THEN
C
C       EVEN item number ->  the first byte of the scaler is in
C       byte # 3 of the zebra longword.
C
C       find and pack the Least Significant 3 bytes of the scaler count
        SCALER_INT          = 0
        TRGR_INT            = L1_BLOCK( SCALER_WORD_OFFSET + 0)
        SCALER_BYT(BYTE1)   = TRGR_BYT( BYTE3 )
        TRGR_INT            = L1_BLOCK( SCALER_WORD_OFFSET + 1)
        SCALER_BYT(BYTE2)   = TRGR_BYT( BYTE1 )
        SCALER_BYT(BYTE3)   = TRGR_BYT( BYTE3 )
        SCALER_COUNT(LOW)   = SCALER_INT
C       find and pack the Most Significant 2 bytes of the scaler count
        SCALER_INT          = 0
        TRGR_INT            = L1_BLOCK( SCALER_WORD_OFFSET + 2)
        SCALER_BYT(BYTE1)   = TRGR_BYT( BYTE1 )
        SCALER_BYT(BYTE2)   = TRGR_BYT( BYTE3 )
        SCALER_COUNT(HIGH)  = SCALER_INT
C
      ELSE
C       ODD item number ->  the first byte of the scaler is in
C       byte # 1 of the zebra longword.
C
C       find and pack the Least Significant 3 bytes of the scaler count
        SCALER_INT          = 0
        TRGR_INT            = L1_BLOCK( SCALER_WORD_OFFSET + 0)
        SCALER_BYT(BYTE1)   = TRGR_BYT( BYTE1 )
        SCALER_BYT(BYTE2)   = TRGR_BYT( BYTE3 )
        TRGR_INT            = L1_BLOCK( SCALER_WORD_OFFSET + 1)
        SCALER_BYT(BYTE3)   = TRGR_BYT( BYTE1 )
        SCALER_COUNT(LOW)   = SCALER_INT
C       find and pack the Most Significant 2 bytes of the scaler count
        SCALER_INT          = 0
        SCALER_BYT(BYTE1)   = TRGR_BYT( BYTE3 )
        TRGR_INT            = L1_BLOCK( SCALER_WORD_OFFSET + 2)
        SCALER_BYT(BYTE2)   = TRGR_BYT( BYTE1 )
        SCALER_COUNT(HIGH)  = SCALER_INT
C
      END IF
C
C----------------------------------------------------------------------
  999 RETURN
      END
