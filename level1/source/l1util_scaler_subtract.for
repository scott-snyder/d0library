      SUBROUTINE L1UTIL_SCALER_SUBTRACT(A, B, DIFFERENCE)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Find the difference between two five byte
C-     quantities. The difference is rounded to the nearest 4 byte quantity.
C-     (DIFFERENECE = A - B)
C-
C-   Inputs  : A        Array of 2 integers
C-             B        Array of 2 integers
C-   Outputs : DIFFERENCE the 32 bit difference A - B
C-   Controls: none
C-
C-   Created  16-JAN-1992   Philippe Laurens, Steven Klocek
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
C
      INTEGER LOW_WORD, HIGH_WORD
      PARAMETER (LOW_WORD = 1, HIGH_WORD = 2)
      INTEGER BYTE_LENGTH
      PARAMETER (BYTE_LENGTH = 8)
C
      INTEGER A(2), B(2), DIFFERENCE
      INTEGER INTA(2), INTB(2), INTC(2)
      INTEGER SWAP
      LOGICAL NEGATIVE
      INTEGER BIGNUM
      PARAMETER (BIGNUM = 2147483647) ! 2**31 - 1
C
C       1 BYTE:4 BYTES => 2 BYTES:3 BYTES
C
      INTA(LOW_WORD) = IBITS(A(LOW_WORD), 0, 3*BYTE_LENGTH)
      INTA(HIGH_WORD) = IBITS(A(LOW_WORD), 3*BYTE_LENGTH, BYTE_LENGTH)
      CALL MVBITS(A(HIGH_WORD), 0, BYTE_LENGTH, 
     &  INTA(HIGH_WORD), BYTE_LENGTH)
      INTB(LOW_WORD) = IBITS(B(LOW_WORD), 0, 3*BYTE_LENGTH)
      INTB(HIGH_WORD) = IBITS(B(LOW_WORD), 3*BYTE_LENGTH, BYTE_LENGTH)
      CALL MVBITS(B(HIGH_WORD), 0, BYTE_LENGTH, 
     &  INTB(HIGH_WORD), BYTE_LENGTH)
C
      IF ((INTA(HIGH_WORD) .LT. INTB(HIGH_WORD)) 
     &  .OR. ((INTA(LOW_WORD) .LT. INTB(LOW_WORD)) 
     &        .AND. (INTA(HIGH_WORD) .LE. INTB(HIGH_WORD)))) THEN
C
        SWAP = INTA(LOW_WORD)
        INTA(LOW_WORD) = INTB(LOW_WORD)
        INTB(LOW_WORD) = SWAP
        SWAP = INTA(HIGH_WORD)
        INTA(HIGH_WORD) = INTB(HIGH_WORD)
        INTB(HIGH_WORD) = SWAP
        NEGATIVE = .TRUE.
C
      ELSE
        NEGATIVE = .FALSE.
      ENDIF
C
      IF (INTA(LOW_WORD) .LT. INTB(LOW_WORD)) THEN
        INTA(LOW_WORD) = INTA(LOW_WORD) + 2**(BYTE_LENGTH*3)
        INTA(HIGH_WORD) = INTA(HIGH_WORD) - 1
      ENDIF
      INTC(LOW_WORD) = INTA(LOW_WORD) - INTB(LOW_WORD)
      INTC(HIGH_WORD) = INTA(HIGH_WORD) - INTB(HIGH_WORD)
C
      IF (INTC(HIGH_WORD) .GE. 2**BYTE_LENGTH) THEN
        DIFFERENCE = BIGNUM
      ELSE
        DIFFERENCE = INTC(LOW_WORD)
        CALL MVBITS(INTC(HIGH_WORD),0, BYTE_LENGTH-1, 
     &    DIFFERENCE, BYTE_LENGTH*3)
      ENDIF
C
      IF (NEGATIVE .EQV. .TRUE.) THEN
        DIFFERENCE = -DIFFERENCE 
      ENDIF
C----------------------------------------------------------------------
  999 RETURN
      END
