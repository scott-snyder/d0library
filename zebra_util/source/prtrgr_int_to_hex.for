      CHARACTER*8 FUNCTION PRTRGR_INT_TO_HEX (VALUE)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Convert an integer value into a hexadecimal string.
C-      Pad the retuned string with leading zeros.
C-
C-   Returned value  : The hexadecimal representation.
C-   Inputs  : VALUE    The input value
C-   Outputs : none
C-   Controls: none
C-
C-   Created  23-JAN-1992   Philippe Laurens, Steven Klocek
C-
C----------------------------------------------------------------------
      IMPLICIT  NONE
      INTEGER   VALUE
      INTEGER   I, DIGIT
      LOGICAL   NEGATIVE
C
      CHARACTER*1 TO_HEX(0:15)
      DATA TO_HEX / '0', '1', '2', '3', '4', '5', '6', '7', 
     &              '8', '9', 'A', 'B', 'C', 'D', 'E', 'F' /
C
      PRTRGR_INT_TO_HEX = ' '
      I = VALUE
      IF (VALUE .LT. 0) THEN
        NEGATIVE = .TRUE.
      ELSE
        NEGATIVE = .FALSE.
      ENDIF
C
      DO DIGIT = 8, 1, -1
        PRTRGR_INT_TO_HEX(DIGIT:DIGIT) = TO_HEX(IAND(I, 15))
        IF (NEGATIVE .EQV. .FALSE.) THEN
          I = I / 16
        ELSE
          I = NOT(NOT(I) / 16)
        ENDIF
      END DO
C----------------------------------------------------------------------
  999 RETURN
      END
