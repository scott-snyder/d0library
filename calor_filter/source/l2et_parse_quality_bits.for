      SUBROUTINE L2ET_PARSE_QUALITY_BITS(QUALITY_BITS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : sets information about the l2 calculation
C-                         of the global quantities for ESUM flag.
C-
C-   Inputs  : none
C-   Outputs : QUALITY_BITS
C-   Controls:
C-
C-   Created   8-MAR-1993   Amber S. Boehnlein
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER QUALITY_BITS
      INTEGER BITS, CURRENT_EVENT
      LOGICAL MICRO_BLANK
      INTEGER NBAD
      INTEGER HOT_CELL_CORRECTION_BIT,MICRO_BLANK_CORRECTION_BIT
      PARAMETER(HOT_CELL_CORRECTION_BIT = 0)
      PARAMETER(MICRO_BLANK_CORRECTION_BIT = 1)
      INTEGER IER
      SAVE BITS, CURRENT_EVENT
      DATA CURRENT_EVENT/-987654/
C----------------------------------------------------------------------
      IF (CURRENT_EVENT.NE.IQ(LHEAD+7)) THEN
        BITS = 0
        CALL CL2_BAD_CELL(NBAD)
        IF(NBAD.GE.1)BITS = IBSET(BITS,
     &    HOT_CELL_CORRECTION_BIT)
        IF(MICRO_BLANK(IER))BITS = IBSET(BITS,
     &    MICRO_BLANK_CORRECTION_BIT)
        CURRENT_EVENT = IQ(LHEAD+7)
      ENDIF
      QUALITY_BITS=BITS
  999 RETURN
      END
