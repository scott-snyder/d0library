      SUBROUTINE GET_TYPE_INTERACT(KCASE,TYPE)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Encode KCASE (Character string) to TYPE (integer)
C-
C-   Inputs  : KCASE - Geant interaction type
C-   Outputs : TYPE  - Encoded interaction type
C-   Controls:
C-
C-   Created  29-SEP-1989   Chip Stewart
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER KCASE
      INTEGER TYPE
C----------------------------------------------------------------------
      TYPE = 0
      IF ( KCASE .EQ. 4hDCAY ) THEN
        TYPE = 11
      ELSEIF ( KCASE .EQ. 4hHADR ) THEN
        TYPE = 12
      ELSEIF ( KCASE .EQ. 4hMUNU ) THEN
        TYPE = 13
      ELSEIF ( KCASE .EQ. 4hPFIS ) THEN
        TYPE = 14
      ELSEIF ( KCASE .EQ. 4hPAIR ) THEN
        TYPE = 15
      ELSEIF ( KCASE .EQ. 4hCOMP ) THEN
        TYPE = 16
      ELSEIF ( KCASE .EQ. 4hPHOT ) THEN
        TYPE = 17
      ELSEIF ( KCASE .EQ. 4hANNI ) THEN
        TYPE = 18
      ELSEIF ( KCASE .EQ. 4hBREM ) THEN
        TYPE = 22
      ELSEIF ( KCASE .EQ. 4hDRAY ) THEN
        TYPE = 23
      ELSEIF ( KCASE .EQ. 4hSTOP ) THEN
        TYPE = 999
      ENDIF
  999 RETURN
      END
