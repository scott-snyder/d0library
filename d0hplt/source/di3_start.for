      SUBROUTINE DI3_START(DEVICE)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Open Di3000 window.
C-
C-   Inputs  : DEVICE   [I]     Device number
C-   Outputs : 
C-   Controls: 
C-
C-   Created  20-FEB-1991   Harrison B. Prosper, Sharon Hagopian
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER DEVICE
      LOGICAL FIRST
      DATA FIRST/.TRUE./
      SAVE FIRST
C----------------------------------------------------------------------
      IF ( FIRST ) THEN
        CALL HPLINT(DEVICE)
        FIRST = .FALSE.
      ENDIF
      RETURN
C
      ENTRY DI3_END
      IF ( .NOT. FIRST ) THEN
        CALL HPLEND
      ENDIF
  999 RETURN
      END
