      FUNCTION L1ESUM_EVT()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Controlling routine for Level 1 ESUM filling.
C-
C-   Inputs  : TRGR bank/l1sim commons
C-   Outputs : ESUM bank 
C-   Controls: 
C-
C-   Created  11-JAN-1992   Amber S. Boehnlein
C-   Modified 21-DEC-1993   G. Snow, to call filling routine for L1.5
C-                          ESUM bank
C-   Modified 22-MAY-1995   G. Snow, to call filling routine for
C-                          CT15 Zebra bank
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL L1ESUM_EVT
C----------------------------------------------------------------------
      CALL L1ESUM_TRGR
C
C-    Call L1.5 ESUM bank filling
C
      CALL L15CAL_ESUM_FILL
      CALL CT15FL
C
      L1ESUM_EVT=.TRUE.
  999 RETURN
      END
