      FUNCTION L1_PASSED()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-
C-      find out if ANY Level 1 bit was PASSED
C-
C-  RELATED FUNCTIONS: 
C-    L1BIT_ON,L1BIT_PASSED
C-
C-   Inputs  : HEAD Bank
C-   Outputs : TRUE if the bit is PASSED
C-   Controls: NONE
C-
C-        uses NUMBIT, a CERNLIB routine for counting bits set
C-
C-   Created  6-DEC-1991   Amber S. Boehnlein, Created L1_PASSED from
C-                                             L2_PASSED   
C-  
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL L1_PASSED
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER NUMBIT
C----------------------------------------------------------------------
      L1_PASSED = .FALSE. 
      IF  (LHEAD.GT.0) THEN 
        L1_PASSED =  NUMBIT(IQ(LHEAD+11)).NE.0 
      END IF
  999 RETURN
      END
