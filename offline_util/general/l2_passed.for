      FUNCTION L2_PASSED()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-
C-      find out if ANY Level 2 bit was PASSED, where PASSED means that 
C-        the filter script was TRIED and PASSED
C-
C-  RELATED FUNCTIONS: 
C-    L2BIT_ON,L2BIT_PASSED,L2BIT_SET,L2BIT_TRIED,L2BIT_UNBIASED
C-
C-   Inputs  : FILT Bank
C-   Outputs : TRUE if the bit is PASSED
C-   Controls: NONE
C-
C-        uses NUMBIT, a CERNLIB routine for counting bits set
C-
C-   Created 27-OCT-1991   James T. Linnemann   
C-   Updated  12-DEC-1991   James T. Linnemann run from HEAD instead of FILT 
C-  
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL L2_PASSED
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER J, NUMBIT
C----------------------------------------------------------------------
      L2_PASSED = .FALSE. 
      IF  (LHEAD.GT.0) THEN 
        DO J = 0,3
          L2_PASSED = L2_PASSED.OR.( NUMBIT(IQ(LHEAD+15+J)).NE.0 ) 
        ENDDO
      END IF
  999 RETURN
      END
