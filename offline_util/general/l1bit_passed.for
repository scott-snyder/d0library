      FUNCTION L1BIT_PASSED(I)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-
C-      find out if Level 1 bit was PASSED
C-
C-  RELATED FUNCTIONS: 
C-    L1BIT_ON,L1_PASSED
C-
C-   Inputs  : I is 0-31, the level 1 bit number
C-             HEAD Bank
C-   Outputs : TRUE if the bit is PASSED
C-   Controls: NONE
C-
C-   Created  6-DEC-1991   Amber Boehnlein, in the fashion of L2BIT_PASSED
C-   Updated 11-Mar-1992   Herbert B. Greenlee
C-      UNIX update.  Got rid of machine block
C-  
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL L1BIT_PASSED
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER  I, L1_WORD 
      LOGICAL BTEST
C----------------------------------------------------------------------
      IF (I.LT.0 .OR. I.GE.32 .OR. LHEAD.LE.0) THEN 
        L1BIT_PASSED = .FALSE.  ! CHECK FOR I OUT OF RANGE, LHEAD BAD
        RETURN
      END IF
      L1_WORD = IQ(LHEAD+11)
      L1BIT_PASSED = BTEST(L1_WORD,I)
C
C
  999 RETURN
      END
