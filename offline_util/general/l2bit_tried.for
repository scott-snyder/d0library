      FUNCTION L2BIT_TRIED(I)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-
C-      find out if Level 2 bit was TRIED, where TRIED means that 
C-        the filter script was actually run
C-
C-  RELATED FUNCTIONS: 
C-    L2BIT_ON,L2BIT_PASSED,L2BIT_SET,L2BIT_TRIED,L2BIT_UNBIASED
C-
C-   Inputs  : I is 0-127, the level 2 bit number
C-             FILT Bank
C-   Outputs : TRUE if the bit is set
C-   Controls: NONE
C-
C-   Created 22-OCT-1991   James T. Linnemann   
C-   Updated 11-Mar-1992   Herbert B. Greenlee
C-     Got rid of machine block
C-  
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL L2BIT_TRIED
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZFILT.LINK'
      INTEGER LFILT, GZFILT, I, J, L2_WORD, MY_BIT
      LOGICAL BTEST
C----------------------------------------------------------------------
      LFILT = GZFILT()
      IF (I.LT.0 .OR. I.GE.128 .OR. LFILT.LE.0) THEN 
        L2BIT_TRIED = .FALSE.  ! CHECK FOR I OUT OF RANGE, LFILT BAD
        RETURN
      END IF
      J = INT(I/32)
      MY_BIT = MOD(I,32)
      L2_WORD = IQ(LFILT+6+J)
      L2BIT_TRIED = BTEST(L2_WORD,MY_BIT)
C
C
  999 RETURN
      END
