      FUNCTION L2BIT_ON(I)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-
C-      find out if Level 2 bit was ON, where ON means that 
C-        a) the filter script was PASSED, or
C-        b) the filter script had its corresponding level 1 bit on, but was 
C-          never TRIED because another filter bit caused the event to pass, or
C-        c) the filter script was run as a part of the UNBIASED sample
C-
C-      the bit being ON means that as far as this filter script is concerned,
C-        there is no objection to writing the event.  This is the bit used to
C-        steer the events to the output streams; if condition b) holds, the
C-        filter has never run ("stream pollution").  If necessary, this can be
C-        avoided by setting up this script as MUST_TRY in the COOR 
C-        configuration file.
C-
C-  RELATED FUNCTIONS: 
C-    L2BIT_ON,L2BIT_WRITE,L2BIT_PASSED,L2BIT_SET,L2BIT_TRIED,L2BIT_UNBIASED
C-
C-   Inputs  : I is 0-127, the level 2 bit number
C-             HEAD Bank
C-   Outputs : TRUE if the bit is ON
C-   Controls: NONE
C-
C-   Created 22-OCT-1991   James T. Linnemann   
C-   Updated 11-Mar-1992   Herbert B. Greenlee
C-     Got rid of machine block
C-   Modified 30-Dec-1995   sss - compile with g77.
C-  
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL L2BIT_ON,L2BIT_WRITE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER I, J, L2_WORD, MY_BIT
      LOGICAL BTEST
C----------------------------------------------------------------------
      ENTRY L2BIT_WRITE(I)
      IF (I.LT.0 .OR. I.GE.128 .OR. LHEAD.LE.0) THEN 
        L2BIT_ON = .FALSE.  ! CHECK FOR I OUT OF RANGE, LHEAD BAD
       GO TO 999
      END IF
      J = INT(I/32)
      MY_BIT = MOD(I,32)
      L2_WORD = IQ(LHEAD+15+J)
      L2BIT_ON = BTEST(L2_WORD,MY_BIT)
  999 CONTINUE
      L2BIT_WRITE = L2BIT_ON
      RETURN
      END
