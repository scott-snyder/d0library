      FUNCTION L2BIT_UNBIASED(I)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-      find out if Level 2 bit was UNBIASED, where UNBIASED means that
C-        the filter script was TRIED, and the event was, written without
C-        regard to whether or not it PASSED, because of a prescaling selection
C-        which made it one of an UNBIASED sample.
C-
C-  RELATED FUNCTIONS:
C-    L2BIT_ON,L2BIT_PASSED,L2BIT_SET,L2BIT_TRIED,L2BIT_UNBIASED
C-
C-   Inputs  : I is 0-127, the level 2 bit number
C-            FILT BANK
C-   Outputs : TRUE if the bit is UNBIASED
C-   Controls: NONE
C-
C-   Created 22-OCT-1991   James T. Linnemann
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZFILT.LINK'
      INCLUDE 'D0$PARAMS:MONITOR_BIT.PARAMS'
      INTEGER LFILT, GZFILT, I, J, L2_WORD, MY_BIT
      LOGICAL L2BIT_UNBIASED
      LOGICAL L2BIT_ON,L2BIT_TRIED,L2BIT_PASSED
C----------------------------------------------------------------------
      LOGICAL BTEST
C----------------------------------------------------------------------
      LFILT = GZFILT()
      IF (IQ(LFILT+1).EQ.1) THEN    !old version of FILT bank
C
C...for now, UNBIASED = TRIED.AND..NOT.PASS.AND.ON
C...the ON bit signifies that this event would be written because of this bit
        L2BIT_UNBIASED = L2BIT_ON(I).AND.L2BIT_TRIED(I).AND.
     &    (.NOT.L2BIT_PASSED(I))
      ELSE  !later versions have full information
        IF (I.LT.0 .OR. I.GE.128 .OR. LFILT.LE.0) THEN
          L2BIT_UNBIASED = .FALSE.  ! CHECK FOR I OUT OF RANGE, LFILT BAD
          RETURN
        END IF
        J = INT(I/32)
        MY_BIT = MOD(I,32)
        L2_WORD = IQ(LFILT+14+J)
        L2BIT_UNBIASED = BTEST(L2_WORD,MY_BIT)
      ENDIF
C
C... last chance: from monitor stream? (only way to get L1.5 unbiased)
      IF (.NOT.L2BIT_UNBIASED) L2BIT_UNBIASED = L2BIT_ON(MONITOR_BIT)
  999 RETURN
      END
