      FUNCTION L2BIT_PASSED(I)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-
C-      find out if Level 2 bit was PASSED, where PASSED means that
C-        the filter script was TRIED and PASSED
C-
C-    HOWEVER, if the FILT bank has been dropped, report instead ON from
C-      the event header bank: ON means that either the filter script was
C-      TRIED and PASSED, or that the corresponding level 1 bit was on, but
C-      the filter script was never TRIED because another filter script passed
C-      and this filter script was set up as TRY_AS_NEEDED rather than MUST_TRY
C-
C-  RELATED FUNCTIONS:
C-    L2BIT_ON,L2BIT_PASSED,L2BIT_SET,L2BIT_TRIED,L2BIT_UNBIASED
C-
C-   Inputs  : I is 0-127, the level 2 bit number
C-             FILT Bank
C-   Outputs : TRUE if the bit is PASSED
C-   Controls: NONE
C-
C-   Created  23-SEP-1991   Nicholas Hadley
C-   Updated  22-OCT-1991   James T. Linnemann   documentation; add related
C-                                                functions
C-   Updated  12-DEC-1991   James T. Linnemann   allow to run from event HEAD
C-   Updated 11-Mar-1992   Herbert B. Greenlee
C-     Got rid of machine block
C-   Updated  13-APR-1995   James T. Linnemann
C-                say passed if timed out or ran out of space
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL L2BIT_PASSED
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZFILT.LINK'
      INTEGER LFILT, GZFILT, I, J, L2_WORD, MY_BIT, IAND, IOR, NOT
      LOGICAL BTEST
C----------------------------------------------------------------------
      L2BIT_PASSED = .FALSE.
      IF (I.LT.0 .OR. I.GE.128 )  RETURN
      J = INT(I/32)
      LFILT = GZFILT()
      IF (LHEAD.GT.0) THEN
        L2_WORD = IQ(LHEAD+15+J) !no FILT bank; use ON bits from event header
        IF (LFILT.GT.0 ) THEN
          L2_WORD = IQ(LFILT+10+J) ! true PASSED bits
C
C... use actual passed bits IF the event processing was finished
          IF( (IQ(LFILT+1).GT.5) .AND. (IQ(LFILT+19).NE.0) ) THEN
C            FILT version 6 or more, and event timed out or ran out of space
C             will call it passed if it never really came to a decision on bit
            IF (IQ(LFILT+20).EQ.I) THEN
C               interrupted processing of this L2 bit; take as passed if set
C                           (set means L1 bit was on, which it must have been)
              L2_WORD = IQ(LFILT+2+J) !use set bits
            ELSE
C               pass = really passed .or. (set and .not.tried)
              L2_WORD = IOR(L2_WORD,
     &          IAND(IQ(LFILT+2+J),NOT(IQ(LFILT+6+J))))
            ENDIF
          ENDIF
        ENDIF
      ELSE
        RETURN  ! no bank with information so say no bits found on
      END IF
      MY_BIT = MOD(I,32)
      L2BIT_PASSED = BTEST(L2_WORD,MY_BIT)
C
C
  999 RETURN
      END
