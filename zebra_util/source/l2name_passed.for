       FUNCTION L2NAME_PASSED(L2FILTNAME)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-
C-      This routine uses the TSUM banks to interpret a Level 2 filter 
C-      NAME and determine whether the corresponding BIT was passed.
C-      It is intended to buffer the user from the changing bit numbers
C-      corresponding to different trigger menus and run configuraions.
C-      Written purely for convenience, no originality claimed.
C-
C-      The following remarks were lifted from the routine L2BIT_PASSED.FOR
C-      and should be relevant to users of this code.
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
C-    L2BIT_ON,L2BIT_WRITE,L2BIT_PASSED,L2BIT_SET,L2BIT_TRIED,L2BIT_UNBIASED
C-    L2NAME_ON
C-
C-   Inputs  : L2FILTNAME is a character variable 32 characters or less
C-            
C-   Outputs : TRUE if the bit corresponding to L2FILTNAME is PASSED.
C-             Since the routine GET_L2_BIT_NUMBER checks the length
C-             of this character variable and only matches the 
C-             corresponding part of the filter names in TSUM, partial
C-             names of L2 filters are allowable. 
C-             
C-   Controls: NONE
C-
C-   Created   5-DEC-1992   Norman A. Graf
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL L2NAME_PASSED
      LOGICAL FOUND,L2BIT_PASSED
      CHARACTER*(*) L2FILTNAME
      CHARACTER*32 TRIGNON(32),FILTNON(128)
      INTEGER NTRIGON,TRIGBON(32),NFILTON,FILTBON(128)
      INTEGER BITNUM
C
C----------------------------------------------------------------------
C
        L2NAME_PASSED = .FALSE.
C
C ****  Interpret TSUM bank; get list of triggers which are ON.
C
        CALL GTTSUM(NTRIGON,TRIGBON,TRIGNON,NFILTON,FILTBON,FILTNON)
C
C ****  Check specific filter name. (CALLS entry point in GTTSUM 
C ****  which needs to be set up by above call). 
C
        CALL GET_L2_BIT_NUMBER(L2FILTNAME,BITNUM,FOUND)
        IF(FOUND) L2NAME_PASSED = L2BIT_PASSED(BITNUM)
C
  999 RETURN
      END
