       FUNCTION L2NAME_ON(L2FILTNAME)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-
C-      This routine uses the TSUM banks to interpret a Level 2 filter 
C-      NAME and determine whether the corresponding BIT was on.
C-      It is intended to buffer the user from the changing bit numbers
C-      corresponding to different trigger menus and run configuraion.
C-      Written purely for convenience, no originality claimed.
C-
C-      The following remarks were lifted from the routine L2BIT_ON.FOR
C-      and should be relevant to users of this code.
C-
C-      
C-      find out if Level 2 Filter was ON, where ON means that 
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
C-    L2NAME_PASSED
C-
C-   Inputs  : L2FILTNAME a character variable 
C-            
C-   Outputs : TRUE if the bit corresponding to L2FILTNAME is ON.
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
      LOGICAL L2NAME_ON
      LOGICAL FOUND
      CHARACTER*32 L2FILTNAME
      CHARACTER*32 TRIGNON(32),FILTNON(128)
      INTEGER NTRIGON,TRIGBON(32),NFILTON,FILTBON(128)
      INTEGER BITNUM
C
C----------------------------------------------------------------------
C
        L2NAME_ON = .FALSE.
C
C ****  Interpret TSUM bank; get list of triggers which are ON.
C
        CALL GTTSUM(NTRIGON,TRIGBON,TRIGNON,NFILTON,FILTBON,FILTNON)
C
C ****  Check specific filter name. (entry point in GTTSUM which needs 
C ****  to be set up by above call). 
C
        CALL GET_L2_BIT_NUMBER(L2FILTNAME,BITNUM,FOUND)
        L2NAME_ON = FOUND
C
  999 RETURN
      END
