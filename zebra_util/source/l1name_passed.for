       FUNCTION L1NAME_PASSED(L1TRIGNAME)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-
C-      This routine uses the TSUM banks to interpret a Level 1 trigger
C-      NAME and determine whether the corresponding BIT was on.
C-      It is intended to buffer the user from the changing bit numbers
C-      corresponding to different trigger menus and run configuraion.
C-      Written purely for convenience, no originality claimed.
C-
C-   Inputs  : L1TRIGNAME a character variable 32 characters or less
C-            
C-   Outputs : TRUE if the bit corresponding to L1TRIGNAME is ON.
C-             Since the routine GET_L1_BIT_NUMBER checks the length
C-             of this character variable and only matches the 
C-             corresponding part of the trigger names in TSUM, partial
C-             names of L1 triggers are allowable. 
C-             
C-   Controls: NONE
C-
C-   Created   5-DEC-1992   Norman A. Graf 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL L1NAME_PASSED
      LOGICAL FOUND
      CHARACTER*(*) L1TRIGNAME
      CHARACTER*32 TRIGNON(32),FILTNON(128)
      INTEGER NTRIGON,TRIGBON(32),NFILTON,FILTBON(128)
      INTEGER BITNUM
C
C----------------------------------------------------------------------
C
        L1NAME_PASSED = .FALSE.
C
C ****  Interpret TSUM bank; get list of triggers which are ON.
C
        CALL GTTSUM(NTRIGON,TRIGBON,TRIGNON,NFILTON,FILTBON,FILTNON)
C
C ****  Check specific trigger name. (entry point in GTTSUM which needs 
C ****  to be set up by above call). 
C
        CALL GET_L1_BIT_NUMBER(L1TRIGNAME,BITNUM,FOUND)
        L1NAME_PASSED = FOUND
C
  999 RETURN
      END
