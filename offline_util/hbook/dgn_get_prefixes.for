C CMS REPLACEMENT HISTORY, Element DGN_GET_PREFIXES.FOR
C *1    13-JAN-1996 13:12:36 HARRY "Update"
C CMS REPLACEMENT HISTORY, Element DGN_GET_PREFIXES.FOR
      SUBROUTINE DGN_GET_PREFIXES(MAXN,NPREFIXNAME,PREFIXNAME)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return prefixes specified in RCP bank supplied 
C-   to DGN_BEGIN.
C-
C-   Inputs  : MAXN           [I]   Maximum number of prefixes
C-            
C-   Outputs : NPREFIXNAME    [I]   Number of prefixes
C-             PREFIXNAME(*)  [C*]  Prefixes
C-   Controls: 
C-
C-   Created  10-JUL-1995   Harrison B. Prosper   
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER MAXN, NPREFIXNAME
      CHARACTER*(*) PREFIXNAME(*)
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:DGNCOM.INC'
C----------------------------------------------------------------------
      INTEGER I, N
C----------------------------------------------------------------------
      NPREFIXNAME = NFIELD
      N = MIN(MAXN,NPREFIXNAME)
      DO I =  1, N
        PREFIXNAME(I) = PREFIX(I)
      ENDDO
  999 RETURN
      END
