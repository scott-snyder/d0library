      SUBROUTINE DGN_GET_TAGS(MAXN,NTAGNAME,TAGNAME)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return tags specified in RCP bank supplied tp
C-   DGN_BEGIN.
C-
C-   Inputs  : MAXN       [I]   Maximum number of tags
C-            
C-   Outputs : NTAGNAME   [I]   Number of tags
C-             TAGNAME(*) [C*]  Tagnames
C-   Controls: 
C-
C-   Created  10-MAR-1995   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER MAXN, NTAGNAME
      CHARACTER*(*) TAGNAME(*)
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:DGNCOM.INC'
C----------------------------------------------------------------------
      INTEGER I, N
C----------------------------------------------------------------------
      NTAGNAME = NFIELD
      N = MIN(MAXN,NTAGNAME)
      DO I =  1, N
        TAGNAME(I) = FIELD(I)
      ENDDO
  999 RETURN
      END
