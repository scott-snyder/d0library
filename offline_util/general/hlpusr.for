      SUBROUTINE HLPUSR (PRTID,PRT,TEXT,NLINES)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return HELP text for given prompt (ID,PRT)
C-
C-   Inputs  : PRTID       Prompt identifier
C-             PRT         Prompt
C-
C-   Outputs : TEXT(*)     Lines of help text
C-             NLINES      Number of lines of text
C-
C-   Created  22-APR-1988   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER       PRTID,NLINES
      CHARACTER*(*) PRT,TEXT(*)
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      NLINES = 1
      TEXT(1)= ' Sorry NO HELP as yet! '
  999 RETURN
      END
