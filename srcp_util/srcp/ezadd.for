      SUBROUTINE EZADD (RECORD,NREC,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Add the parameter(s) specified in the array
C-   of strings RECORD(*).
C-
C-   Inputs  : RECORD(*)        [C*]    One or more strings
C-             NREC             [I]     Number of strings
C-   Outputs : IER              [I]     Error code
C-   Controls:
C-
C-   Created  11-NOV-1989   Harrison B. Prosper
C-   Updated  26-FEB-1991   Harrison B. Prosper
C-      Do not use LEN to get length of records
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) RECORD(*)
      INTEGER       NREC
      INTEGER       IER
C----------------------------------------------------------------------
      INCLUDE 'D0$PARAMS:SRCP.DEF'
C----------------------------------------------------------------------
      CHARACTER*(CHRCRD) STRING
      INTEGER L,I,LSRCP,LPTI,LPTO,LPTV,LPTT
C----------------------------------------------------------------------
C
C ****  Get pointers into RCP bank
C
      CALL EZZGPT (LSRCP,LPTI,LPTO,LPTV,LPTT)
C
C ****  Insert records into RCP bank
C
      DO I = 1 , NREC
        STRING = RECORD(I)              ! STRING could be altered
        CALL EZZIRC (STRING,LSRCP,LPTI,LPTO,LPTV,LPTT,IER)
      ENDDO
C
  999 RETURN
      END
