      SUBROUTINE EZGREM (PARAM1,REMARK,LREM)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Get REMark associated with parameter PARAM1
C-                         from selected SRCP bank. Use EZPICK to select
C-                         an SRCP bank.
C-
C-   Inputs  : PARAM1      Name of parameter (CHARACTER string)
C-
C-   Outputs : REMARK      String containing comment
C-             LREM        Length of comment
C-                         Error code. Use EZERR to check for errors.
C-
C-                           0 --> OK
C-                          -1 --> Variable not found
C-                          -2 --> Bank not found
C-
C-   ENTRY EZSREM (PARAM1,REMARK,LREM)
C-
C-   Controls: None
C-
C-   Created   7-MAR-1989   Harrison B. prosper
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      CHARACTER*(*) PARAM1
      CHARACTER*(*) REMARK
      INTEGER       LREM
C
      INTEGER IER,L,N,ID
      LOGICAL GET
C----------------------------------------------------------------------
      GET = .TRUE.
      GOTO 5
C
C ****  Entry point for setting routine
C
      ENTRY EZSREM (PARAM1,REMARK,LREM)
      GET = .FALSE.
    5 CONTINUE
C
      L = LEN(PARAM1)
      CALL EZGETI (PARAM1(1:L),ID,IER)
      IF ( IER .NE. 0 ) GOTO 999
C
      N = LEN(REMARK)
      IF ( GET ) THEN
        CALL EZZGRM (ID,REMARK(1:N),LREM)
      ELSE
        CALL EZZSRM (ID,REMARK(1:N),LREM)
      ENDIF
C
  999 RETURN
      END
