      SUBROUTINE EZGETR (PARAM1,IVAL,TYPE,TOTAL,REMARK,LREM)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return data associated with the identifier
C-                         PARAM1 from a pre-selected SRCP bank. See
C-                         also EZGETD.
C-
C-   Inputs  : PARAM1   [C*]    Name of parameter (CHARACTER string)
C-
C-   Outputs : IVAL(*)          Value(s) pertaining to PARAM1.
C-             TYPE(*)  [I*]    Value type(s)
C-             TOTAL    [I]     Total number of values
C-             REMARK   [C*]    Remark associated with parameter
C-             LREM     [I]     Length of remark
C-
C-                         Error code. Use EZERR to check for errors.
C-
C-                           0 --> OK
C-                          -1 --> Variable not found
C-                          -2 --> Bank not found
C-
C-   ENTRY EZSETR (PARAM1,IVAL,TYPE,TOTAL,REMARK,LREM)
C-
C-   Controls: None
C-
C-   Created   7-MAR-1989   Harrison B. prosper
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      CHARACTER*(*) PARAM1
      INTEGER       IVAL(*)
      INTEGER       TYPE(*)
      INTEGER       TOTAL
      CHARACTER*(*) REMARK
      INTEGER       LREM
C
      INTEGER IER,ID,L,LENGIN,KSTART,KEND,KSTEP,NUMVAL
      LOGICAL GET
      CHARACTER*32  PARAM
C
C----------------------------------------------------------------------
      GET = .TRUE.
      GOTO 5
C
C ****  Entry point for setting routine
C
      ENTRY EZSETR (PARAM1,IVAL,TYPE,TOTAL,REMARK,LREM)
      GET = .FALSE.
    5 CONTINUE
C
C ****  Decode parameter name
C
      L = LEN(PARAM1)
      CALL EZZDCD (PARAM1(1:L),PARAM,LENGIN,KSTART,KEND,KSTEP)
C
C ****  Get parameter ID
C
      CALL EZGETI (PARAM,ID,IER)
      IF ( IER .NE. 0 ) GOTO 999

      IF ( GET ) THEN
C
C ****  Get values and value-types
C
        CALL EZGET2 (ID,KSTART,KEND,KSTEP,IVAL,TYPE,NUMVAL,TOTAL,IER)
        IF ( IER .NE. 0 ) GOTO 999
C
C ****  Get remark
C
        CALL EZZGRM (ID,REMARK,LREM)
C
      ELSE
C
C ****  Set values and value-types
C
        CALL EZSET2 (ID,KSTART,KEND,KSTEP,IVAL,TYPE,NUMVAL,TOTAL,IER)
        IF ( IER .NE. 0 ) GOTO 999
C
C ****  Set remark
C
        CALL EZZSRM (ID,REMARK,LREM)
      ENDIF
C
  999 RETURN
      END
