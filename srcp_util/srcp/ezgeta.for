      SUBROUTINE EZGETA (PARAM1,JSTART,JEND,JSTEP,IVAL,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Get value(s) of variable PARAM1 from a
C-                         pre-selected SRCP bank. Use EZPICK to select
C-                         an SRCP bank.
C-                         EZGETA is the same as EZGET except that the array
C-                         indices are given explicitly, thereby avoiding
C-                         the string decoding done in EZGET. Use the
C-                         entry point EZSETA to set values. See also
C-                         EZGET1.
C-
C-   Inputs  : PARAM1   [C*]    Name of parameter
C-             JSTART   [I]     First array index. If JSTART = 0
C-                              then the length of the array is
C-                              returned in IVAL.
C-             JEND     [I]     Last array index
C-             JSTEP    [I]     Index step
C-
C-                              DO I = JSTART, JEND, JSTEP
C-
C-   Outputs : IVAL(*)     Value(s) pertaining to name PARAM1.
C-             IER         Error code
C-
C-                           0 --> OK
C-                          -1 --> Variable not found
C-                          -2 --> Bank not found
C-
C-   Controls: None
C-
C-   ENTRY EZSETA (PARAM1,JSTART,JEND,JSTEP,IVAL,IER)
C-
C-   Created   6-APR-1989   Harrison B. Prosper
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      CHARACTER*(*) PARAM1
      INTEGER       JSTART
      INTEGER       JEND
      INTEGER       JSTEP
      INTEGER       IVAL(*)
      INTEGER       IER
C
      CHARACTER*32  PARAM
C
      INTEGER I,J,K,L,ID
      LOGICAL GET
C
C----------------------------------------------------------------------
      GET = .TRUE.
      GOTO 5
C
C ****  Entry point for setting routine
C
      ENTRY EZSETA (PARAM1,JSTART,JEND,JSTEP,IVAL,IER)
      GET = .FALSE.
    5 CONTINUE
C
C ****  Get parameter index
C
      L      = LEN(PARAM1)
      CALL EZGETI (PARAM1(1:L),ID,IER)
      IF ( IER .NE. 0 ) GOTO 999
C
C ****  Get/Set parameter
C
      IF ( GET ) THEN
        CALL EZGET1 (ID,JSTART,JEND,JSTEP,IVAL,IER)
      ELSE
        CALL EZSET1 (ID,JSTART,JEND,JSTEP,IVAL,IER)
      ENDIF
C
  999 RETURN
      END
