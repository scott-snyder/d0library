      SUBROUTINE EZGSET (CHAR1,IVAL,IDX)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Access value(s) of variable CHAR1 in given
C-                         SRCP bank. This calls EZGET or EZSET
C-                         and bombs if the return code from either of
C-                         these routines is non-zero.
C-
C-   Inputs  : CHAR1       Name of datum or data (CHARACTER string)
C-
C-   Outputs : IVAL(*)     Value(s) pertaining to name ``CHAR1''.
C-
C-
C-   Controls: IDX         1 --> GET value(s), -1 ---> SET value
C-
C-
C-   Created  27-NOV-1987   Rajendran Raja
C-   Modified 28-JUN-1988   Harrison B. Prosper
C-   Updated  12-Feb-1992   Herbert Greenlee
C-      UNIX version.
C-   Updated   3-Jan-1996   sss - Compile with g77.
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) CHAR1
      CHARACTER*132 CTEMP
      INTEGER IVAL(*),IER,N,IDX,I,J,L
      CHARACTER*132 STRING
      INCLUDE 'D0$PARAMS:SRCP.DEF'
C----------------------------------------------------------------------
      ENTRY      GTSRCP (CHAR1,IVAL,IDX)
C----------------------------------------------------------------------
      IF ( IDX .GT. 0 ) THEN
        CALL EZGET  (CHAR1,IVAL,IER)
      ELSEIF ( IDX .LT. 0 ) THEN
        CALL EZSET  (CHAR1,IVAL,IER)
      ELSE
        IER = EZS_BAD_ARGUMENT
      ENDIF
C
C ****  Check error return code
C
      IF ( IER .EQ. EZS_SUCCESS ) THEN
        GOTO 999
      ELSE
        N = LEN(CHAR1)
        CALL EZGET_ERROR_TEXT(IER,STRING)
        CALL SWORDS(STRING,I,J,L)
        CTEMP = STRING(1:L)//':'//CHAR1(1:N)
        CALL ERRMSG('SRCP','EZGSET',CTEMP(1:L+N+1),'F')
      ENDIF
  999 RETURN
      END
