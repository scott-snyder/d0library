      SUBROUTINE EZZDMP (LUNOUT,PARAM,REMARK,NUMBER,ITYPE,TOTAL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Print a parameter in RCP format.
C-
C-   Inputs  : LUNOUT           [I]     Logical unit number of output file.
C-             PARAM            [C*]    Identifier (32-char. max)
C-             REMARK           [C*]    Remark
C-             NUMBER(*)        [R]     Value(s)
C-             ITYPE(*)         [I]     Value types
C-             TOTAL            [I]     Number of values
C-
C-   Outputs : None
C-   Controls: None
C-
C-   Created  12-NOV-1988   Harrison B. Prosper
C-   Updated   1-MAY-1990   Harrison B. Prosper
C-      Correct format bug for single long strings
C-   Updated  12-APR-1991   Harrison B. Prosper
C-      Added entry EZSTYLE
C-   Updated  23-APR-1991   Harrison B. Prosper
C-      Added HEX values
C-   Updated  12-JUN-1991   Harrison B. Prosper
C-      Use EZZWRT
C-   Updated  12-Feb-1992   Herbert Greenlee
C-      Fixed for UNIX.  (Eliminate concatenation argument)
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER       LUNOUT
      CHARACTER*(*) PARAM
      CHARACTER*(*) REMARK
      REAL          NUMBER(*)
      INTEGER       ITYPE(*)
      INTEGER       TOTAL
C----------------------------------------------------------------------
      INTEGER I,J,K,L,N,NBUF
C----------------------------------------------------------------------
      INCLUDE 'D0$PARAMS:SRCP.DEF'
      INTEGER MAXBUF
      PARAMETER( MAXBUF = 300 )         ! 300 lines maximum/array
      INTEGER TRULEN
C----------------------------------------------------------------------
      CHARACTER*(CHRCRD) BUFFER(MAXBUF)
      CHARACTER*(CHRCRD) ERROR_MESSAGE
C----------------------------------------------------------------------
C
C ****  Convert to character strings
C
      CALL EZZWRT(PARAM(1:LEN(PARAM)),
     &            REMARK(1:LEN(REMARK)),
     &            NUMBER,ITYPE,TOTAL,MAXBUF,NBUF,BUFFER)
C
C ****  Write out buffer
C
      N = IABS(NBUF)
      DO I = 1, N
        CALL SWORDS(BUFFER(I),J,K,L)
        CALL EZUDMP(LUNOUT,BUFFER(I)(1:K))
      ENDDO
C
C ****  Check for overflow
C
      IF ( NBUF .LT. 0 ) THEN
        CALL WORD(PARAM(1:LEN(PARAM)),I,J,K)
        ERROR_MESSAGE =
     &    'Parameter '//PARAM(I:J)//' overflows internal buffer'
        CALL ERRMSG('OVERFLOW','EZZDMP',
     &    ERROR_MESSAGE(1:TRULEN(ERROR_MESSAGE)),'W')
      ENDIF
  999 RETURN
      END
