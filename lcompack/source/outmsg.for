      SUBROUTINE OUTMSG(STRING)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Output string of character to display. Use
C-                         screen manipulation routines if possible,
C-                         write to unit 6 (terminal) if not.
C-
C-   Inputs  : STRING: Characters to display
C-   Outputs : None
C-   Controls: None
C-
C-   Documented 22-SEP-1988   Jan S. Hoftun
C-   Updated 20-Feb-1993  Herbert Greenlee
C-       Line mode version
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) STRING
      INCLUDE 'D0$PARAMS:MAXLEV.DEF'
      INCLUDE 'D0$INC:COMNUM.INC'
      INCLUDE 'D0$INC:COMCHR.INC'
      INCLUDE 'D0$INC:SMGCOM.INC'
      INTEGER ISTAT,TRULEN,I,J,K,M
      LOGICAL CC
C----------------------------------------------------------------------
C&IF VAXVMS
      CC = .TRUE.
C&ELSE
C&      CC = .FALSE.
C&ENDIF
      K=TRULEN(STRING)
      J=1
  1   CONTINUE
      I=INDEX(STRING(J:),CHAR(13))+J-1
      IF(I.GE.J.AND.I.LT.K) THEN
        M=I-1-J
        IF(M.GT.132) THEN
          M=132-J
        ELSE
        M=I-1
        ENDIF
        IF(.NOT.CC .AND. J.LE.M .AND. STRING(J:J).EQ.'0')THEN
          WRITE(6,101) STRING(J+1:M)
        ELSEIF(.NOT.CC .AND. J.LE.M .AND. STRING(J:J).EQ.'$')THEN
          WRITE(6,102) STRING(J+1:M)
        ELSEIF(.NOT.CC .AND. J.LE.M .AND. STRING(J:J).EQ.' ')THEN
          WRITE(6,100) STRING(J+1:M)
        ELSE
          WRITE(6,100) STRING(J:M)
        ENDIF
 100    FORMAT(A)
 101    FORMAT(/A)
 102    FORMAT($,A)
        J=I+2     !Skip <CR> and <LF>
        GOTO 1
      ELSE
        M=K-J
        IF(M.GT.132) THEN
          M=132-J
        ELSE
          M=K
        ENDIF
        IF(.NOT.CC .AND. J.LE.M .AND. STRING(J:J).EQ.'0')THEN
          WRITE(6,101) STRING(J+1:M)
        ELSEIF(.NOT.CC .AND. J.LE.M .AND. STRING(J:J).EQ.'$')THEN
          WRITE(6,102) STRING(J+1:M)
        ELSEIF(.NOT.CC .AND. J.LE.M .AND. STRING(J:J).EQ.' ')THEN
          WRITE(6,100) STRING(J+1:M)
        ELSE
          WRITE(6,100) STRING(J:M)
        ENDIF
      ENDIF
      RETURN
      END
