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
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) STRING
      INCLUDE 'D0$PARAMS:MAXLEV.DEF'
      INCLUDE 'D0$INC:COMNUM.INC'
      INCLUDE 'D0$INC:COMCHR.INC'
      INCLUDE 'D0$INC:SMGCOM.INC'
      INTEGER ISTAT,LIBLIN,TRULEN,LIBERP,I,J,K,M
      LOGICAL GETDEV
C----------------------------------------------------------------------
      IF(GETDEV().AND.MAINID.GT.0) THEN  ! Always use screen routines on
C                                        ! VT terminal
        IF(STRING(1:1).EQ.'0') THEN
          ISTAT=LIBLIN(' ',0)
        ELSEIF(STRING(1:1).EQ.'1') THEN
          IF(FULSCR) THEN
            ISTAT=LIBERP(2,PBROWS-1)
          ELSE
            ISTAT=LIBERP(1,PBROWS)
          ENDIF
        ENDIF
        ISTAT=LIBLIN(STRING(2:TRULEN(STRING)+1),0)
      ELSE
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
          WRITE(6,100) STRING(J:M)
  100     FORMAT(A)
          J=I+2     !Skip <CR> and <LF>
          GOTO 1
        ELSE
          M=K-J
          IF(M.GT.132) THEN
            M=132-J
          ELSE
            M=K
          ENDIF
          WRITE(6,100) STRING(J:M)
        ENDIF
      ENDIF
      RETURN
      END
