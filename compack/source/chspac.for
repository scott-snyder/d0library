      SUBROUTINE CHSPAC
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Change line spacing in menu display
C-
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Documented 22-SEP-1988   Jan S. Hoftun
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
C     Get definitions for COMPACK
C
      INCLUDE 'D0$PARAMS:MAXLEV.DEF'
      INCLUDE 'D0$INC:COMNUM.INC'
      INCLUDE 'D0$INC:COMCHR.INC'
C
      INTEGER K,INT,ISTAT,CONINT
      CHARACTER*20 INTXT
      CHARACTER*40 PROMPT
C----------------------------------------------------------------------
      INT=LINSPA
      WRITE(PROMPT,100) INT
  100 FORMAT('Enter line spacing [',I1,'] >')
      CALL GETPAR(1,PROMPT,'I',INT)
      IF(PF.EQ.0.AND..NOT.SETUP) THEN
        IF(INT.GT.0.AND.INT.LT.4) THEN
          LINSPA=INT
          PF=4
        ELSE
          CALL OUTMSG('0Illegal line spacing, should be 1--3 !'//
     *                      CHAR(7))
          CALL OUTMSG(' ')
          CALL PFWAIT
        ENDIF
      ENDIF
      RETURN
      END
