      SUBROUTINE CHCOLS
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Change number of columns in menu display
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
      INT=NUMCOL
      IF(INT.LT.10) THEN
        WRITE(PROMPT,100) INT
  100   FORMAT('Enter # of menu columns [',I1,'] >')
      ELSE
        WRITE(PROMPT,101) INT
  101   FORMAT('Enter # of menu columns [',I2,'] >')
      ENDIF
      CALL GETPAR(1,PROMPT,'I',INT)
      IF(PF.EQ.0.AND..NOT.SETUP) THEN
        IF(INT.GT.0.AND.INT.LT.11) THEN
          NUMCOL=INT
          PF=4
        ELSE
          CALL OUTMSG('0Illegal # of columns, should be 1--10 !'//
     *               CHAR(7))
          CALL OUTMSG(' ')
          CALL PFWAIT
        ENDIF
      ENDIF
      RETURN
      END
