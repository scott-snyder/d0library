      SUBROUTINE EZZSTO (RECORD,NUMBER,TYPE,TOTAL,LSRCP,
     &  LPTI,LPTO,LPTV,LPTT,CNVERT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-
C-   Store decoded parameter in an SRCP bank.
C-
C-   Inputs  : RECORD   [C*]    Identifier + comment
C-             NUMBER   [I*]    Array of values
C-             TYPE     [I*]    Array of value types
C-             TOTAL    [I]     Number of values in arrays
C-             LSRCP    [I]     Bank address
C-             LPTI     [I]     Absolute pointer to identfier list
C-             LPTO     [I]     Absolute pointer to order map
C-             LPTV     [I]     Absolute pointer to values
C-             LPTT     [I]     Absolute pointer to value types
C-             CNVERT   [I]     0 --- Do nothing to integers
C-                              1 --- Convert type 1 from REAL
C-                                    to INTEGER representation
C-   Outputs : None
C-   Controls: None
C-
C-   Created  16-JUN-1989   Harrison B. Prosper
C-   Updated  11-MAY-1990   Harrison B. Prosper
C-   Updated  19-SEP-1990   Harrison B. Prosper  
C-      Added entry point to suppress abort 
C-   Updated  14-DEC-1991   Harrison B. Prosper  
C-      Allow for zero length arrays; identified by total=-1 
C-   Updated  15-NOV-1993   Stan M. Krzywdzinski
C-      Added propagation of input value error, ERRVAL, up to INRCP
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      CHARACTER*(*) RECORD
      INTEGER       WRDIDS
      INTEGER       NUMBER(*)
      INTEGER       TYPE(*)
      INTEGER       TOTAL
      INTEGER       LPTI
      INTEGER       LPTO
      INTEGER       LPTV
      INTEGER       LPTT
      INTEGER       CNVERT
C
      REAL    RNUMB
      INTEGER INUMB
      EQUIVALENCE (RNUMB,INUMB)
C
      INTEGER      CHRIDS,I,J,K,L
      INTEGER      LIDS,LVAL,IPTO,IPTI
      INTEGER EZZSHFT
      LOGICAL SWITCH
      CHARACTER*1 SEVERITY
      CHARACTER*132 REMARK
C----------------------------------------------------------------------
      INCLUDE 'D0$PARAMS:SRCP.DEF'
      INCLUDE 'D0$INC:BFSRCP.INC'
      INCLUDE 'D0$INC:LKSRCP.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
C----------------------------------------------------------------------
      LOGICAL CRASH
      SAVE CRASH
      DATA CRASH/.TRUE./        ! Default behaviour on error
C----------------------------------------------------------------------
C
C ****  Store identifier count
C
      LIDS = IC(LSRCP+JJIDS)  ! Identifier count
      LIDS = LIDS + 1
      IC(LSRCP+JJIDS) = LIDS
C
C ****  Store identifier
C
      WRDIDS = IC(LSRCP+JJNWRD) ! Number words/identifier
      CHRIDS = WRDIDS*4
      IPTI = LPTI + (LIDS-1)*WRDIDS
      CALL UCTOH (RECORD(1:CHRIDS),IC(IPTI+1),4,CHRIDS)
C
C ****  Encode position of start of value(s) in order list
C       and identifier index
C
      LVAL = IC(LSRCP+JJVAL)  ! Value count
      IC(LPTO+LIDS) = EZZSHFT(LVAL+1,NBITS)
      IC(LPTO+LIDS) = IC(LPTO+LIDS) + LIDS
C
C ****  Enter values into bank according to type
C
      DO 280 I = 1, IABS(TOTAL)
        LVAL = LVAL + 1
        INUMB = NUMBER(I)
C
        IF       ( TYPE(I) .LT. 0 ) THEN
          ERRVAL = EZS_BAD_VALUE
          VARNAME = RECORD(1:CHRIDS)
          IF ( CRASH ) THEN
            SEVERITY = 'F'
          ELSE
            SEVERITY = 'W'
          ENDIF
          REMARK = 'Bad value in parameter: '//RECORD(1:CHRIDS)
          CALL ERRMSG('BAD_VALUE','EZZSTO',REMARK,SEVERITY)
C
        ELSEIF   ( TYPE(I) .EQ. 1 ) THEN
C
          IF ( CNVERT .GT.0 ) THEN
            INUMB = RNUMB
          ENDIF
          IC(LPTV+LVAL)= INUMB
C
        ELSEIF ( TYPE(I) .GE. 2 ) THEN
          C(LPTV+LVAL) = RNUMB
C
        ELSE
          IC(LPTV+LVAL)= 999999999
        ENDIF
C
        IC(LPTT+LVAL) = EZZSHFT(TYPE(I),NBITS)
        IF ( TOTAL .GT. 0 ) THEN
          IC(LPTT+LVAL) = IC(LPTT+LVAL) + TOTAL
        ENDIF
  280 CONTINUE
C
C ****  Store value count
C
      IC(LSRCP+JJVAL) = LVAL
      RETURN
C
      ENTRY EZ_ABORT_ON_BAD_VALUE(SWITCH)
      CRASH = SWITCH
  999 RETURN
      END
