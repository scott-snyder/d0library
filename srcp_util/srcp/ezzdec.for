      SUBROUTINE EZZDEC (RECORD,RVALUE,ITYPE,NUMVAL,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-
C-      Decode the given string (RECORD) into the arrays RVALUE and
C-      ITYPE. Single line records are returned immediately, multiple line
C-      records specified with the \ARRAY....\END construct are batched
C-      and the values are returned only when the \END has been
C-      received.
C-
C-   Inputs  : RECORD           A single line (Character*(*))
C-
C-   Outputs : RECORD           Identifier+comment
C-             RVALUE(*)        [R]     Decoded values
C-             ITYPE(*)         [I]     Types
C-             NUMVAL           [I]     Number of values
C-             IER              [I]     0--OK
C-   Controls: None
C-
C-   Created  15-DEC-1988   Harrison B. Prosper
C-   Updated  21-DEC-1991   Harrison B. Prosper  
C-      Allow zero length arrays 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      CHARACTER*(*) RECORD
      REAL          RVALUE(*)
      INTEGER       ITYPE(*)
      INTEGER       NUMVAL
      INTEGER       IER
C----------------------------------------------------------------------
      INCLUDE 'D0$PARAMS:SRCP.DEF'
C----------------------------------------------------------------------
      LOGICAL ARRAY,AT_LEAST_ONE_VALUE
      INTEGER LENCHR
      INTEGER II,JJ,I,J,K,L,N,LNAME,TOTAL
      INTEGER IINAME,KKNAME,IIREM,JJREM,NNVAL
      REAL    VALUE
C
      CHARACTER*(NUMCHR)  ANAME,NAME
      CHARACTER*(CHRCRD)  REMARK
C----------------------------------------------------------------------
      SAVE ARRAY,NNVAL,REMARK,LNAME,AT_LEAST_ONE_VALUE
C----------------------------------------------------------------------
C
      NUMVAL = 0                        ! Clear number of values
      IER = EZS_SUCCESS                 ! Clear status code
C
C ****  Do preliminary decoding of record
C
      L = LEN(RECORD)
      CALL EZZDRC (RECORD(1:L),NAME,IINAME,KKNAME,LENCHR,IIREM)
C
      IF ( NAME(1:LENCHR) .EQ. '\STOP' ) THEN
        IER = EZS_ENDOF_DATA
        GOTO 999
C
      ELSEIF ( NAME(1:LENCHR) .EQ. '\ARRAY' ) THEN
C
C ****  ARRAY TYPE
C
        ARRAY  = .TRUE.
        NNVAL  = 0
        AT_LEAST_ONE_VALUE = .FALSE.
C
C ****  Extract array name and length of name from record
C
        CALL WORD (RECORD(KKNAME+1:),I,J,LNAME)
        CALL UPCASE (RECORD(KKNAME+I:KKNAME+J),ANAME(1:LNAME))
C
C ****  Note possible remark
C
        JJREM = IIREM
        IF ( JJREM .LT. CHRCRD ) THEN
          REMARK = RECORD(JJREM:)
        ENDIF
C
        GOTO 999
C
      ELSEIF ( ARRAY ) THEN
C
C ****  Check for an end of array marker
C
        IF ( NAME(1:LENCHR) .EQ. '\END' ) THEN
          ARRAY = .FALSE.
C
C ****  Build record (Identifier + remark)
C
          NAME  = ANAME(1:LNAME)
          IF ( JJREM .LT. CHRCRD ) THEN
            RECORD = NAME(1:NUMCHR)//REMARK
          ELSE
            RECORD = NAME(1:NUMCHR)
          ENDIF
C
C ****  Maybe this was a zero length array
C
          IF ( .NOT. AT_LEAST_ONE_VALUE ) THEN
            NNVAL    =-1  ! Indicates zero length array.
            ITYPE(1) = 1
            RVALUE(1)= 0.0
          ENDIF
C
        ELSE
C
          AT_LEAST_ONE_VALUE = .TRUE.
C
C ****  Extract values from current record ignoring stuff after comment.
C
          CALL VALUSY (RECORD(IINAME:IIREM-1),
     &                 RVALUE(NNVAL+1),
     &                 ITYPE(NNVAL+1),
     &                 TOTAL,
     &                 0)
          NNVAL = NNVAL + TOTAL
          GOTO 999
        ENDIF
C
      ELSE
C
C ****  SIMPLE type
C
C ****  Check for a comment or a blank line
C
        IF ( ( RECORD(IINAME:IINAME) .EQ. '!' ) .OR.
     &       ( LENCHR .LE. 0 ) ) THEN
          ITYPE(1) = 0
          NNVAL    = 1
        ELSE
C
C ****  Skip first word and possible = sign
C
          CALL WORD (RECORD(KKNAME+1:IIREM-1),I,J,K)
          IF ( RECORD(KKNAME+I:KKNAME+J) .EQ. '=' ) THEN
            KKNAME = KKNAME+J
          ENDIF
C
C ****  Decode record;

          CALL VALUSY (RECORD(KKNAME+1:IIREM-1),RVALUE,ITYPE,NNVAL,0)
C
C ****  Append comment
C
          IF ( IIREM .LT. CHRCRD ) THEN
            RECORD = NAME(1:NUMCHR)//RECORD(IIREM:)
          ELSE
            RECORD = NAME(1:NUMCHR)
          ENDIF
C
        ENDIF
C
      ENDIF
C
      NUMVAL = NNVAL                    ! Return number of values
C
  999 RETURN
      END
