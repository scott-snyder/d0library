      SUBROUTINE EZZGRM (ID,REMARK,LREM)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Get REMark associated with parameter index ID
C-                         from currently selected SRCP bank.
C-
C-   Inputs  : ID       [I]     Parameter index
C-
C-   Outputs : REMARK   [C*]    String containing comment
C-             LREM     [I]     Length of comment
C-
C-   ENTRY EZZSRM (ID,REMARK,LREM)
C-
C-   Controls: None
C-
C-   Created   7-MAR-1989   Harrison B. prosper
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INTEGER       ID
      CHARACTER*(*) REMARK
      INTEGER       LREM
C
      INTEGER IER,LIDS,IPTI,IPTV,IPTO,IPTT,CHRIDS
      INTEGER IPNT,I,J,K,L,N,WRDIDS
C
      LOGICAL GET
      CHARACTER*132  RECORD
C
      INCLUDE 'D0$PARAMS:SRCP.DEF'
      INCLUDE 'D0$INC:LKSRCP.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
C----------------------------------------------------------------------
      GET = .TRUE.
      GOTO 5
C
C ****  Entry point for setting routine
C
      ENTRY EZZSRM (ID,REMARK,LREM)
      GET = .FALSE.
    5 CONTINUE
C
C ****  Get the bank address
C
      LSRCP = KSRCP(ISRCP)
C
      LIDS = IC(LSRCP+JJIDS)           ! Number of identifiers
      WRDIDS = IC(LSRCP+JJNWRD)        ! Number of words/record
      CHRIDS = WRDIDS*4
C
C ****  Get pointers
C
      CALL EZZGPT (LSRCP,IPTI,IPTO,IPTV,IPTT)
C
C ****  Get record
C
      IPNT = IPTI + WRDIDS*(ID-1)
      CALL UHTOC (IC(IPNT+1),4,RECORD,CHRIDS)
C
      IF ( GET ) THEN
C
C ****  Extract remark
C
        I = INDEX(RECORD,'!')
        IF ( I .GT. 0 ) THEN
          N = LEN(REMARK)
          J = MIN(N,CHRIDS)
          J = MAX(J,I+1)
          REMARK = RECORD(I+1:J)
          CALL SWORDS (REMARK(1:N),I,LREM,J)
        ELSE
          REMARK = ' '
          LREM = 0
        ENDIF
C
      ELSE
C
C ****  Change remark
C
        N = LEN(REMARK)
        I = INDEX(REMARK(1:N),'!')
        IF ( I .LE. 0 ) I = NUMCHR
        RECORD = RECORD(1:I)//'!'//REMARK(1:N)
        CALL UCTOH (RECORD,IC(IPNT+1),4,CHRIDS)
      ENDIF
C
  999 RETURN
      END
