      SUBROUTINE EZGET_NEXT_NAME (PARAM,NEXT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return next parameter in SRCP bank.
C-
C-   Inputs  : None
C-
C-   Outputs : PARAM    [C*]    Parameter name
C-   Controls: NEXT     [I]     Pointer to next name (Set to 1 initially)
C-
C-   Created   6-JUN-1990   Harrison B. Prosper
C-   Updated  28-MAR-1991   Lupe Howell   The last parameter signal is set when
C-                          NEXT GE last id.
C-   Updated   4-DEC-1991   Lupe Howell   Tidy up
C-   Updated  16-DEC-1991   Harrison B. Prosper   
C-      Use symbolic string length (CHRCRD)
C-   Updated  10-JAN-1992   Lupe Howell   Modify the search loop to guard 
C-                                        in case NEXT hits LIDS
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) PARAM
      INTEGER NEXT
C----------------------------------------------------------------------
      INCLUDE 'D0$PARAMS:SRCP.DEF'
      INCLUDE 'D0$INC:LKSRCP.INC'
      INCLUDE 'D0$INC:BFSRCP.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
C----------------------------------------------------------------------
      INTEGER LBANK
      INTEGER LIDS,LVAL,IPTO,I,J,K,L,N,II,JJ,NN
      INTEGER CHRIDS,TOTAL,IINAME,KKNAME,IIREM,LENCHR
C
      CHARACTER*(NUMCHR) IDENTF,BKNAME
      CHARACTER*(CHRCRD) REMARK,RECORD
C----------------------------------------------------------------------
C
C ****  Clear error flag
C
      ERRSRC = EZS_SUCCESS
C
C ****  Get Address of SRCP and SRCP banks
C
      IF ( ISRCP .GT. 0 ) THEN
        LSRCP = KSRCP(ISRCP)
      ELSE
        LSRCP = 0
      ENDIF
C
C ****  Check address
C
      IF ( LSRCP .LE. 0 ) THEN
        ERRSRC = EZS_BANK_NOTFOUND
        CALL ERRMSG ('NO_RCP_BANK','EZDUMP',
     &    'SRCP bank NOT FOUND','W')
        GOTO 999
      ENDIF
C
      LIDS   = IC(LSRCP+JJIDS)  ! Number of identifiers
      LVAL   = IC(LSRCP+JJVAL)  ! Number of values
      CALL EZGETNAME (LSRCP,BKNAME)
C
C ****  Get data from SRCP bank
C
      PARAM  = ' '
      ITYPE(1) = 0
      DO WHILE(( ITYPE(1) .LE. 0 ) .AND. ( NEXT .LE. LIDS ) )
        CALL EZGETD (NEXT,RVALUE,ITYPE,TOTAL,RECORD,CHRIDS)
        IF ( ITYPE(1) .GT. 0 ) THEN
          CALL WORD   (RECORD,K,J,N)
          PARAM = RECORD(K:J)
        ENDIF
        NEXT = NEXT + 1                 ! Update pointer
      ENDDO
      IF ( NEXT .GT. LIDS ) THEN
        NEXT = -1                       ! Last parameter
      ENDIF
  999 RETURN
      END
