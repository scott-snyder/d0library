      SUBROUTINE DO_MISC (LUNSET)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Write out mixtures, views structures, dead
C-                         material set etc. for Calorimeter to unit
C-                         LUNSET.
C-
C-   Inputs  : LUNSET   Output unit number (normally to file SRCP_REST)
C-   Outputs : None
C-   Controls: None
C-
C-   Created  19-JAN-1989   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER LUNSET
      INTEGER LBANK
      INTEGER LIDS,LVAL,I,J,N
      INTEGER CHRIDS,TOTAL,IINAME,KKNAME,IIREM,LENCHR
C
      CHARACTER*32 IDENTF,BKNAME
      CHARACTER*80 RECORD
C
      INCLUDE 'D0$PARAMS:SRCP.DEF'
      INCLUDE 'D0$INC:LKSRCP.INC'
      INCLUDE 'D0$INC:BFSRCP.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
C----------------------------------------------------------------------
C
C ****  Get Address of SRCP and SRCP banks
C
      IF ( LBANK .LE. 0 ) THEN
C
C ****  Use address of pre-selected bank
C
        IF ( ISRCP .GT. 0 ) THEN
          LSRCP = KSRCP(ISRCP)
        ELSE
          LSRCP = 0
        ENDIF
      ELSE
        LSRCP = LBANK
      ENDIF
C
C ****  Check address
C
      IF ( LSRCP .LE. 0 ) THEN
        ERRSRC = EZS_BANK_NOTFOUND
        CALL ERRMSG ('SRCP','EZDUMP',
     &    'SRCP bank NOT FOUND','W')
        GOTO 999
      ENDIF
C
      LIDS   = IC(LSRCP+JJIDS)  ! Number of identifiers
      LVAL   = IC(LSRCP+JJVAL)  ! Number of values
      RECORD = ' '
      CALL EZGETNAME (LSRCP,BKNAME)
C
C
C ****  Print parameters in original order
C
      WRITE(UNIT=RECORD,FMT='(''\SIZE  '',2I10)') LVAL,LIDS
      CALL SWORDS (RECORD,I,J,N)
      CALL EZUDMP (LUNSET,RECORD(1:J))
C
      DO 20 I = 1 , LIDS
C
C ****  Get data from SRCP bank
C
        CALL EZGETD (I,RVALUE,ITYPE,TOTAL,RECORD,CHRIDS)
C
        CALL EZZDRC
     &        (RECORD(1:CHRIDS),IDENTF,IINAME,KKNAME,LENCHR,IIREM)
C
        IF ( IIREM .GT. CHRIDS ) IIREM = CHRIDS
C
C ****  Print data in some well-defined format
C
        CALL EZZDMP
     &    (LUNSET,IDENTF,RECORD(IIREM:CHRIDS),RVALUE,ITYPE,TOTAL)
C
   20 CONTINUE
C
  999 RETURN
      END
