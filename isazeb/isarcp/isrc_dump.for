      SUBROUTINE ISRC_DUMP (PRUNIT,LBANK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Prints the contents of given ISRC bank
C-
C-   Inputs  : PRUNIT   Print Unit
C-             LBANK    Bank address. If ZERO use address of currently
C-                      selected bank.
C-   Outputs : None
C-   Controls: None
C-
C-
C-   Created  12-JAN-1990   Chip Stewart
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER PRUNIT,LBANK
      INTEGER LIDS,LVAL,IPTO,I,J,K,L,N,II,JJ,NN
      INTEGER CHRIDS,TOTAL,IINAME,KKNAME,IIREM,LENCHR,GZISRC
      CHARACTER*8  STRG
      CHARACTER*32 IDENTF
      CHARACTER*80 REMARK,RECORD
C
      INCLUDE 'D0$PARAMS:SRCP.DEF'
      INCLUDE 'D0$INC:LKSRCP.INC'
      INCLUDE 'D0$INC:NMSRCP.INC'
      INCLUDE 'D0$INC:BFSRCP.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC'
C----------------------------------------------------------------------
C
C ****  Get Address of ISRC bank
C
      IF ( LBANK .LE. 0 ) THEN
C
C ****  Use address of pre-selected bank
C
          LSRCP = GZISRC ()
      ELSE
        LSRCP = LBANK
      ENDIF
C
C ****  Check address
C
      IF ( LSRCP .LE. 0 ) THEN
        CALL ERRMSG ('ISARCP','ISRC_DUMP',
     &    'ISRC bank NOT FOUND','W')
        GOTO 999
      ENDIF
C
      LIDS   = IQ(LSRCP+JJIDS)  ! Number of identifiers
      LVAL   = IQ(LSRCP+JJVAL)  ! Number of values
      RECORD = ' '
C
      WRITE(UNIT=RECORD,FMT='('' \SIZE '',2I10)') LVAL,LIDS
      CALL SWORDS (RECORD,I,J,N)
      CALL EZUDMP (PRUNIT,RECORD(1:J))
C
C ****  Print parameters in original order
C
      DO 20 I = 1 , LIDS
C
C ****  Get data from SRCP bank
C
        CALL ISRC_GETD (LSRCP,I,RVALUE,ITYPE,TOTAL,RECORD,CHRIDS)
C
        CALL EZZDRC
     &        (RECORD(1:CHRIDS),IDENTF,IINAME,KKNAME,LENCHR,IIREM)
C
        IF ( IIREM .GT. CHRIDS ) IIREM = CHRIDS
C
C ****  Print data in some well-defined format
C
        CALL EZZDMP
     &    (PRUNIT,IDENTF,RECORD(IIREM:CHRIDS),RVALUE,ITYPE,TOTAL)
C
   20 CONTINUE
C
  999 RETURN
      END
