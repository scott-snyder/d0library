      SUBROUTINE EZCRCP(ID,LSUP,IZLINK,ISTORE,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : TO COPY a SRCP BANK TO A LINK IN THE FORM
C-                         OF AN ASCII BANK - CRCP
C-
C-   Inputs  : ID     [I] ID of SRCP BANK
C-             LSUP   [I] address of supporting link
C-             IZLINK [I] link number
C-             ISTORE [I] 0 = ZEBSTP, 1=ZEBCOM
C-
C-   Created   12-JUN-1992   Chip Stewart
C-----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER ID,LSUP,IZLINK,ISTORE,IER
      INTEGER MAXLIN
      CHARACTER LINE*132
      INTEGER NCHAR,ND
      INTEGER LCRCP,LUN,NLINK,NBUF
      INTEGER NLINE,NDATA,I1,I2,L1,IPT,ILINE
      INTEGER LIDS,LVAL,IPTO,I,J,K,L,N,II,JJ,NN
      INTEGER CHRIDS,TOTAL,IINAME,KKNAME,IIREM,LENCHR
      INTEGER EZZAND
      INCLUDE 'D0$PARAMS:SRCP.DEF'
      INCLUDE 'D0$INC:LKSRCP.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:BFSRCP.INC'
C----------------------------------------------------------------------
      CHARACTER  STRG*8,MSG*80
      CHARACTER*(NUMCHR) IDENTF,BKNAME
      CHARACTER*(CHRCRD) REMARK,RECORD
      INTEGER MAXBUF,TRULEN
      PARAMETER( MAXBUF = 2000 )         ! 2000 lines maximum/array
      CHARACTER*(CHRCRD) BUFFER(MAXBUF)
      LOGICAL FIRST,OK
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
C ****  EZDUMP THEN READ IN RCP FILE
C
      LSRCP = KSRCP(ID)
C
C ****  Clear error flag
C
      ERRSRC = EZS_SUCCESS
C
C ****  Get Address of SRCP and SRCP banks
C
      LIDS   = IC(LSRCP+JJIDS)  ! Number of identifiers
      LVAL   = IC(LSRCP+JJVAL)  ! Number of values
      ND = LVAL*10
      CALL EZGETNAME (LSRCP,BKNAME)
      CALL BKCRCP(LSUP,IZLINK,ISTORE,ND,LCRCP)
      LSRCP = KSRCP(ID)
      CALL EZPICK(BKNAME)
      IPT = LCRCP + 1
C
C ****  Print parameters in original order starting with /START
C
      WRITE(UNIT=RECORD,FMT='(''\START '',A)') BKNAME
      CALL SWORDS (RECORD,I,J,N)
      NLINE = 1
      NDATA = 0
      IF(N.GT.131) J = 131
      LINE = RECORD(1:J)//CHAR(13)
      NCHAR = J + 1
      NDATA = ( NCHAR + 3 ) / 4
      IF (ISTORE.EQ.1) THEN
        CALL UCTOH(LINE(1:NCHAR),IQ(IPT),4,NCHAR)
      ELSE
        CALL UCTOH(LINE(1:NCHAR),IC(IPT),4,NCHAR)
      END IF
      IPT = LCRCP + 1 + NDATA
C
C ***   Then /SIZE
C
      WRITE(UNIT=RECORD,FMT='(''\SIZE  '',2I10)') LVAL,LIDS
      CALL SWORDS (RECORD,I,J,N)
      NLINE = 2
      IF(N.GT.131) J = 131
      LINE = RECORD(1:J)//CHAR(13)
      NCHAR = J + 1
      NDATA = NDATA + ( NCHAR + 3 ) / 4
      IF (ISTORE.EQ.1) THEN
        CALL UCTOH(LINE(1:NCHAR),IQ(IPT),4,NCHAR)
      ELSE
        CALL UCTOH(LINE(1:NCHAR),IC(IPT),4,NCHAR)
      END IF
      IPT = LCRCP + 1 + NDATA
C
C ****  Loop over rest of BANK
C
      DO 20 L = 1 , LIDS
C
C ****  Get data from SRCP bank
C
        CALL EZGETD (L,RVALUE,ITYPE,TOTAL,RECORD,CHRIDS)
C
        CALL EZZDRC
     &        (RECORD(1:CHRIDS),IDENTF,IINAME,KKNAME,LENCHR,IIREM)
C
        IF ( IIREM .GT. CHRIDS ) IIREM = CHRIDS
C
C ****  Print data in some well-defined format
C
        CALL EZZWRT(IDENTF(1:LEN(IDENTF)),RECORD(IIREM:CHRIDS),
     &            RVALUE,ITYPE,TOTAL,MAXBUF,NBUF,BUFFER)
C
C ****  Check for overflow
C
       IF ( NBUF .LT. 0 ) THEN
         ERRSRC = EZS_ARRAY_TOOLARGE
         IER = ERRSRC
         CALL WORD(IDENTF,I,J,K)
         MSG = 'Parameter '//IDENTF(I:J)//' overflows internal buffer'
         CALL ERRMSG('OVERFLOW','EZCRCP',MSG(1:TRULEN(MSG)),'W')
       ENDIF
C
C ****  Write out buffer
C
        NBUF = IABS(NBUF)
        DO K = 1, NBUF
          CALL SWORDS(BUFFER(K),I,J,N)
          NLINE = NLINE + 1
          IF(N.GT.131) J = 131
          LINE = BUFFER(K)(1:J)//CHAR(13)
          NCHAR = J + 1
          NDATA = NDATA + ( NCHAR + 3 ) / 4
          IF(NDATA.GE.(ND)) THEN
            ND = ND + LVAL
            IF (ISTORE.EQ.1) THEN
              CALL MZPUSH(IXCOM,LCRCP,0, LIDS,'I')
            ELSE
              CALL MZPUSH(IXSTP,LCRCP,0, LIDS,'I')
            END IF
            LSRCP = KSRCP(ID)
            IPT = LCRCP + 1 + NDATA - ( NCHAR + 3 ) / 4
          END IF
          IF (ISTORE.EQ.1) THEN
            CALL UCTOH(LINE(1:NCHAR),IQ(IPT),4,NCHAR)
          ELSE
            CALL UCTOH(LINE(1:NCHAR),IC(IPT),4,NCHAR)
          END IF
          IPT = LCRCP + 1 + NDATA
        ENDDO

   20 CONTINUE
C
      WRITE(UNIT=RECORD,FMT='(''\STOP '')')
      NLINE = NLINE + 1
      CALL SWORDS (RECORD,I,J,N)
      IF(N.GT.131) J = 131
      LINE = RECORD(1:J)//CHAR(13)
      NCHAR = J + 1
      NDATA = NDATA + ( NCHAR + 3 ) / 4
      IF (ISTORE.EQ.1) THEN
        CALL UCTOH(LINE(1:NCHAR),IQ(IPT),4,NCHAR)
      ELSE
        CALL UCTOH(LINE(1:NCHAR),IC(IPT),4,NCHAR)
      END IF
C
C ****  MZPUSH SQUEEZE
C
      ND = NDATA + 1 - ND
      IF (ISTORE.EQ.1) THEN
        CALL MZPUSH(IXCOM,LCRCP,0, ND,'I')
      ELSE
        CALL MZPUSH(IXSTP,LCRCP,0, ND,'I')
      END IF
      CALL EZRSET
  999 RETURN
      END
