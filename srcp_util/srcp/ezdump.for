      SUBROUTINE EZDUMP (PRUNIT,LBANK,SWITCH)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Prints the contents of given SRCP bank
C-
C-   Inputs  : PRUNIT   Print Unit
C-             LBANK    Bank address. If ZERO use address of currently
C-                      selected bank. Use EZPICK to select bank.
C-             SWITCH  -1 Printout of SRCP bank in SORTED order
C-                      0 Printout of SRCP bank in ORIGINAL order
C-                      1 Printout of SRCP bank in ORIGINAL and SORTED order
C-                      2 Printout a list of identifiers only.
C-
C-   Outputs : None
C-                      Error codes
C-                      0 ---- OK
C-                     -1 ---- SRCP bank address is zero
C-
C-   Created  27-NOV-1987   Rajendran Raja
C-   Modified 25-JUN-1988   Harrison B. Prosper
C-                          Removed all non-FORTRAN-77 stuff.
C-   Modified  2-OCT-1988   Harrison B. Prosper
C-                          Integration with new SRCP routines
C-                          EZFILL etc.
C-   Modified 15-OCT-1988   Harrison B. Prosper
C-                          Put in check on bank addresses
C-   Modified 13-NOV-1988   Harrison B. Prosper
C-                          MAJOR CHANGE: New SRCP bank Format
C-   Updated  22-JUN-1989   Harrison B. Prosper
C-   Tidied up code and added option 2.
C-   Updated  11-MAY-1990   Harrison B. Prosper
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER PRUNIT,LBANK,SWITCH
      INTEGER LIDS,LVAL,IPTO,I,J,K,L,N,II,JJ,NN
      INTEGER CHRIDS,TOTAL,IINAME,KKNAME,IIREM,LENCHR
      INTEGER EZZAND
C----------------------------------------------------------------------
      INCLUDE 'D0$PARAMS:SRCP.DEF'
      INCLUDE 'D0$INC:LKSRCP.INC'
      INCLUDE 'D0$INC:BFSRCP.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
C----------------------------------------------------------------------
      CHARACTER*8  STRG
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
      IF ( SWITCH .GE. 0 ) THEN
C
C ****  Print records only
C
        IF ( SWITCH .EQ. 2 ) THEN
          CALL EZUDMP (PRUNIT,' ')
          CALL EZUDMP (PRUNIT,' Identifier list for bank: '//BKNAME)
          CALL EZUDMP (PRUNIT,'  ')
C
          NN = 0
          DO 15 I = 1 , LIDS
C
C ****  Get data from SRCP bank
C
            CALL EZGETD (I,RVALUE,ITYPE,TOTAL,RECORD,CHRIDS)
            IF ( ITYPE(1) .GT. 0 ) THEN
              NN = NN + 1
              CALL INTSTR (NN,'. ',STRG,L)
              CALL WORD   (RECORD,K,J,N)
              CALL EZUDMP (PRUNIT,' '//STRG//RECORD(K:J))
            ENDIF
C
   15     CONTINUE
          GOTO 999
        ENDIF
C
C ****  Print parameters in original order
C
        WRITE(UNIT=RECORD,FMT='(''\START '',A)') BKNAME
        CALL SWORDS (RECORD,I,J,N)
        CALL EZUDMP (PRUNIT,RECORD(1:J))
C
        WRITE(UNIT=RECORD,FMT='(''\SIZE  '',2I10)') LVAL,LIDS
        CALL SWORDS (RECORD,I,J,N)
        CALL EZUDMP (PRUNIT,RECORD(1:J))
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
     &    (PRUNIT,IDENTF,RECORD(IIREM:CHRIDS),RVALUE,ITYPE,TOTAL)
C
   20   CONTINUE
C
        WRITE(UNIT=RECORD,FMT='(''\STOP '')')
        CALL SWORDS (RECORD,I,J,N)
        CALL EZUDMP (PRUNIT,RECORD(1:J))
      ENDIF
C
      IF ( SWITCH .EQ. 0 ) GOTO 999
C
C ****  Print parameters in alphabetical order
C ****  but exclude comment lines
C
      WRITE(UNIT=RECORD,FMT='(''\START '',A)') BKNAME
      CALL SWORDS (RECORD,I,J,N)
      CALL EZUDMP (PRUNIT,RECORD(1:J))
C
      WRITE(UNIT=RECORD,FMT='(''\SIZE '',2I10)') LVAL,LIDS
      CALL SWORDS (RECORD,I,J,N)
      CALL EZUDMP (PRUNIT,RECORD(1:J))
C
      IPTO = LSRCP+IC(LSRCP+JJPORD)-1
      DO 40 I = 1 , LIDS
        J = EZZAND(IC(IPTO+I),MASK)
        CALL EZGETD (J,RVALUE,ITYPE,TOTAL,RECORD,CHRIDS)
C
        IF ( ITYPE(1) .NE. 0 ) THEN
          CALL EZZDRC
     &      (RECORD(1:CHRIDS),IDENTF,IINAME,KKNAME,LENCHR,IIREM)
C
          IF ( IIREM .GT. CHRIDS ) IIREM = CHRIDS
C
C ****  Print data in some well-defined format
C
          CALL EZZDMP
     &    (PRUNIT,IDENTF,RECORD(IIREM:CHRIDS),RVALUE,ITYPE,TOTAL)
        ENDIF
   40 CONTINUE
C
      WRITE(UNIT=RECORD,FMT='(''\STOP '')')
      CALL SWORDS (RECORD,I,J,N)
      CALL EZUDMP (PRUNIT,RECORD(1:J))
C
  999 RETURN
      END
