      SUBROUTINE EZPRINT (LUN,PARAM,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Print the RCP parameter PARAM to unit LUN. If
C-   LUN = 0 print using INTMSG. Use EZPICK to pick the bank prior to
C-   calling this routine.
C-
C-   Inputs  : LUN      [I]     Output unit number.
C-             PARAM    [C*]    Parameter name.
C-   Outputs : IER      [I]     0 --- OK.
C-   Controls: None
C-
C-   Created  30-MAR-1990   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER LUN
      CHARACTER*(*) PARAM
      INTEGER IER
C
      INTEGER ID,CHRIDS,TOTAL,IINAME,KKNAME,IIREM,LENCHR
C
C----------------------------------------------------------------------
      INCLUDE 'D0$PARAMS:SRCP.DEF'
      INCLUDE 'D0$INC:BFSRCP.INC'
C----------------------------------------------------------------------
      CHARACTER*(NUMCHR) IDENTF
      CHARACTER*(CHRCRD) RECORD
C----------------------------------------------------------------------
C
      IER = EZS_SUCCESS
      CALL EZGETI (PARAM,ID,IER)
      IF ( IER .NE. EZS_SUCCESS ) GOTO 999
C
C ****  Get data from SRCP bank
C
      RECORD = ' '
      CALL EZGETD (ID,RVALUE,ITYPE,TOTAL,RECORD,CHRIDS)
      CALL EZZDRC (RECORD(1:CHRIDS),IDENTF,IINAME,KKNAME,LENCHR,IIREM)
      IF ( IIREM .GT. CHRIDS ) IIREM = CHRIDS
C
C ****  Print data in some well-defined format
C
      CALL EZZDMP (LUN,IDENTF,RECORD(IIREM:CHRIDS),RVALUE,ITYPE,TOTAL)
C
  999 RETURN
      END
