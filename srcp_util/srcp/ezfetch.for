      SUBROUTINE EZFETCH (PARAM,MAXBUF,NBUF,BUFFER,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Print the RCP parameter PARAM to the array of
C-   character strings BUFFER(*).
C-
C-   Inputs  : PARAM    [C*]    Parameter name.
C-             MAXBUF   [I]     Maximum number of records
C-                              to return
C-
C-   Outputs : NREC     [I]     Number of records returned
C-             BUFFER(*)[C*]    Array of character strings
C-             IER      [I]     0 --- OK.
C-
C-   Controls: None
C-
C-   Created  12-JUN-1991   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) PARAM
      INTEGER MAXBUF,NBUF
      CHARACTER*(*) BUFFER(*)
      INTEGER IER
C----------------------------------------------------------------------
      INCLUDE 'D0$PARAMS:SRCP.DEF'
      INCLUDE 'D0$INC:BFSRCP.INC'
C----------------------------------------------------------------------
      INTEGER ID,CHRIDS,TOTAL,IINAME,KKNAME,IIREM,LENCHR
      CHARACTER*(NUMCHR) IDENTF
      CHARACTER*(CHRCRD) RECORD
C----------------------------------------------------------------------
C
      IER = EZS_SUCCESS
      CALL EZGETI (PARAM,ID,IER)
      IF ( IER .NE. EZS_SUCCESS ) GOTO 999
C
C ****  Get data from RCP bank
C
      RECORD = ' '
      CALL EZGETD (ID,RVALUE,ITYPE,TOTAL,RECORD,CHRIDS)
      CALL EZZDRC (RECORD(1:CHRIDS),IDENTF,IINAME,KKNAME,LENCHR,IIREM)
      IF ( IIREM .GT. CHRIDS ) IIREM = CHRIDS
C
C ****  Write data in RCP Format to character buffer
C
      CALL EZZWRT(IDENTF,RECORD(IIREM:CHRIDS),RVALUE,ITYPE,TOTAL,
     &  MAXBUF,NBUF,BUFFER)
C
  999 RETURN
      END
