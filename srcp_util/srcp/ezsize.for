      SUBROUTINE EZSIZE (LUN,NUMVAL,NUMREC)
      ENTRY SZSRCP      (LUN,NUMVAL,NUMREC)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Read an RCP file containing parameters and
C-                         return the number of records (identifiers
C-                         +comment lines+blank lines) and values
C-                         within file. The file must be opened
C-                         externally.
C-
C-   Inputs  : LUN         Unit number of input stream
C-
C-   Outputs : NUMVAL        Number of values
C-             NUMREC        Number of records (identifiers + comments)
C-
C-   Created 16-NOV-1988   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER LUN,NUMVAL,NUMREC
C
      INTEGER TOTAL,IER
      CHARACTER*132 RECORD
      CHARACTER*32 NAME
      INTEGER IINAME,KKNAME,LENCHR,IIREM
C
      INCLUDE 'D0$PARAMS:SRCP.DEF'
      INCLUDE 'D0$INC:BFSRCP.INC'
      LOGICAL FIRST
C----------------------------------------------------------------------
C
      NUMREC = 0  ! Identifier counter
      NUMVAL = 0  ! Value counter
      FIRST= .TRUE.
C
   50 CONTINUE
      READ(LUN,FMT='(A)',END=999) RECORD
C
C ****  Skip \START etc.
C
      IF ( FIRST ) THEN
        CALL EZZDRC (RECORD,NAME,IINAME,KKNAME,LENCHR,IIREM)
        IF ( NAME(1:LENCHR) .EQ. '\START' .OR.
     &       NAME(1:LENCHR) .EQ. '\SIZE' ) GOTO 50
        FIRST = .FALSE.
      ENDIF
C
      CALL EZZDEC (RECORD,RVALUE,ITYPE,TOTAL,IER)
      IF ( IER .NE. 0 )   GOTO 999      ! Check for End-of-Data
      IF ( TOTAL .GT. 0 ) THEN
        NUMREC = NUMREC + 1
        NUMVAL = NUMVAL + TOTAL
      ENDIF
C
      GOTO 50
C
  999 RETURN
      END
