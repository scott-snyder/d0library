      SUBROUTINE BKVCOR(LVCOR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book the bank VCOR.
C-
C-   Inputs  : None
C-   Outputs : LVCOR  [I] Address of booked VCOR bank.
C-   Controls: None
C-
C-   Created  17-NOV-1992   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER LPROC
      INTEGER LVCOR
C----------------------------------------------------------------------
      INTEGER ND,NL,NS,IXIO
      INTEGER GZPROC
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZVCOR.LINK'
C----------------------------------------------------------------------
      LOGICAL FIRST
      SAVE FIRST, IXIO
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
C--   INITIALIZE
C
      LVCOR = 0
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        CALL MZFORM('VCOR','1I 1H -F',IXIO)        ! Describe Bank format
      ENDIF
C
C--   FIND LINK TO SUPPORTING PARENT BANK
C
      LPROC = GZPROC()
      IF ( LPROC .LE. 0 ) THEN
        GOTO 999
      ENDIF
C
      NL = 4
      NS = 1
      ND = 12
      CALL MZBOOK(IXMAIN,LVCOR,LPROC,-IZVCOR,'VCOR',NL,NS,ND,IXIO,0)
C
  999 RETURN
      END
