      FUNCTION BANK_DROPPED(LBANK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-         check if bank pointed to by LBANK has been dropped
C-   Returned value  : true if bank dropped, false otherwise
C_
C-   Inputs  : 
C-     LBANK = pointer to bank to be checked
C-
C-   Created   4-SEP-1990   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL BANK_DROPPED
      INTEGER LBANK
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:ISTAT_DROP.PARAMS'
C----------------------------------------------------------------------
C
      BANK_DROPPED=.TRUE.
      IF(LBANK.LE.0) GOTO 999
      IF(IAND(IQ(LBANK),ISTAT_DROP).NE.0) GOTO 999
      BANK_DROPPED=.FALSE.
  999 RETURN
      END
