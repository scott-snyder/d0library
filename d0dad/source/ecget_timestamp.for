      SUBROUTINE ECGET_TIMESTAMP(ILUN,IDDATE)
C-----------------------------------------------------------------------
C-
C-   Purpose and Methods: Get the timestamp for the last run loaded.
C-
C-   Inputs  : ILUN  - Event catalog logical unit.
C-   Outputs : IDATE - Timestamp
C-   Controls:
C-
C-   Created   8-Jul-1994   John D. Hobbs
C-
C-----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:d0dadcom.inc'
      INTEGER ILUN,IDDATE(2)
C-----------------------------------------------------------------------
      CALL ECLSET(ILUN,IDDATE(1))
      IF( IDDATE(1).EQ.0 ) THEN
        IDDATE(1)=IAND(IQ(LECHD+JTS),2**20-1)
        IDDATE(2)=ISHFT(IQ(LECHD+JTS),-20)
      ENDIF
  999 RETURN
      END
