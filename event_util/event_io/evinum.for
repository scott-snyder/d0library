      SUBROUTINE EVINUM(HIGH2,LOW3)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-       Return Level 1 input event number
C-   Inputs  : HIGH2  [I] Upper 2 bytes of event id (Level 1 event number)
C-             LOW3   [I] Lower 3 bytes of event id
C-   Inputs  : HIGH2  [I] Upper 2 bytes of event id (Level 1 event number)
C-             LOW3   [I] Lower 3 bytes of event id
C-   Outputs :
C-   Controls:
C-
C-   Created  13-APR-1992   James T. Linnemann
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER HIGH2,LOW3,LASTHI,LASTLO,THREE_BYTES
      PARAMETER( THREE_BYTES = (2**24)-1 )  !0FFF Hex
      SAVE LASTHI,LASTLO
      DATA LASTHI,LASTLO/2*0/
C----------------------------------------------------------------------
      IF(LHEAD.NE.0) THEN
        LASTLO = IQ(LHEAD+7)
        LASTHI = IQ(LHEAD+8)
      ENDIF
      LOW3 = IAND(LASTLO,THREE_BYTES)
      HIGH2 = LASTHI
  999 RETURN
      END

