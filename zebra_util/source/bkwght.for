      SUBROUTINE BKWGHT(LWGHT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book WGHT bank (event weight).
C-
C-   Inputs:
C-   Outputs : LWGHT = Pointer to booked bank.
C-
C-   Created  19-Apr-1993   Herbert Greenlee
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$LINKS:IZWGHT.LINK'
      INCLUDE 'D0$INC:ZEBCOM.INC'
C
      INTEGER LWGHT
      INTEGER IOWGHT
      INTEGER INCNL
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      IF(FIRST) THEN 
        CALL MZFORM('WGHT','1I 1F',IOWGHT)   ! format for WGHT
        FIRST=.FALSE.
      ENDIF
      LWGHT = 0
      IF(LHEAD.EQ.0)RETURN
C- Make sure that the HEAD bank has enough links for the WGHT bank (19).
C- Push if necessary.
      INCNL = IZWGHT - IQ(LHEAD-3)
      IF(INCNL.GT.0)THEN
        CALL MZPUSH(IXCOM, LHEAD, INCNL, 0, ' ')
      ENDIF
C- Book the WGHT bank (if necessary).
      LWGHT = LQ(LHEAD-19)
      IF(LWGHT.EQ.0)THEN
        CALL MZBOOK(IXMAIN, LWGHT, LHEAD, -IZWGHT, 'WGHT', 0, 0, 2,
     &    IOWGHT, -1)
      ENDIF
  999 RETURN
      END
