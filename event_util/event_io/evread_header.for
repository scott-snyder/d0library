      SUBROUTINE EVREAD_HEADER(IN,IOS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-      reads header record into HEAD bank.
C-      Note: event division must be wiped before reading
C-            event record if this subroutine is called
C-            
C-   Created  14-MAY-1992   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IN,IOS,IUHEAD(30)
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:QUEST.INC'
      INTEGER IXWIPE,NUHEAD
C----------------------------------------------------------------------
      IXWIPE=IXCOM+IXMAIN
      CALL MZWIPE(IXWIPE)
      NUHEAD=30
    1 CALL FZIN(IN,IXMAIN,0,0,'S',NUHEAD,IUHEAD)
      IOS=IQUEST(1)
      IF(IOS.EQ.0) THEN
        CALL BKHEAD
        CALL UCOPY(IUHEAD,IQ(LHEAD+1),NUHEAD)
      ELSEIF (IOS.LT.0) THEN
        IF(IOS.GT.-6) GOTO 1
      ENDIF
  999 RETURN
      END
