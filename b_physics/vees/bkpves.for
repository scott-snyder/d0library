      SUBROUTINE BKPVES(LPVES)
C----------------------------------------------------------------------
C 
C    Book the Bank PVES
C 
C    Input  : none 
C
C    Outputs : LPVES - link of PVES Bank
C 
C    9-JUL-1990  Daria Zieminska 
C-   Updated   7-NOV-1991   Daria Zieminska  new format 
C 
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER LPVES,LPARH,GZPARH,MBOOKT(5),IBOOKT,IXPVES
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$LINKS:IZPVES.LINK/LIST'
      LOGICAL FIRST
      DATA IBOOKT/4HPVES/
      DATA FIRST/.TRUE./
      LPVES = 0
      IF(FIRST)THEN
        MBOOKT(1)=IBOOKT
        MBOOKT(2)=4      
        MBOOKT(3)=1
        MBOOKT(4)=233
        MBOOKT(5)=IXPVES
        CALL MZFORM('PVES','1I 1B 3I 5F/2I 15F',IXPVES)
        FIRST = .FALSE.
      ENDIF
      LPARH = GZPARH()
      CALL MZLIFT(IXMAIN,LPVES,LPARH,-IZPVES,MBOOKT,0)
 1000 RETURN
      END
