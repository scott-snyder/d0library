      SUBROUTINE BKHSTR(LHSTR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-      book HSTR (history bank), new one made for every call
C-   
C-   Output : LHSTR = pointer to HSTR bank
C-
C-   Created   3-NOV-1988   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$LINKS:IZHSTR.LINK/LIST'
C
      LOGICAL FIRST
      INTEGER LHSTR,IOHSTR,LSUP,PATHGZ
      INTEGER NDATA
      PARAMETER (NDATA=22)
C
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C 
      IF(FIRST) THEN
         CALL MZFORM('HSTR','6I 14H 2B',IOHSTR)   ! format for HSTR
         FIRST=.FALSE.
      ENDIF
      CALL PATHBK(LSUP)
C
C   Create HSTR bank 
C
      CALL MZBOOK(IXMAIN,LHSTR,LSUP,-IZHSTR,
     +            'HSTR',2,2,NDATA,IOHSTR,0)
C
  999 RETURN
      END
