      INTEGER FUNCTION USUNIT()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-    provide a unit number for USER printout (USER.OUT)
C-
C-   Created  22-JUL-1987   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER UNIT,ERR
      LOGICAL FIRST,OK,PRODUC
      SAVE UNIT,FIRST
      DATA FIRST/.TRUE./
      USUNIT=0
      IF(PRODUC()) GOTO 999        ! do not open USER.OUT for production
C
      IF(FIRST) THEN
        CALL GTUNIT(1,UNIT,ERR)
        FIRST=.FALSE.
        CALL D0OPEN(UNIT,'USER.OUT','O',OK)
        IF(.NOT.OK) CALL INTMSG(' Cannot open USER.OUT')
      ENDIF
C
      USUNIT=UNIT              
  999 RETURN
      END
