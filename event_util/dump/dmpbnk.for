      SUBROUTINE DMPBNK(BANK,VALUE)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-       Control individual bank flags for event dumps
C-
C-   Inputs  : 
C-   BANK = bank name
C-   VALUE= true then bank  added to   dump list
C-          false   "      removed from   "
C-
C-   Created  17-APR-1989   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) BANK
      CHARACTER*16 DUMFLG
      LOGICAL FLGCHK,VALUE
C----------------------------------------------------------------------
C
      DUMFLG='DMPUSR_'//BANK
      IF(VALUE) THEN
        IF(.NOT.FLGCHK(DUMFLG)) CALL FLGBK(DUMFLG,1) ! add to list
        CALL FLGSET(DUMFLG,.TRUE.)
      ELSE
        IF(FLGCHK(DUMFLG)) CALL FLGUBK(DUMFLG,1)  ! remove from list
      ENDIF
  999 RETURN
      END
