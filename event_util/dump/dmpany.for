      SUBROUTINE DMPANY(BANK,PRBANK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-       Dumping subroutine for any BANK
C-       check for flag and dump only once per event
C-       will not dump if DMPUNI (dump unit) .le. 0
C-
C-   Inputs  : 
C-   BANK   = bank to dump (character*4)
C-   PRBANK = external printing subroutine
C-
C-   Created  17-APR-1989   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) BANK
      CHARACTER*16 DUMFLG
      EXTERNAL PRBANK
      LOGICAL FLGVAL
      INTEGER DUNIT,DMPUNI
C----------------------------------------------------------------------
      DUNIT=DMPUNI()
      DUMFLG='DMPUSR_'//BANK
        IF(FLGVAL(DUMFLG)) THEN
          IF(DUNIT.GT.0) CALL PRBANK(DUNIT,0,0,'ALL',1)
          CALL FLGSET(DUMFLG,.FALSE.)
        ENDIF
  999 RETURN
      END
