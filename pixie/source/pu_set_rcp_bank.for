      FUNCTION PU_SET_RCP_BANK(PACKAGE)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Select the RCP bank associated with the
C-   specified package. The RCP bank must be named as follows:
C-
C-              'PX_' + package + '_RCP'
C-
C-   Use entry point PU_RESET_RCP_BANK to reset the pointers
C-   to the previously selected RCP bank.
C-
C-   Returned value  : TRUE if EZPICK done successfully.
C-   Inputs  : PACKAGE  [C*]    Name of PIXIE package
C-   Outputs : None
C-   Controls: None
C-
C-   Created  10-SEP-1990   Harrison B. Prosper
C-   Updated  15-MAY-1992   Nobuaki Oshima
C-      If there is no RCP bank, doesn't call EZPICK.
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) PACKAGE
      CHARACTER*32  RCPBANK
      LOGICAL PU_SET_RCP_BANK, PU_RESET_RCP_BANK, EZERROR
      INTEGER I,J,L,IER,LPACKAGE
C----------------------------------------------------------------------
      CALL WORD(PACKAGE(1:LEN(PACKAGE)),I,J,LPACKAGE)
C
      RCPBANK = 'PX_'//PACKAGE(1:LPACKAGE)//'_RCP'
      CALL EZLOC(RCPBANK,L)
      IF ( L .GT. 0 ) THEN
        CALL EZPICK(RCPBANK)
        PU_SET_RCP_BANK = .TRUE.
      ELSE
        PU_SET_RCP_BANK = .FALSE.
      ENDIF
      RETURN
C
      ENTRY PU_RESET_RCP_BANK
      CALL EZRSET
  999 RETURN
      END
