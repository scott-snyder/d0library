      SUBROUTINE L1_FW_AND_CT_DEFDUMP
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Interactively enable/disable Level 1 dump.
C-
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created  24-JAN-1992   Philippe Laurens, Steven Klocek
C-              Moved this routine from an entry point of L1_FW_AND_CT_DUMP.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      LOGICAL YES
C
      YES = .TRUE.
C
      CALL GETPAR(1,' Generate formatted dump of the TRGR bank?[Y]:',
     &  'L',YES)
      CALL OUTMSG(' ')
      CALL L1_FW_AND_CT_DUMP_ENABLE(YES)
      IF (YES .EQV. .TRUE.) THEN
        CALL L1DMP_SELECT_SECTIONS(.TRUE.)
      ENDIF
C
  999 RETURN
      END
