      SUBROUTINE EZLOC (BANK1,LP)
      ENTRY GZSRCP (BANK1,LP)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return address of SRCP bank BANK1. By default
C-                         the SRCP bank which hangs below STPH (via SCPH)
C-                         is called SCPH. Use EZRNAM to rename bank if needed.
C-
C-   Inputs  : BANK1       Name of SRCP bank.
C-
C-   Outputs : LP          Address of SRCP bank (INTEGER).
C-                            LP > 0 Bank exists
C-                            LP = 0 Bank does not exist
C-
C-   Created  13-MAY-1988   Harrison B. Prosper
C-   Modified 16-MAY-1988   Name change from PNTRCP to PTSRCP
C-   Modified 15-JUN-1988   Name change from PTSRCP to GZSRCP
C-   Modified  3-OCT-1988   Integration with new SRCP routines CRSRCP
C-                          EZFILL etc.
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) BANK1
      INTEGER       LP,ID
C----------------------------------------------------------------------
      CALL EZZLOC (BANK1,LP,ID)
  999 RETURN
      END
