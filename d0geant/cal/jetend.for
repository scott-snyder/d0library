      SUBROUTINE JETEND
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : JETEND is called at the end of each event.  It calls
C-              routines to finish the JETS and JPTS banks.
C-
C-   Inputs  : None
C-   Outputs : None
C-   Controls:
C-
C-   Created  20-OCT-1988   Z. Wolf
C-   Updated  20-JAN-1989   Z. Wolf
C-   Updated  24-MAR-1989   Z. Wolf
C----------------------------------------------------------------------
      IMPLICIT NONE
C
C--   ZEBRA
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
C
C--   INTERNAL VARIABLES
      INTEGER LJET0,GZJETS
C----------------------------------------------------------------------
C
C--   LOCATE FIRST JETS BANK
      CALL PATHST('GEAN')
      LJET0=GZJETS()
C
C--   FINISH JETS BANK
      CALL ENJETS(LJET0)
C
C--   FINISH JPTS BANK
      CALL ENJPTS(LJET0)
C
  999 RETURN
      END
