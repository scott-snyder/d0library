      INTEGER FUNCTION GZJPTS()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : GZJPTS returns the link to the JPTS bank under the
C-              first JETS bank.
C-
C-   Returned value  : Zebra link to JPTS banks
C-   Inputs  : None
C-   Outputs :
C-   Controls:
C-
C-   Created  20-MAR-1989   Z. Wolf
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
C--   ZEBRA
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$LINKS:IZJPTS.LINK/LIST'
C
C--   GEANT
      INCLUDE 'D0$INC:GCUNIT.INC/LIST'
C
C--   INTERNAL VARIABLES
      INTEGER LJET0,GZJETS
      INTEGER LJPT0
C----------------------------------------------------------------------
C
C--   INITIALIZE
      GZJPTS=0
C
C--   GET LINK TO FIRST JETS BANK
      LJET0=GZJETS()
      IF(LJET0.LE.0)THEN
CDBG    WRITE(LOUT,*)'GZJPTS--> LJET0.LE.0'
        GO TO 999
      END IF
C
C--   FIND LINK TO JPTS BANK UNDER FIRST JETS BANK
      LJPT0=LQ(LJET0-IZJPTS)
      GZJPTS=LJPT0
C
  999 RETURN
      END
