      FUNCTION ISCHAR ( ID)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : gives the charge of the particle , depending
C-                         on the ISAJET identifier ID.
C-
C-   Inputs  : ID     = Isajet particle ident
C-   Outputs : ISCHAR = 0 Neutral particle
C-                    = +-1 positive or negative charged particle
C-
C-   Created  14-APR-1988   Ghita Rahal-Callot
C-   Modified to use ISAJET charge, Serban Protopopescu 30-Nov-1988
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER ID,  ISCHAR
      REAL CHARGE
C----------------------------------------------------------------------
      ISCHAR=INT(CHARGE(ID))
  999 RETURN
      END
