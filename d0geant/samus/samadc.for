C+
      INTEGER FUNCTION SAMADC (NST, NSE, DRFDIS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Convert SAMUS drift length to ADC count
C-               Temporary simplified version with linear dependence
C-               and without electronics constants
C-
C-   Returned value  : ADC count
C-   Inputs  : NST - station number,
C-             NSE - section number,
C-             DRFDIS - drift distance in cm,
C-   Outputs : None
C-   Controls: None
C-
C-   Created  17-OCT-1990   V. Glebov & V. Podstavkov   
C-   Updated  21-MAR-1991   V. GLEBOV   
C-   Updated   9-NOV-1992   Alexander Efimov   
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      REAL    DRFDIS
      INTEGER NST, NSE
      REAL    FACT, C1, C2
      DATA FACT/1.5/, C1/3500.0/, C2/3000.0/
C
      SAMADC = C1 - DRFDIS * C2 / FACT
C
      RETURN
      END
