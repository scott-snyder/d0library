C----------------------------------------------------------------------
C-
C-   Created  15-NOV-1990   Stephen Kahn
C-
C----------------------------------------------------------------------
      INTEGER NMPHI
      PARAMETER (NMPHI=64)      
      REAL SINROT, COSROT
      COMMON /SINTBL/ SINROT(0:NMPHI), COSROT(0:NMPHI)
