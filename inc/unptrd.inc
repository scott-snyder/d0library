C----------------------------------------------------------------------
C-
C-   Created  30-MAY-1990   J.Fr.Glicenstein
C-
C----------------------------------------------------------------------
      REAL HITTRD
      INTEGER NTFADC,NUNPWIR
      PARAMETER (NTFADC = 128)
      PARAMETER (NUNPWIR= 256)
      COMMON /UNPTRD/ HITTRD(NTFADC,6,NUNPWIR)
