C----------------------------------------------------------------------
C-
C-   Name     BFSRCP.INC
C-   Purpose  Buffers used by RXSRCP
C-   Created  9-NOV-1988   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      INTEGER ITYPE (MAXVAL)
      REAL    RVALUE(MAXVAL)
      COMMON /BFSRCP/ RVALUE,ITYPE
