C----------------------------------------------------------------------
C-
C-   Created   13-NOV-1992   Andrew J. Milder
C-
C----------------------------------------------------------------------
      INTEGER NPMAX,NJMAX
      PARAMETER (NPMAX=1000) 
      PARAMETER (NJMAX=63)  
      COMMON /UPJET/ P_PART(4,NPMAX),ET_JET(NPMAX),P_JET(4,NJMAX),
     &  JETMAP(2,NPMAX),NP
      INTEGER JETMAP,NP,ISHARE(NJMAX)
      REAL P_PART,ET_JET,P_JET
