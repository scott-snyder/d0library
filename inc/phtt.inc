C----------------------------------------------------------------------
C-
C-   Created  12-SEP-1989   LEVEL 2, MSU
C-
C----------------------------------------------------------------------
      COMMON /PHTT/ PHTTETA,PHTTPHI
      INTEGER     PHTTETA(-NETAL:NETAL)
      INTEGER     PHTTPHI(NPHIL)
C                   PHTTETA(IETAC) <-- L1ETAC
C                   PHTTPHI(IPHIC) <-- L1PHIC
C
C     --- Gives the L-1 (L1ETAC,L1PHIC) indices of the TT the ROT, --- 
C         specified by (IETAC,IPHIC), is apart of.                     
