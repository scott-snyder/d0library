C----------------------------------------------------------------------
C-
C-   Created  11-JUL-1995   Bob Hirosky
C-   Modified 02-SEP-1995   Andrew G. Brandt rename to L0HITS
C----------------------------------------------------------------------
C
C ****  level0 block
C
      INTEGER NHITS_SCN,NHITS_SCS,NHITS_LCN,NHITS_LCS
      REAL    TIME_N,TIME_S
      LOGICAL HITS_SCN(20),HITS_SCS(20),HITS_LCN(16),HITS_LCS(16)

      COMMON /L0HITS/ NHITS_SCN,NHITS_SCS,NHITS_LCN,NHITS_LCS,
     &  HITS_SCN,HITS_SCS,HITS_LCN,HITS_LCS,TIME_N,TIME_S

      CHARACTER*256 CH_L0
      DATA CH_L0 /'NHITS_SCN[0,20],NHITS_SCS[0,20],NHITS_LCN[0,16],
     &  NHITS_LCS[0,16],HITS_SCN(20):L,HITS_SCS(20):L,HITS_LCN(16):L,
     &  HITS_LCS(16):L,TIME_N:R,TIME_S:R'/
