C------------------------------------------------------------------
C   The INC file for using in Electron analys of D0
C   data. It is only for Level2 code.
C      Oct. 5, 91                        Yi    Xia
C------------------------------------------------------------------
      INTEGER ETA1
      PARAMETER(ETA1 = 13)
      INTEGER ETA2
      PARAMETER(ETA2 = 19)
      INTEGER PHI1
      PARAMETER(PHI1 = 64)
      INTEGER PHI2
      PARAMETER(PHI2 = 64)
      INTEGER LYR1
      PARAMETER(LYR1 = 8)
      INTEGER LYR2
      PARAMETER(LYR2 = 8)
C
      REAL CHNEN(-ETA1:ETA1,PHI1,LYR1)
      REAL CHNENE(-ETA2:ETA2,PHI2,LYR2)
      COMMON/L2ELE/CHNEN,CHNENE
