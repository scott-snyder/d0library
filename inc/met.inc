C                  MET.INC
C
C ** Common containing MISSING ET variables**
C
      INTEGER MAX_TYPE
      PARAMETER (MAX_TYPE=5)
      COMMON/MET/META
C
      REAL    META(8,MAX_TYPE)
CC
C ** The various elements of the array are as follows
C
C        META(1,I) = MET
C        META(2,I) = MET_PHI
C        META(3,I) = SCAL_MET
C        META(4,I) = MET_CORR
C        META(5,I) = MET_PHI_CORR
C        META(6,I) = ERRMET_X
C        META(7,I) = ERRMET_Y
C        META(8,I) = SIGMET
