C                  JET.INC
C
C ** Common containing all jet variables**
C
      INTEGER MAX_JET
      PARAMETER (MAX_JET=10)
      COMMON/JET/NJET,NJETNN,JET,NNJET,IBAD,NUM_JET,IJET,
     &  BJETA,BJPHI,BJTHETA
C
      INTEGER NJET,NJETNN,IBAD,NUM_JET,IJET(MAX_JET)
      REAL    JET(8,MAX_JET),NNJET(6,MAX_JET),BJETA(MAX_JET),
     &  BJPHI(MAX_JET),BJTHETA(MAX_JET)
CC
C ** The various elements of the array are as follows
C
C            JET(1,I) = ET
C            JET(2,I) = ET_CORR
C            JET(3,I) = NEWPHI
C            JET(4,I) = NEWETA
C            JET(5,I) = FICD
C            JET(6,I) = OLDEMF
C            JET(7,I) = NCELL
C            JET(8,I) = ETAD
C
C            NNJET(1,I) = ET
C            NNJET(2,I) = PHI
C            NNJET(3,I) = ETA
C            NNJET(4,I) = FICD
C            NNJET(5,I) = FEM
C            NNJET(6,I) = NCELL
C
