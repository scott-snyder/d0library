C                  PHOT.INC
C
C ** Common containing all electron variables**
C
      INTEGER MAX_PHO
      PARAMETER (MAX_PHO=10)
      COMMON/PHOT/NPHOT,IPHOT,RPHOT,IGPHOT
C
      INTEGER NPHOT,IPHOT(4,MAX_PHO),IGPHOT(MAX_PHO)
      REAL    RPHOT(16,MAX_PHO)
CC
C ** The various elements of the array are as follows
C
C        IPHOT(1,I)=NPHOT
C        IPHOT(2,I)=NCELL
C        IPHOT(3,I)=CLEANEM1
C        IPHOT(4,I)=CLEANEM2
C
C        RPHOT(1,I)=ET
C        RPHOT(2,I)=PHI
C        RPHOT(3,I)=ETA
C        RPHOT(4,I)=THE
C        RPHOT(5,I)=DET_ETA
C        RPHOT(6,I)=CHISQ
C        RPHOT(7,I)=TCSQ
C        RPHOT(8,I)=FISO
C        RPHOT(9,I)=SIGET
C        RPHOT(10,I)=CORR
C        RPHOT(11,I)=FEM
C        RPHOT(12,I)=ET_ISOL1
C        RPHOT(13,I)=ET_ISOL2
C        RPHOT(14,I)=MT
C        RPHOT(15,I)=DPHIMET
C
C    The following variables are filled for only for the sake of computing
C    other quantities later on. The variables till RELEC(15,I) were filled
C    for output to ntuple.
C
C     RPHOT(16,I)=E
C
C
C    Most of the variables are filled by the routine GET_PHOTONS. Some
C  of the variables not filled are:
C
C         TCSQ
C         MT
C         DPHIMET
C
C    NPHOT as filled by GET_PHOTONS is the total number of electrons.
C    PHOTON_CUTS changes it to the number of good electrons.
C
