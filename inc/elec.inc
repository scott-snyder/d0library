C                  ELEC.INC
C
C ** Common containing all electron variables**
C
      INTEGER MAX_ELE
      PARAMETER (MAX_ELE=10)
      COMMON/ELEC/NELEC,IELEC,RELEC,IGELEC,NFAKE_ELEC
C
      INTEGER NELEC,IELEC(6,MAX_ELE),IGELEC(MAX_ELE),NFAKE_ELEC
      REAL    RELEC(22,MAX_ELE)
CC
C ** The various elements of the array are as follows
C
C     IELEC(1,I)=NCELL
C     IELEC(2,I)=CLEANEM1
C     IELEC(3,I)=CLEANEM2
C     IELEC(4,I)=NTRK
C     IELEC(5,I)=DTRK
C     IELEC(6,I)=NTRKCON
C
C     RELEC(1,I)=ET
C     RELEC(2,I)=PHI
C     RELEC(3,I)=ETA
C     RELEC(4,I)=THETA
C     RELEC(5,I)=DET_ETA
C     RELEC(6,I)=CHISQ
C     RELEC(7,I)=TCSQ
C     RELEC(8,I)=FISO
C     RELEC(9,I)=SIGET
C     RELEC(10,I)=CORR
C     RELEC(11,I)=FEM
C     RELEC(12,I)=ETISOL1
C     RELEC(13,I)=ETISOL2
C     RELEC(14,I)=DEDX
C     RELEC(15,I)=RDPHI
C     RELEC(16,I)=DZ_DR
C     RELEC(17,I)=MATCH_SIG
C     RELEC(18,I)=TRDINFO
C     RELEC(19,I)=TRDMEAN
C     RELEC(20,I)=MT
C     RELEC(21,I)=DPHIMET
C
C    The following variables are filled for only for the sake of computing
C    other quantities later on. The variables till RELEC(21,I) were filled
C    for output to ntuple.
C
C     RELEC(22,I)=E
C
C    Most of the variables are filled by the routine GET_ELECTRONS. Some
C  of the variables not filled are:
C
C         TCSQ
C         DZ_DR
C         MT
C         DPHIMET
C    As of now RDPHI is actually only dphi.
C
C    NELEC as filled by GET_ELECTRONS is the total number of electrons.
C    ELECTRON_CUTS changes it to the number of good electrons.
C
