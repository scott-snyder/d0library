      SUBROUTINE PYTBFL(NEVENTS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book and fill PYTHIA begin-of-run bank
C-
C-   Inputs  : NEVENTS : Number of events to generate for this run
C-   Outputs : 
C-   Controls: 
C-
C-   Created  30-JAN-1991   Norman A. Graf
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:PRIMAR.INC'
      INCLUDE 'D0$INC:ISAUNT.INC'
      INCLUDE 'D0$LINKS:IZISAB.LINK'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER IOHEAD,IOISAB,IRN,LOUT,LISAB,NREAC,IUH
      INTEGER NEVENTS
      LOGICAL FIRST
C
      COMMON/LUJETS/N,K(4000,5),P(4000,5),V(4000,5)
      COMMON/LUDAT3/MDCY(500,3),MDME(2000,2),BRAT(2000),KFDP(2000,5)
      COMMON/PYSUBS/MSEL,MSUB(200),KFIN(2,-40:40),CKIN(200)
      COMMON/PYPARS/MSTP(200),PARP(200),MSTI(200),PARI(200)
      INTEGER N,K,MDCY,MDME,KFDP,MSEL,MSUB,KFIN,MSTP,MSTI
      REAL P,V,BRAT,CKIN,PARP,PARI
C
      DATA FIRST/.TRUE./
      INTEGER IRUN
C
C----------------------------------------------------------------------
C
C  create head bank
      CALL MZWIPE(0)      ! make sure division is wiped before starting
      CALL BKHEAD
C  identify this as a HERWIG initial record
      IQ(LHEAD+1)=1001
      CALL UCTOH('PYTH',IUH,4,4)
      IQ(LHEAD+2)=IUH
      CALL UCTOH('BEG ',IUH,4,4)
      IQ(LHEAD+3)=IUH
      IQ(LHEAD+6)=1111
      IQ(LHEAD+14)=1
      LOUT=LHEAD+2
C
C  create Zebra bank ISAB
C
      IF(FIRST) CALL MZFORM('ISAB','3I,-F',IOISAB)
      CALL MZBOOK(IXMAIN,LISAB,LHEAD,-IZISAB,
     $            'ISAB',2,2,22,IOISAB,-1)
       IQ(LISAB+1)=54                     !  PYTHIA version 5.4
       IQ(LISAB+2)=MSTI(1)                ! PYTHIA PROCESS NUMBER
       IQ(LISAB+3)=NEVENTS
       Q(LISAB+4)=PARI(11)                ! CM energy
C       CALL UCOPY(PTMIN,Q(LISAB+5),6)
c       CALL UCOPY(THMIN,Q(LISAB+11),6)
c       CALL UCOPY(PHIMIN,Q(LISAB+17),6)
       FIRST=.FALSE.
C----------------------------------------------------------------------
  999 RETURN
      END
