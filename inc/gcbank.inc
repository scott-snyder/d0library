C----------------------------------------------------------------------
C-
C-   Created  26-APR-1991   Alan M. Jonckheere - Geant 3.14
C-
C----------------------------------------------------------------------
      INTEGER NGCBNK
      PARAMETER( NGCBNK = 69000 )
      INTEGER KWBANK,KWWORK
      PARAMETER (KWBANK=NGCBNK,KWWORK=5200)
      COMMON/GCBANK/NZEBRA,GVERSN,ZVERSN,IXSTOR,IXDIV,IXCONS,FENDQ(16)
     +             ,LMAIN,LR1,WS(KWBANK)
      EQUIVALENCE (Q(1),IQ(1),LQ(9)),(LQ(1),LMAIN),(IWS(1),WS(1))
c
      INTEGER IQ(2),LQ(8000),IWS(2)
      INTEGER NZEBRA,IXSTOR,IXDIV,IXCONS,LMAIN,LR1
      REAL GVERSN,ZVERSN,FENDQ,WS,Q(1)
C
