C
C--     Zebra working space; Histogram Banks.
C   -   Updated  22-MAR-2004   sss - expand nnh
C
      INTEGER NNH,NHR
      PARAMETER (NNH=10000)
      PARAMETER (NHR=9)
      COMMON/ZEBHST/IXHST,IDVHST,FENHST,LCLBH,LRHST,ZHSTR
C
      INTEGER IXHST,IDVHST,FENHST(10),ZHSTR(NNH+1)
      REAL H(NNH)
      INTEGER IH(NNH),LH(NNH)
      INTEGER*2 KH(2*NNH)
      EQUIVALENCE (LCLBH,LH(1)),(LH(9),IH(1),H(1),KH(1))
      INTEGER LCLBH               
     $,  LRHST(NHR)
C
