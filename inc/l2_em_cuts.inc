C------------------------------------------------------------------
C     Common Block for cuts for L2_EM
C      Oct. 5, 89                        Yi    Xia
C-   Updated  16-DEC-1991   James T. Linnemann  separate cuts from STP constants
C------------------------------------------------------------------
C
C...the following are fed to L2_EM as cuts.  The arrays are the full set; the
C...single values are the cuts for this call
      INTEGER NPARIN,NSETS  
      PARAMETER(NSETS = 128)
      REAL ETCUT(NSETS),EETCUT(NSETS),ELIMIT(NSETS),
     &  DELETA(NSETS),DELPHI(NSETS)
      REAL CCET_CUT(NSETS),ECET_CUT(NSETS),DETA_TR(NSETS),DPHI_TR(NSETS)
      REAL CONE_R(NSETS),CONE_FRACT_MAX(NSETS)
      INTEGER NUMBER_EM(NSETS)
      COMMON/L2_EM_CUTS/NPARIN,CCET_CUT,ECET_CUT,DETA_TR,DPHI_TR,
     &  NUMBER_EM,CONE_R,CONE_FRACT_MAX
