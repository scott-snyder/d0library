C----------------------------------------------------------------------
C-
C-   Created   07-JAN-1992  Andrew G. Brandt
C-   Updated   03-NOV-1994  Andrew G. Brandt convert to CW allow 20 objects
C-
C----------------------------------------------------------------------
C
C QCD_JUTL_JETS1
C
      INTEGER MAX_TT
      PARAMETER (MAX_TT=20)
C
      COMMON /QCD_JUTL_JETS1/NTT,TT_ETA(MAX_TT),TT_PHI(MAX_TT),
     &         TT_ET(MAX_TT)
C
      INTEGER NTT
      REAL TT_ETA,TT_PHI,TT_ET
      CHARACTER*255 QCD_JUTL_JETS1_TAG
      DATA QCD_JUTL_JETS1_TAG/'NTT[0,20],TT_ETA(NTT):R,TT_PHI(NTT):R,
     &       TT_ET(NTT):R'/
C
C QCD_JUTL_JETS2
C
      INTEGER MAX_LJ
      PARAMETER (MAX_LJ=20)
      COMMON /QCD_JUTL_JETS2/NLJ,LJ_ETA(MAX_LJ),LJ_PHI(MAX_LJ),
     &         LJ_ET(MAX_LJ)
      REAL LJ_ETA,LJ_PHI,LJ_ET
      INTEGER NLJ
      CHARACTER*255 QCD_JUTL_JETS2_TAG
      DATA QCD_JUTL_JETS2_TAG/'NLJ[0,20],LJ_ETA(NLJ):R,LJ_PHI(NLJ):R,
     &       LJ_ET(NLJ):R'/
C
C QCD_JUTL_JETS3
C
      INTEGER MAX_L2
      PARAMETER (MAX_L2=20)
      COMMON /QCD_JUTL_JETS3/NL2,CONESZ,L2_ETA(MAX_L2),L2_PHI(MAX_L2),
     &         L2_ET(MAX_L2),L2_EMF(MAX_L2)
      REAL CONESZ,L2_ETA,L2_PHI,L2_ET,L2_EMF
      INTEGER NL2
      CHARACTER*255 QCD_JUTL_JETS3_TAG
      DATA QCD_JUTL_JETS3_TAG/'NL2[0,20],L2CONE:R,L2_ETA(NL2):R,
     &     L2_PHI(NL2):R,L2_ET(NL2):R,L2_EMF(NL2):R'/
