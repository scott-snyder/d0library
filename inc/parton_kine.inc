C----------------------------------------------------------------------
C-
C-   Created   8-FEB-1993   Rajendran Raja
C-
C----------------------------------------------------------------------
      COMMON /PARTON_KINE/SHAT(5),THAT(5),UHAT(5),TAU,XF,X1,X2,
     &  X1P(4),X2P(4),ROOTS_TEV,PCM,SH,TH,UH,ROOT,STR_FN1,
     &  STR_FN2,QSQ,ALPHA_S,TMASS,L1,L2,CONV_PB,IQ,IH,TESTQ,IS1,IS2
      DOUBLE PRECISION    L1,L2
      DOUBLE PRECISION    SHAT,THAT,UHAT,TAU,XF,X1,X2
      DOUBLE PRECISION    X1P,X2P,ROOTS_TEV,PCM,SH,TH,UH,ROOT
      DOUBLE PRECISION    STR_FN1,STR_FN2,QSQ,ALPHA_S,TMASS,CONV_PB
      INTEGER IQ,IH,IS1,IS2
      LOGICAL TESTQ
C
C Shat,that and uhat are the 5 vectors in parton parton cms
C Tau xf x1,x2 are the usual parton kine quantities
C roots_tev = tevatron cms energy
C PCM is the proton momentum in CMS
C th,uh,sh are that,uhat and shat
C Str_fn1,str_fn2 are the Proton and Pbar structure functions
C QSQ is the qsquared at which the structure functions are evaluated
C TMASS = top mass
C ALPHA_S = alpha strong
C IQ,IH are the parton and hadron flags for STRUC
C TESTQ IF TRUE WILL HISTOGRAM FUNCTIONS BY ENTERING A TEST MODE
C CONV_PB CONVERTS GEV-2 TO PB
C IS1,IS2 are the solutions
C L1,L2 are Lepton energies in top 1 and top 2 restframes
C 
C          ISTRUC=3 FOR EICHTEN, HINCHLIFFE, LANE, AND QUIGG (1984) 
C                   SOLUTION 1  
C          ISTRUC=4 DUKE AND OWENS, PHYS. REV. D30, 49. 
C                   SOLUTION 1  
C          QUARK TYPES--    
C          IQ=1  2  3  4  5  6  7  8  9  10 11 12 13    
C             GL UP UB DN DB ST SB CH CB BT BB TP TB    
C          HADRON TYPES--   
C          IH=+1120  -1120  +1220  -1220    
C                 P     AP      N     AN    
C                 
C ISAJET COMMON BLOCK
      COMMON/QCDPAR/ALAM,ALAM2,CUTJET,ISTRUC    
      REAL    ALAM,ALAM2,CUTJET
      INTEGER ISTRUC
