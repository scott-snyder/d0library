C----------------------------------------------------------------------
C-
C-   Created   9-DEC-1992   Richard V. Astur
C-
C----------------------------------------------------------------------
      INTEGER QCD_TO_D0(0:QCD_BIT_MAX-1)           
      INTEGER D0_TO_QCD(0:127)                    
      INTEGER QCD_PRESCALE(0:QCD_BIT_MAX-1)        
C: L2 Filter definitions
      INTEGER QCD_L2_NJETS(0:QCD_BIT_MAX-1)        
      REAL QCD_L2_THRESH(0:QCD_BIT_MAX-1)          
      LOGICAL QCD_L2_END(0:QCD_BIT_MAX-1)          
C: QCD cuts applied to DST's,STA's
      INTEGER N_QCD_BITS
      INTEGER N_QCD_CUTS                          
      PARAMETER( N_QCD_CUTS = 1 )     
C  
      INTEGER N_BITS_SEEN( 0:QCD_BIT_MAX )         
C  
      INTEGER N_BITS_DROPPED( 0:QCD_BIT_MAX-1, N_QCD_CUTS+1) 
C  
C
      COMMON /QCD_BIT/ QCD_TO_D0, D0_TO_QCD, QCD_PRESCALE, QCD_L2_NJETS,
     &  QCD_L2_THRESH, QCD_L2_END, N_BITS_DROPPED, N_BITS_SEEN,
     &  N_QCD_BITS

