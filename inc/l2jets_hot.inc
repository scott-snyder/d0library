C----------------------------------------------------------------------
C-                                                      L2JETS_HOT.INC
C-   Created   6-MAY-1990   Richard V. Astur
C-
C-    Common block L2JETS_HOT for TRGR Hot tower lists
C----------------------------------------------------------------------
      INTEGER NLIST_MAX, NJLT_MAX, NJ15_MAX, NJTT_MAX 
      PARAMETER (NLIST_MAX=128)         
      PARAMETER( NJLT_MAX = 39 )        
      PARAMETER( NJ15_MAX = 10 )        
      PARAMETER( NJTT_MAX = NLIST_MAX - NJLT_MAX - NJ15_MAX )
      INTEGER IEVT_HOT                  
                                        
      INTEGER IHOT_ADR_JT               
      INTEGER IHOT_ADR_EM               
      INTEGER NEMHOT,NJTHOT             
      INTEGER NJTTHOT                   
      INTEGER NJLTHOT, NJ15HOT          
      LOGICAL EM_COMP,JT_COMP           
      INTEGER IHOT_MSK_EM               
      INTEGER IHOT_MSK_JT               
      INTEGER IHOT_JT(2*NLIST_MAX)      
      INTEGER IHOT_EM(2*NLIST_MAX)
      INTEGER JET_MASK( NLIST_MAX )     
      COMMON /L2JETS_HOT/ IEVT_HOT,NEMHOT, NJTHOT, EM_COMP,JT_COMP,
     &  IHOT_JT, IHOT_EM, NJTTHOT, NJLTHOT, NJ15HOT, JET_MASK
