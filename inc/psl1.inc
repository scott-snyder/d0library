C----------------------------------------------------------------------
C-
C-   Created  28-JUN-1993   Richard V. Astur
C-
C----------------------------------------------------------------------
      REAL THRESH_1X1                 
      REAL THRESH_3X3                 
      REAL THRESH_5X5                 
      INTEGER COUNT_1, COUNT_2        
      REAL ET_THRESH_1, ET_THRESH_2   
      REAL ETA_LOW_1, ETA_LOW_2       
      REAL ETA_HIGH_1, ETA_HIGH_2     
      INTEGER MAX_ENTRY
      PARAMETER ( MAX_ENTRY = 88 )    
      REAL PSLDSP_ET( MAX_ENTRY )     
      REAL PSLDSP_ETA( MAX_ENTRY )    
      REAL PSLDSP_PHI( MAX_ENTRY )    
      REAL PSL1TOW( 40, 32 )          
      REAL PSL1LT( 10, 4 )            
      INTEGER NENTRY(0:4)             
      COMMON /PSL1/ PSL1TOW, PSL1LT, THRESH_1X1, THRESH_3X3, THRESH_5X5,
     &  PSLDSP_ET, PSLDSP_ETA, PSLDSP_PHI, NENTRY, COUNT_1, COUNT_2,
     &  ET_THRESH_1, ET_THRESH_2, ETA_LOW_1, ETA_LOW_2, ETA_HIGH_1,
     &  ETA_HIGH_2

