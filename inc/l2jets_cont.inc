C DEC/CMS REPLACEMENT HISTORY, Element L2JETS_CONT.INC
C *1    12-JUL-1991 00:33:25 ASTUR "Add result saving variables"
C DEC/CMS REPLACEMENT HISTORY, Element L2JETS_CONT.INC
C----------------------------------------------------------------------
C-                                                      L2JETS_CONT.INC
C-   Created   6-MAY-1990   Richard V. Astur
C-
C-  Common block L2JETS_CONT. Control variables for JETS tool
C----------------------------------------------------------------------
C---These logical control what mode we are running in.
      LOGICAL RUN_ELN                   
      LOGICAL TRGR_READY                
      LOGICAL USE_CL2                   
      LOGICAL DUMP_JAUX                 
C---These variables control how we make our decisions during this event
      LOGICAL NOW_NEW_PARAMS            
      INTEGER DO_ALL_CAND               
      INTEGER IEVT_CONT                 
      INTEGER IEVT_JAUX                 
      INTEGER NOWEVT                    
      INTEGER NOWPARAM                  
      INTEGER NOW_IND_PARAM             
C                                       
      INTEGER IND_PARAM(NPAR_MAX)       
C                                       
C                                       
      INTEGER NUM_IND_PARAMS            
C                                       
      INTEGER NUM_PARAMS                
      INTEGER TRIED_BIT(NPAR_IND_MAX)   
C this particular individual parameter set. If so, no need to process.
      INTEGER MAKE_CUTS_BIT(NPAR_MAX)   
      INTEGER BIT_PASSED(NPAR_MAX)      
      INTEGER NOWL1BIT                  
C                                       
C                                       
      LOGICAL NOWRESULT                 
      LOGICAL INIT_OK                   
C---Variables for ERRMSG utility
      INTEGER NOWERR                    
      INTEGER ERR_L2J_UTIL              
      INTEGER ERR_NO_SL2H
      INTEGER ERR_NO_RCP                
      INTEGER ERR_READ_RCP
      INTEGER ERR_PARAM_OVERFLOW
      INTEGER ERR_IND_PARAM_OVERFLOW
      INTEGER ERR_BAD_PAR_COUNT
      INTEGER ERR_UTILITY
      INTEGER ERR_BAD_PARAM_VALUE
      INTEGER ERR_HARD_MASK
      INTEGER ERR_NO_CAND
      INTEGER ERR_CAL_UNPACK
      INTEGER ERR_NONE
      INTEGER ERR_NO_TRGR
      INTEGER ERR_CAND_LIST
      PARAMETER (ERR_NONE = 0)
      PARAMETER (ERR_L2J_UTIL =2)           
      PARAMETER (ERR_NO_SL2H=3)
      PARAMETER (ERR_NO_RCP    =4)            
      PARAMETER (ERR_READ_RCP =5)
      PARAMETER (ERR_PARAM_OVERFLOW =6)
      PARAMETER (ERR_IND_PARAM_OVERFLOW =7)
      PARAMETER (ERR_BAD_PAR_COUNT  =8)
      PARAMETER (ERR_UTILITY  =9)
      PARAMETER (ERR_BAD_PARAM_VALUE  =10)
      PARAMETER (ERR_HARD_MASK  = 11)
      PARAMETER (ERR_NO_CAND  = 12)
      PARAMETER (ERR_CAL_UNPACK = 13)
      PARAMETER (ERR_NO_TRGR  = 14)
      PARAMETER (ERR_CAND_LIST  = 15)
      
C---Variables that hold Jet Calibrations:
      REAL L2JCAL_CEN                   
      REAL L2JCAL_CRA                   
      REAL L2JCAL_END                   
      REAL L2JCAL_CEN_CL2               
      REAL L2JCAL_CRA_CL2               
      REAL L2JCAL_END_CL2               
      
C
      COMMON/L2JETS_CONT/NOWPARAM,NOWL1BIT,NOWRESULT,NOWERR,NOWEVT,
     &  TRIED_BIT,BIT_PASSED,NOW_IND_PARAM,IND_PARAM,
     &  NUM_IND_PARAMS,INIT_OK,NUM_PARAMS,IEVT_CONT,DUMP_JAUX,
     &  RUN_ELN, TRGR_READY, L2JCAL_CEN,L2JCAL_CRA,L2JCAL_END,
     &  L2JCAL_CEN_CL2,L2JCAL_CRA_CL2,L2JCAL_END_CL2,USE_CL2,
     &  DO_ALL_CAND, MAKE_CUTS_BIT, IEVT_JAUX, NOW_NEW_PARAMS

