C----------------------------------------------------------------------
C-
C-   Created  17-JUN-1994   Gregory L. Griffin
C-
C----------------------------------------------------------------------

      LOGICAL DO_HEADER, DO_FRAME_CODE, DO_FRAME_PARAM, DO_TOOL_PARAM,  
     &        DO_LDSP, DO_GDSP, DO_DEBUG 
      INTEGER MAX_HEADER_DIFFS, MAX_FRAME_CODE_DIFFS,   
     &        MAX_FRAME_PARAM_DIFFS, MAX_TOOL_PARAM_DIFFS,              
     &        MAX_LDSP_DIFFS, MAX_GDSP_DIFFS, MAX_DEBUG_DIFFS(0:4)
      LOGICAL REAL_ONLY


      COMMON /L15CT_RCP_VARS/                                           
     &  DO_HEADER, DO_FRAME_CODE, DO_FRAME_PARAM, DO_TOOL_PARAM,        
     &  DO_LDSP, DO_GDSP, DO_DEBUG,                                     
     &  MAX_HEADER_DIFFS, MAX_FRAME_CODE_DIFFS,
     &  MAX_FRAME_PARAM_DIFFS, MAX_TOOL_PARAM_DIFFS,
     &  MAX_LDSP_DIFFS, MAX_GDSP_DIFFS, MAX_DEBUG_DIFFS,
     &  REAL_ONLY
                
