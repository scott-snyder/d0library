C----------------------------------------------------------------------
C-
C-   PBD_COMMON.INC - Program Builder Common Block Definition
C-
C-   Created  17-JUN-1991   Hyon Joo Kehayias
C-   Updated  28-MAY-1993   Hyon Joo Kehayias
C-     ( Added new variables LOC_PACK_NAME, LOC_PACK_LEN, and TOT_INP_PACK 
C-       changed COMB_PACK_NAME and COMB_PACK_LEN from array to simple 
C-       variables and deleted NUM_COMB_PACK )
C-
C----------------------------------------------------------------------
C
C     PBD PARAMETER DEFINITION 
C
      INTEGER*2 MAX_HOOK           
      INTEGER*2 MAX_PACK           
      INTEGER*2 MAX_OBJ            
      INTEGER*2 MAX_RCP            
      INTEGER*2 MASTER_HOOK        
      INTEGER*2 FRAME              
      INTEGER*2 COMBINED           
      INTEGER*2 NUMQUAL            
      INTEGER*2 FRAME_QUAL         
      INTEGER*2 NAME_QUAL          
      INTEGER*2 PACK_QUAL          
      INTEGER*2 LIBRARY_QUAL       
      INTEGER*2 ZEBCOM_QUAL        
      INTEGER*2 ZEBSTP_QUAL        
      INTEGER*2 ZEBWRK_QUAL        
      INTEGER*2 PAWC_QUAL          
      INTEGER*2 GCBANK_QUAL        
      INTEGER*2 PRODID_QUAL        
      INTEGER*2 VERSION_QUAL       
      INTEGER*2 PASS_QUAL          
      INTEGER*4 MAXCOM             

      PARAMETER ( MAX_HOOK = 50 )
      PARAMETER ( MAX_PACK = 50 )
      PARAMETER ( MAX_OBJ  = 100 )
      PARAMETER ( MAX_RCP  = 100 )
      PARAMETER ( MASTER_HOOK = 1 )
      PARAMETER ( FRAME = 2 )
      PARAMETER ( COMBINED = 3 )
      PARAMETER ( NUMQUAL = 21 )
      PARAMETER ( FRAME_QUAL   = 1 ) 
      PARAMETER ( NAME_QUAL    = 2 ) 
      PARAMETER ( PACK_QUAL    = 3 ) 
      PARAMETER ( LIBRARY_QUAL = 4 ) 
      PARAMETER ( ZEBCOM_QUAL  = 5 )
      PARAMETER ( ZEBSTP_QUAL  = 6 )
      PARAMETER ( ZEBWRK_QUAL  = 7 )
      PARAMETER ( PAWC_QUAL    = 8 )     
      PARAMETER ( GCBANK_QUAL  = 9 )      
      PARAMETER ( PRODID_QUAL  =10 )      
      PARAMETER ( VERSION_QUAL =11 )      
      PARAMETER ( PASS_QUAL    =12 )
      PARAMETER ( MAXCOM = 10000000 )
C
C     PBD COMMAND QUALIFIER TABLES
C
      CHARACTER*20 ZEBCOM_SIZE          
      CHARACTER*20 ZEBSTP_SIZE          
      CHARACTER*20 ZEBWRK_SIZE          
      CHARACTER*20 PAWC_SIZE            
      CHARACTER*20 GCBANK_SIZE          
      CHARACTER*32 FRAME_NAME           
      CHARACTER*32 COMBINED_NAME        
      CHARACTER*32 PACKAGE_NAME(MAX_PACK)     
      CHARACTER*32 LOC_PACK_NAME(MAX_PACK)    
      CHARACTER*80 LIBRARY_NAME         
      LOGICAL      LOG                  
      LOGICAL      COMPILE              
      LOGICAL      HISTORY              
      CHARACTER*8  PRODID               
      CHARACTER*8  VERSION              
      CHARACTER*8  PASS                 
C
C     OTHER COMMON VARIABLES
C
      CHARACTER*32 VALID_HOOK(MAX_HOOK) 
      CHARACTER*32 HOOK_NAME(MAX_HOOK)  
      CHARACTER*32 SUB_NAME(MAX_HOOK)   
      CHARACTER*80 INT_NAME(MAX_PACK,MAX_HOOK)   
      CHARACTER*80 OBJ_FILE(MAX_PACK,MAX_OBJ)    
      CHARACTER*80 RCP_FILE(MAX_RCP)    
      INTEGER*2    TOTAL_HOOKS          
      INTEGER*2    ACTION(MAX_HOOK)     
C                                         1 = SKIP, 2 = ABORT )
      INTEGER*2    NUMHOOK              
      INTEGER*2    NUMPACK              
      INTEGER*2    NUMOBJ(MAX_PACK)     
      INTEGER*2    NUMRCP               
      INTEGER*2    FRAME_LEN            
      INTEGER*2    COMBINED_LEN         
      INTEGER*2    LIBRARY_LEN          
      INTEGER*2    PACK_NAME_LEN(MAX_PACK) 
      INTEGER*2    LOC_PACK_LEN(MAX_PACK)  
      INTEGER*2    OBJ_FILE_LEN(MAX_PACK,MAX_OBJ)  
      INTEGER*2    RCP_FILE_LEN(MAX_RCP)   
      INTEGER*2    ZEBCOM_LEN           
      INTEGER*2    ZEBSTP_LEN           
      INTEGER*2    ZEBWRK_LEN           
      INTEGER*2    PAWC_LEN             
      INTEGER*2    GCBANK_LEN           
      INTEGER*2    PRODID_LEN           
      INTEGER*2    VERSION_LEN          
      INTEGER*2    PASS_LEN             
      INTEGER*2    FOR_FILE_LEN         
      LOGICAL      QUALFLAG(NUMQUAL)    
      LOGICAL      ERROR_FLAG           
      CHARACTER*32 COMB_PACK_NAME       
      INTEGER*2    COMB_PACK_LEN        
      INTEGER*2    TOT_INP_PACK         
C
      CHARACTER*80 OUTLINE              
      CHARACTER*80 COMMENT_LINE         
      CHARACTER*80 FOR_FILE_NAME        
      CHARACTER*80 COMMON_LINE(4)       
      CHARACTER*80 RET_LINE             
      CHARACTER*80 END_LINE             
      CHARACTER*80 DATE_TIME            
      CHARACTER*6  COMMENT_STR          
      CHARACTER*6  START_LINE           
      CHARACTER*6  CONT_LINE            
      INTEGER*2    HOOK_NAME_LEN(MAX_HOOK) 
      INTEGER*2    INT_NAME_LEN(MAX_PACK,MAX_HOOK) 
      INTEGER*2    SUB_NAME_LEN(MAX_HOOK) 
      INTEGER*2    RCP_OPT_LEN(MAX_RCP)   
      CHARACTER*32 RCP_OPT(MAX_RCP)       
      CHARACTER*80 INPBUF(500)            
C-
C- Common block for standard types (one machine word).
C-
      COMMON /PBDCOM/  LOG, COMPILE, HISTORY,
     &                 QUALFLAG, ERROR_FLAG
C-
C- Common block for INTEGER*2
C-
      COMMON /PBDCOM2/ TOTAL_HOOKS, ACTION, NUMHOOK,
     &                 NUMPACK, NUMOBJ, NUMRCP,
     &                 FRAME_LEN, COMBINED_LEN, LIBRARY_LEN,
     &                 PACK_NAME_LEN, OBJ_FILE_LEN, RCP_FILE_LEN,
     &                 ZEBCOM_LEN, ZEBSTP_LEN, ZEBWRK_LEN,
     &                 PAWC_LEN, GCBANK_LEN, PRODID_LEN, 
     &                 VERSION_LEN, PASS_LEN, FOR_FILE_LEN,
     &                 COMB_PACK_LEN, TOT_INP_PACK, 
     &                 HOOK_NAME_LEN, INT_NAME_LEN, SUB_NAME_LEN,
     &                 RCP_OPT_LEN, LOC_PACK_LEN
C-
C- Common block for character variables.
C-
      COMMON /PBDCOMC/ ZEBCOM_SIZE, ZEBSTP_SIZE, ZEBWRK_SIZE,
     &                 PAWC_SIZE, GCBANK_SIZE, FRAME_NAME, 
     &                 COMBINED_NAME, PACKAGE_NAME,LIBRARY_NAME, 
     &                 PRODID, VERSION, PASS, 
     &                 VALID_HOOK, HOOK_NAME, SUB_NAME, 
     &                 INT_NAME, OBJ_FILE, RCP_FILE, 
     &                 COMB_PACK_NAME, 
     &                 OUTLINE, COMMENT_LINE,FOR_FILE_NAME,COMMON_LINE,
     &                 RET_LINE, END_LINE,DATE_TIME,
     &                 COMMENT_STR, START_LINE,CONT_LINE,
     &                 RCP_OPT, INPBUF, LOC_PACK_NAME
