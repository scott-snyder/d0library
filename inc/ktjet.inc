C----------------------------------------------------------------------
C-
C-   Created  26-JUL-1992   Richard V. Astur
C-
C-   Updated  9-oct-1995    Changed ktcl_header_size from 2 to 3 words
C-                          Gordon Watts
C-   Updated  2-Nov-1995    Added test histogram dump flag
C-                          Gordon Watts
C----------------------------------------------------------------------
      REAL COM_E
      PARAMETER( COM_E = 1800. )        
      INTEGER KTMAX_CUTS                
      PARAMETER( KTMAX_CUTS = 30 )
      INTEGER IPRINT                    
      INTEGER KCF_SEE                   
      INTEGER  N_KTALG                  
      INTEGER  N_KTCUTS                 
      INTEGER INPUT_TYPE                
      INTEGER INPUT_TYPE_KT( KTMAX_CUTS) 
      REAL   D_SEP_KT( KTMAX_CUTS )    
      REAL   F_CUT_KT( KTMAX_CUTS )    
      REAL   D_SEP, F_CUT
      LOGICAL DO_PRECLUSTER            
      LOGICAL DO_POSTCLUSTER           
      LOGICAL MAKE_KTCL                
      REAL PREC_WIDTH                  
      REAL MINIMUM_ET                  
      REAL KT_ET_MIN                   
      REAL NEGATIVE_ET_CUT             
      REAL DELTA_ET                    
      REAL ET_REMOVED                  
      INTEGER LARGE_NEGATIVE_CELLS     
                                       
      REAL MIN_PREC_ET                 
      LOGICAL ET_ORDERED               
      LOGICAL ETA_ORDERED              
      INTEGER SEARCH_SIZE              
      LOGICAL REMOVE_LOW_ET_CELLS      
      LOGICAL DO_SUBJETS               
      INTEGER NJETS                    
      INTEGER ISUBJET                  
      INTEGER IFSCALE                  
      INTEGER NSCALE                   
      INTEGER IETDEF, IETADEF          
      REAL     FSCALE                  
      REAL KTCUT( 0:KTMAX_CUTS )        
      LOGICAL KTLOG10                   
C                                       
      INTEGER IKTCUT                    
      LOGICAL KT_USE_ET                 
      REAL KT_MAX_CHAN                  
      LOGICAL KT_MAKE_JETS              
      LOGICAL USE_DET_ETA               
                                        
      REAL XMCUT_MAX,DCUT
      PARAMETER( XMCUT_MAX = 10.*1800.**2)  
      INTEGER MAX_GIAN, GIAN_BLOCK_SIZE
      PARAMETER (MAX_GIAN = 18000)
      PARAMETER (GIAN_BLOCK_SIZE = 10)
      REAL XGIAN(MAX_GIAN)
      INTEGER NGIAN

      LOGICAL DO_BASIC_HIST             

      COMMON /KTJET/ INPUT_TYPE, KTCUT, N_KTCUTS, IKTCUT,
     &  N_KTALG, DO_PRECLUSTER, DO_SUBJETS, D_SEP_KT, F_CUT_KT,
     &  INPUT_TYPE_KT, IFSCALE, NSCALE,
     &  IPRINT, KT_USE_ET, KT_MAX_CHAN, DCUT, NGIAN, XGIAN, KCF_SEE,
     &  KTLOG10, NJETS, D_SEP, F_CUT, FSCALE, ISUBJET, PREC_WIDTH,
     &  IETDEF, IETADEF, MAKE_KTCL, DO_POSTCLUSTER, MINIMUM_ET,
     &  KT_MAKE_JETS,USE_DET_ETA,KT_ET_MIN,NEGATIVE_ET_CUT,
     &  DELTA_ET,LARGE_NEGATIVE_CELLS,ET_REMOVED,MIN_PREC_ET,
     &  ET_ORDERED,ETA_ORDERED,SEARCH_SIZE,REMOVE_LOW_ET_CELLS,
     &  DO_BASIC_HIST


C
C  These parameters are to help work with the kvec bank.  They define
C  offsets for each and ever block in there.  Note there is a 4 word
C  offset at the start for status information.
C
C  I can't find what the zero'th offset word is.  Must be hidden
C

      INTEGER KVEC_BLOCK_SIZE
      PARAMETER (KVEC_BLOCK_SIZE = 15)

      INTEGER KVEC_POINT
      PARAMETER (KVEC_POINT = 0)

      INTEGER KVEC_EX, KVEC_EY, KVEC_EZ, KVEC_E
      PARAMETER (KVEC_EX = 1)
      PARAMETER (KVEC_EY = 2)
      PARAMETER (KVEC_EZ = 3)
      PARAMETER (KVEC_E = 4)

      INTEGER KVEC_ET, KVEC_ETA, KVEC_PHI
      PARAMETER (KVEC_ET = 5)
      PARAMETER (KVEC_ETA = 6)
      PARAMETER (KVEC_PHI = 7)

      INTEGER KVEC_NCELL, KVEC_EM, KVEC_ICD, KVEC_FH
      INTEGER KVEC_CELL_EM, KVEC_CELL_ICD, KVEC_CELL_FH

      PARAMETER (KVEC_NCELL = 8)
      PARAMETER (KVEC_EM = 9)
      PARAMETER (KVEC_ICD = 10)
      PARAMETER (KVEC_FH = 11)
      PARAMETER (KVEC_CELL_EM = 12)
      PARAMETER (KVEC_CELL_ICD = 13)
      PARAMETER (KVEC_CELL_FH = 14)

C
C  parameters for the ktcl bank
C

      INTEGER KTCL_BLOCK_SIZE, KTCL_HEADER_SIZE
      PARAMETER (KTCL_BLOCK_SIZE = 2)
      PARAMETER (KTCL_HEADER_SIZE = 3)


