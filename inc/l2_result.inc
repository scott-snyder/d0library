C----------------------------------------------------------------------
C-
C-   Created   4-AUG-1992   Richard V. Astur
C-
C----------------------------------------------------------------------
      INTEGER L2JR_NODE
      INTEGER L2JR_MAX                       
      PARAMETER( L2JR_MAX = 4 )
      INTEGER L2JR_ID( L2JR_MAX )            
      INTEGER L2JR_NHAVE( L2JR_MAX )         
      INTEGER L2JR_NWANT( L2JR_MAX )         
      INTEGER L2JR_NXY( 2, L2JR_MAX )        
      REAL    L2JR_XYMIN(2, L2JR_MAX )       
      REAL    L2JR_XYMAX(2, L2JR_MAX )       
      LOGICAL L2JR_RESET                     
      INTEGER L2JR_NEVT                      
      COMMON /L2_RESULT/ L2JR_NODE, L2JR_ID, L2JR_NHAVE, L2JR_NWANT,
     &  L2JR_NXY, L2JR_NEVT, L2JR_RESET, L2JR_XYMAX, L2JR_XYMIN
