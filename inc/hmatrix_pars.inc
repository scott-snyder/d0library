C----------------------------------------------------------------------
C-
C-   Created   6-DEC-1990   Rajendran Raja
C-
C----------------------------------------------------------------------
      INTEGER VIS_MAX
      PARAMETER( VIS_MAX = 100 )
      INTEGER INVIS_MAX
      PARAMETER( INVIS_MAX = 50 )       
                                        
C
      COMMON /HMATRIX_PARS/ TOP_DIRECTORY,
     &  SUB_DIRECTORY,
     &  RZ_UNIT,KEY_DESCR,CHTAG,
     &  MSG,ACCUMULATE,NEW_RZ,TOT_DIM,VIS_DIM,INVIS_DIM,
     &  VISIBLE_QUANTITIES,INVISIBLE_QUANTITIES
C
      CHARACTER*32 SUB_DIRECTORY
      CHARACTER*80 MSG
      CHARACTER*80 TOP_DIRECTORY
      INTEGER NKEYS
      PARAMETER( NKEYS = 1 )
      CHARACTER*4 CHTAG(NKEYS),KEY_DESCR
      INTEGER RZ_UNIT
      LOGICAL ACCUMULATE,NEW_RZ
      INTEGER TOT_DIM,VIS_DIM,INVIS_DIM
      CHARACTER*32 VISIBLE_QUANTITIES(VIS_MAX)
      CHARACTER*32 INVISIBLE_QUANTITIES(INVIS_MAX)
C
