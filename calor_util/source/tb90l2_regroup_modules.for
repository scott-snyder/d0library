      FUNCTION tb90l2_regroup_modules(module,layer,new_mod)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : This routine regroups calor modules to facilitate
C-   partial sum amongst various layers. The layers are defined in
C-   tb90l2_modules.def The first call makes a lookup table
C-
C-   Returned Value : Layer number in new module. If not in new module
C-   returns 0.
C-   Inputs  :
C-      module  -  calorimeter module
C-      layer   -  layer of cal. module
C-      new_mod -  module in new set of modules
C-   Outputs : none
C-   Controls: none
C-   Controls: none
C-
C-   Created  13-JUN-1991   James Richardson
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER tb90l2_regroup_modules
      INCLUDE 'd0$params:tb90l2_modules.def'
      INTEGER module,layer,new_mod
C
C ****  new_map is a table which is used for partial sums.
C ****  indicies :      1 - calorimeter module
C ****                  2 - layer in calorimeter module
C ****                  3 - new_module
C ****  This array contain the layer number in the new modules
C
      INTEGER new_map(NUM_OF_MODULES,MAX_LYR_NUM,NUM_MOD_GRPS)
      LOGICAL first_in                  ! true before init
      INTEGER i
      DATA first_in/.TRUE./
      SAVE first_in,new_map
C----------------------------------------------------------------------
      IF ( first_in ) THEN
        first_in = .FALSE.
        CALL
     &    vzero(new_map(1,1,1),NUM_OF_MODULES*MAX_LYR_NUM*NUM_MOD_GRPS)
C
C ****  build look up table
C
        new_map(TB90L2EM,1,EM_FH1) = 1  ! EM_FH1 module
        new_map(TB90L2EM,2,EM_FH1) = 2
        new_map(TB90L2EM,3,EM_FH1) = 3
        new_map(TB90L2EM,4,EM_FH1) = 4
        new_map(TB90L2FH,1,EM_FH1) = 5
c
        new_map(TB90L2EM,2,EM_FH_CH) = 1        ! EM_FH_CH module
        new_map(TB90L2EM,3,EM_FH_CH) = 1
        new_map(TB90L2EM,4,EM_FH_CH) = 1
        new_map(TB90L2EM,5,EM_FH_CH) = 1
        new_map(TB90L2FH,1,EM_FH_CH) = 2
        new_map(TB90L2FH,2,EM_FH_CH) = 3
        new_map(TB90L2FH,3,EM_FH_CH) = 4
        new_map(TB90L2CH,1,EM_FH_CH) = 5
c
        new_map(TB90L2EM,1,EM_FH_MH) = 1        ! EM_FH_MH module
        new_map(TB90L2EM,2,EM_FH_MH) = 1
        new_map(TB90L2EM,3,EM_FH_MH) = 1
        new_map(TB90L2EM,4,EM_FH_MH) = 1
        new_map(TB90L2FH,1,EM_FH_MH) = 2
        new_map(TB90L2FH,2,EM_FH_MH) = 3
        new_map(TB90L2FH,3,EM_FH_MH) = 4
        new_map(TB90L2MH,1,EM_FH_MH) = 5
        new_map(TB90L2MH,2,EM_FH_MH) = 6
        new_map(TB90L2MH,3,EM_FH_MH) = 7
        new_map(TB90L2MH,4,EM_FH_MH) = 8
        new_map(TB90L2MH,5,EM_FH_MH) = 9
c
        new_map(TB90L2EM,1,EM_FH_OH) = 1        ! EM_FH_OH module
        new_map(TB90L2EM,2,EM_FH_OH) = 1
        new_map(TB90L2EM,3,EM_FH_OH) = 1
        new_map(TB90L2EM,4,EM_FH_OH) = 1
        new_map(TB90L2FH,1,EM_FH_OH) = 2
        new_map(TB90L2FH,2,EM_FH_OH) = 3
        new_map(TB90L2FH,3,EM_FH_OH) = 4
        new_map(TB90L2OH,1,EM_FH_OH) = 5
        new_map(TB90L2OH,2,EM_FH_OH) = 6
        new_map(TB90L2OH,3,EM_FH_OH) = 7
c
        new_map(TB90L2EM,1,EM_FH_MH_OH) = 1      ! EM_FH_MH_OH module
        new_map(TB90L2EM,2,EM_FH_MH_OH) = 1
        new_map(TB90L2EM,3,EM_FH_MH_OH) = 1
        new_map(TB90L2EM,4,EM_FH_MH_OH) = 1
        new_map(TB90L2FH,1,EM_FH_MH_OH) = 2
        new_map(TB90L2FH,2,EM_FH_MH_OH) = 3
        new_map(TB90L2FH,3,EM_FH_MH_OH) = 4
        new_map(TB90L2MH,1,EM_FH_MH_OH) = 5
        new_map(TB90L2MH,2,EM_FH_MH_OH) = 6
        new_map(TB90L2MH,3,EM_FH_MH_OH) = 7
        new_map(TB90L2MH,4,EM_FH_MH_OH) = 8
        new_map(TB90L2MH,5,EM_FH_MH_OH) = 9
        new_map(TB90L2OH,1,EM_FH_MH_OH) = 10
        new_map(TB90L2OH,2,EM_FH_MH_OH) = 11
        new_map(TB90L2OH,3,EM_FH_MH_OH) = 12
c
        new_map(TB90L2EM,1,EM_FH_CH_MH_OH) = 1      ! EM_FH_CH_MH_OH module
        new_map(TB90L2EM,2,EM_FH_CH_MH_OH) = 1
        new_map(TB90L2EM,3,EM_FH_CH_MH_OH) = 1
        new_map(TB90L2EM,4,EM_FH_CH_MH_OH) = 1
        new_map(TB90L2FH,1,EM_FH_CH_MH_OH) = 2
        new_map(TB90L2FH,2,EM_FH_CH_MH_OH) = 2
        new_map(TB90L2FH,3,EM_FH_CH_MH_OH) = 2
        new_map(TB90L2CH,1,EM_FH_CH_MH_OH) = 3
        new_map(TB90L2MH,1,EM_FH_CH_MH_OH) = 4
        new_map(TB90L2MH,2,EM_FH_CH_MH_OH) = 4
        new_map(TB90L2MH,3,EM_FH_CH_MH_OH) = 4
        new_map(TB90L2MH,4,EM_FH_CH_MH_OH) = 4
        new_map(TB90L2MH,5,EM_FH_CH_MH_OH) = 4
        new_map(TB90L2OH,1,EM_FH_CH_MH_OH) = 5
        new_map(TB90L2OH,2,EM_FH_CH_MH_OH) = 5
        new_map(TB90L2OH,3,EM_FH_CH_MH_OH) = 5
      ENDIF
      tb90l2_regroup_modules = new_map(module,layer,new_mod)
      RETURN
      END
