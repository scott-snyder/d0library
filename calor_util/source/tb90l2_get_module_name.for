      FUNCTION tb90l2_get_module_name(module_number,len_name)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : given module number as defined in tb90l2_modules.
C-   def, return char string contain modulename_RANGE and len of string.
C-   used to convert module index into char string for rcp fetches
C-
C-   Returned value  : module name_RANGE
C-   Inputs  : module number
C-   Outputs : len of name
C-   Controls: none
C-
C-   Created  14-JUN-1991   James Richardson
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER trulen
      INCLUDE 'd0$params:tb90l2_modules.def'
      CHARACTER*20 tb90l2_get_module_name
      INTEGER module_number, len_name
C----------------------------------------------------------------------
C
C ****  Go over all modules
C
      IF ( module_number .EQ. TB90L2EM ) THEN
        tb90l2_get_module_name = 'TB90L2EM_RANGE'
      ELSEIF ( module_number .EQ. TB90L2FH ) THEN
        tb90l2_get_module_name = 'TB90L2FH_RANGE'
      ELSEIF ( module_number .EQ. TB90L2CH ) THEN
        tb90l2_get_module_name = 'TB90L2CH_RANGE'
      ELSEIF ( module_number .EQ. TB90L2MH ) THEN
        tb90l2_get_module_name = 'TB90L2MH_RANGE'
      ELSEIF ( module_number .EQ. TB90L2OH ) THEN
        tb90l2_get_module_name = 'TB90L2OH_RANGE'
      ELSEIF ( module_number .EQ. TB90L2ECMG ) THEN
        tb90l2_get_module_name = 'TB90L2ECMG_RANGE'
      ELSEIF ( module_number .EQ. TB90L2CCMG ) THEN
        tb90l2_get_module_name = 'TB90L2CCMG_RANGE'
      ELSEIF ( module_number .EQ. TB90L2ICD ) THEN
        tb90l2_get_module_name = 'TB90L2ICD_RANGE'
      ELSEIF ( module_number .EQ. TB90L2ICDMG ) THEN
        tb90l2_get_module_name =  'TB90L2ICDMG_RANGE'
      ENDIF
      len_name=trulen(tb90l2_get_module_name)
  999 RETURN
      END
