C----------------------------------------------------------------------
C-
C-      /SCCCH_MR_BEAMPIPE/ 
C-
C-      CCCH Special Module containing Main Ring beam pipe
C-      position parameters
C-
C-      Note: Dimensions are in CM 
C-
C-   Created  12-JAN-1989   Stuart Fuess
C-
C----------------------------------------------------------------------
      REAL CCCH_MR_MODULE_X             
      REAL CCCH_MR_MODULE_Y             
      REAL CCCH_MR_MODULE_ANGLE         
      REAL CCCH_MR_FLOOR_OFFSET         
      REAL CCCH_MR_CUTOUT_DX            
      REAL CCCH_MR_CUTOUT_DZ            
      COMMON /SCCCH_MR_BEAMPIPE/ CCCH_MR_MODULE_X,
     &                          CCCH_MR_MODULE_Y,
     &                          CCCH_MR_MODULE_ANGLE,
     &                          CCCH_MR_FLOOR_OFFSET,
     &                          CCCH_MR_CUTOUT_DX,
     &                          CCCH_MR_CUTOUT_DZ
