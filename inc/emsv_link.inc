C----------------------------------------------------------------------
C-
C-permanent link area for EMSV em candidate memory banks
C-
C-   Created  30-SEP-1994   Lewis Taylor Goss
C-
C----------------------------------------------------------------------
      INTEGER EMCAND_MAX
      PARAMETER(EMCAND_MAX = 10)
      INTEGER L2EMSV( EMCAND_MAX )
      COMMON /EMSV_LINK/ L2EMSV
