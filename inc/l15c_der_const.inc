C----------------------------------------------------------------------
C- 
C- Created 2-NOV-1994  sJerger
C-
C- Contains common block info pertinent to the derived constant
C- array (used to fill debug block)
C-
C----------------------------------------------------------------------
      INTEGER L15C_DER_CONST_ARRAY(0:7,1:31,1:NUM_DSPS,1:15)
      INTEGER L15C_NUM_DER_CONST(1:31)
C
      COMMON /L15C_DER_CONST/L15C_DER_CONST_ARRAY,
     &                        L15C_NUM_DER_CONST
C
