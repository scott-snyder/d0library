C----------------------------------------------------------------------
C-
C-   Created  12-SEP-1989   LEVEL 2, MSU
C-
C----------------------------------------------------------------------
      COMMON /TTDBI/ TTDBI
C&IF  VAXELN,VAXVMS
C&      INTEGER*2      TTDBI(-NETAL1:NETAL1,NPHIL1)
C&C                           Thease parameters can be found in 
C&C                           L1PHP.PARAMS
C&ELSE
      INTEGER        TTDBI(-NETAL1:NETAL1,NPHIL1)
C&ENDIF
C
C                    TTDBI(IETAL1,IPHIL1) --gives--> L-1 DBI
