C----------------------------------------------------------------------
C-permanent       LINKS FOR CL2_XXX ROUTINES FOR STP BANKS 
C-   Created  26-APR-1991   James T. Linnemann
C-
C----------------------------------------------------------------------
      INTEGER NCL2_CLINK
      PARAMETER( NCL2_CLINK = 20 )
      INTEGER CL2_CLINK
      COMMON /CL2_STP_LINK/ CL2_CLINK(NCL2_CLINK)
      INTEGER LL2CADT(12)
      EQUIVALENCE ( LL2CADT, CL2_CLINK )
      INTEGER L2CAGS
      EQUIVALENCE ( L2CAGS, CL2_CLINK(13) )
      INTEGER LSL2H
      EQUIVALENCE ( LSL2H, CL2_CLINK(14) )
      
