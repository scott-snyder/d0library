      FUNCTION tb90l2_ntuple_fill()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : fills the ntuple word
C-   Call from exm_post_analysis hook of examine2
C-
C-   Returned value  : true
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created  28-JUN-1991   James Richardson
C-   Updated   8-JAN-1992   Chip Stewart - added hcdir to //PAWC  
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL tb90l2_ntuple_fill
      LOGICAL tb90l2_ntuple_fill_reset
      INCLUDE 'd0$params:tb90l2_ntuple.def'
      REAL    ntuple(NTUPLE_SIZE)
C----------------------------------------------------------------------
      tb90l2_ntuple_fill = .true.
      CALL tb90l2_ntuple_camac_fill(ntuple(CAMAC_BGN_O+1))
      CALL tb90l2_ntuple_trk_fit_fill(ntuple(TRK_FIT_BGN_O+1))
      CALL tb90l2_ntuple_calorim_fill(ntuple(CALORIM_BGN_O+1))
      CALL hcdir('//PAWC',' ')
      CALL hcdir('//NTUPLE',' ')
      CALL hfn(1000,ntuple)
      RETURN
C#######################################################################
      ENTRY tb90l2_ntuple_fill_reset
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 0 out ntuple words for next run
C-
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created  24-JUL-1991   James Richardson
C-
C----------------------------------------------------------------------
      CALL vzero(ntuple(1),NTUPLE_SIZE)
      RETURN
      END
