      subroutine top_jet_likelihood_rcp_name (passed_rcp_name,
     &                                        the_rcp_name)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-     Handle defaulting of RCP name for top_jets_likelihood.
C-
C-   Inputs  :
C-     passed_rcp_name : RCP name as passed to a procedure.
C-
C-   Outputs :
C-     the_rcp_name : RCP name to actually use.
C-
C-   Controls: 
C-
C-   Created  21-FEB-1994   scott snyder
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      character*(*) passed_rcp_name, the_rcp_name
C----------------------------------------------------------------------

      if (passed_rcp_name .eq. ' ') then
        the_rcp_name = 'top_jet_likelihood_rcp'
      else
        the_rcp_name = passed_rcp_name
      endif

  999 RETURN
      END
