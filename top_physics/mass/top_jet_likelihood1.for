      real function top_jet_likelihood1 (x, y,
     &                                   denname, numname,
     &                                   rcp_name)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-     Evaluate the `likelihood' function at x,y for a specific pair
C-     of histograms.
C-
C-   Returned value  :
C-     Value of the `likelihood' function.
C-
C-   Inputs  : 
C-     x  : x-coordinate at which to evaluate function.
C-     y  : y-coordinate at which to evaluate function.
C-     denname : Name of RCP parameter giving ID of denominator histogram.
C-     numname : Name of RCP parameter giving ID of numerator histogram.
C-     rcp_name : Name of RCP back pointing to the histograms.
C-
C-   Outputs : 
C-   Controls: 
C-
C-   Created  21-FEB-1994   scott snyder
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      real x, y
      character*(*) denname, numname, rcp_name

      character*32 me
      parameter (me = 'top_jet_likelihood1')

      integer den_id, num_id, ier
      real p1, p2
      character*128 saved_directory, the_rcp_name

      real     top_jet_likelihood_getval
      external top_jet_likelihood_getval
      integer  ez_get_integer
      external ez_get_integer
C----------------------------------------------------------------------

      call top_jet_likelihood_rcp_name (rcp_name, the_rcp_name)
c
c *** Get histogram IDs.
c
      call ezpick_and_signal (the_rcp_name, me)
      den_id = ez_get_integer (denname, me)
      num_id = ez_get_integer (numname, me)
      call ezrset
c
c *** Set up the proper hbook directory.
c
      call hcdir (saved_directory, 'R')
      call dhdir (the_rcp_name, 'HBOOK_DIRECTORY', ier, ' ')
      if (ier .ne. 0) then
        call errmsg ('dhdir error', me, ' ', 'f')
      endif
c
c *** Get interpolated histogram values.
c
      p1 = top_jet_likelihood_getval (den_id, x, y, the_rcp_name)
      p2 = top_jet_likelihood_getval (num_id, x, y, the_rcp_name)

      if (p1 .ne. 0) then
        top_jet_likelihood1 = p2 / p1
      else
        top_jet_likelihood1 = 0
      endif

      call hcdir (saved_directory, ' ')

  999 RETURN
      END
