      subroutine top_jet_likelihood_readhists (rcp_name)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-     Read in the contents of the top directory of the hbook
C-     file defined in the RCP bank RCP_NAME into the current
C-     hbook directory.
C-
C-     Warning: this may screw up the hbook default directories;
C-     but that works in such a bizarre fashion and is so poorly
C-     documented, that i'm not completely sure what will happen.
C-
C-   Inputs  :
C-     rcp_name : Name of the RCP file defining the hbook file.
C-
C-   Outputs : 
C-   Controls: 
C-
C-   Created  21-FEB-1994   scott snyder
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      character*(*) rcp_name
      
      character*32 me
      parameter (me = 'top_jet_likelihood_readhists')
      character*32 topdir_name
      parameter (topdir_name = 'topjetli')
      integer user_number
      parameter (user_number = 112416)

      character*128 hbook_file_name, saved_directory
      integer ier, lun, len, impure_lrec
C----------------------------------------------------------------------

      call ezpick_and_signal (rcp_name, me)
      call ez_get_string ('hbook_file_name', hbook_file_name, len, me)
      call ezrset

      call hcdir (saved_directory, 'R')

      call gtunit (user_number, lun, ier)
      if (ier .ne. 0) then
        call errmsg ('can''t reserve lun', me, ' ', 'f')
      endif

      impure_lrec = 0 ! hropen writes to this, tho the doc doesn't say so
      call hropen (lun, topdir_name, hbook_file_name, ' ',
     &             impure_lrec, ier)
      if (ier .ne. 0) then
        call errmsg ('can''t open hbook file', me, hbook_file_name, 'f')
      endif

      call hcdir (saved_directory, ' ')

      call hrin (0, 9999999, 0)

      call hrend (topdir_name)
      close (lun)

      call rlunit (user_number, lun, ier)
      if (ier .ne. 0) then
        call errmsg ('problem releasing lun', me, ' ', 'f')
      endif

      call hcdir (saved_directory, ' ')

  999 RETURN
      END
