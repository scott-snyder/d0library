      logical function fzdiff_stcomp(l1, l2, chopt)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Compare two Zebra tree structures
C-
C-   Returned value  : .TRUE.  - Comparison succeeded (banks agree).
C-                     .FALSE. - Comparison failed.
C-   Inputs  : l1 - Link of first header bank.
C-             l2 - Link of second header bank.
C-   Controls: chopt - Character options:
C-                     Q - Quiet.  Do not print error message if banks
C-                         diagree.  Default is to print error message.
C-                     V - Verbose.
C-
C-   Created   14-Sep-1992   Herbert Greenlee
C-
C----------------------------------------------------------------------
      implicit none
      include 'd0$inc:zebcom.inc'
      include 'd0$inc:quest.inc'
      integer l1, l2              ! Links to banks
      character*(*) chopt         ! Character options
      character*1 ch
      logical ok, quiet, verbose
      integer report_level        ! Controls amount of output
      integer i
      integer ier                 ! Error flag
      logical first
      character*8 qchopt, vchopt
C-
C- Variables for flattened Zebra structure
C-
      integer maxflat
      parameter (maxflat = 5000)
      integer lfzhd(2)
      integer lflat(2,maxflat)    ! Flattened Link array
      logical precomp(maxflat)    ! Precomparison status
      integer nflat               ! Number of banks in flattened array
C-
C- Functions
C-
      logical fzdiff_comp, ezerr
      data first/.true./
C-----------------------------------------------------------------------
      fzdiff_stcomp = .true.
      if(first)then
C-
C- Get static RCP parameters
C-
        first = .false.
        call ezpick('FZDIFF_RCP')
        if(ezerr(ier))then
          call errmsg('fzdiff','fzdiff_comp',
     &      'Unable to pick SRCP bank FZDIFF_RCP','F')
        endif
        call ezget('REPORT_LEVEL', report_level, ier)
        if(ier.ne.0)then
          call errmsg('fzdiff', 'fzdiff_comp',
     &      'Error fetching RCP parameters', 'F')
        endif
      endif
C-
C- Scan options
C-
      quiet = .false.
      verbose = .false.
      do 10 i=1,len(chopt)
        call upcase(chopt(i:i), ch)
        if(ch.eq.'Q')quiet = .true.
        if(ch.eq.'V')verbose = .true.
 10   continue
      if(quiet)then
        qchopt = chopt
      else
        qchopt = 'Q'//chopt
      endif
      if(verbose)then
        vchopt = chopt
      else
        vchopt = 'V'//chopt
      endif
C-
C- Flatten the bank structure under the two header banks into a list of
C- corresponding banks.
C-
      nflat = maxflat
      lfzhd(1) = l1
      lfzhd(2) = l2
      call fzdiff_flat(lfzhd, maxflat, lflat, precomp, nflat)
C-
C- Do detailed comparison for each pair of banks in list
C-
      do 100 i=1,nflat
        if(report_level.ge.2)then
          ok = fzdiff_comp(lflat(1,i), lflat(2,i), vchopt)
        elseif(report_level.eq.1)then
          ok = fzdiff_comp(lflat(1,i), lflat(2,i), chopt)
        else
          ok = fzdiff_comp(lflat(1,i), lflat(2,i), qchopt)
          if(.not.quiet .and. .not.ok)then
            ok = fzdiff_comp(lflat(1,i), lflat(2,i), chopt)
          endif
        endif
        fzdiff_stcomp = fzdiff_stcomp .and. ok
 100  continue
 999  return
      end
