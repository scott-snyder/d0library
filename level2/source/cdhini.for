      LOGICAL FUNCTION CDHINI()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Initialize L2CDH package.
C-   Read in L2cdht rcp file. 
C-
C-   Returned value  : .true. if successful.
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  27-APR-1993   Chris Klopfenstein
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      include 'd0$inc:zebcom.inc'
      integer ier, lrcp
      character*(*) rcpfile
      parameter (rcpfile = 'L2CDHT_RCP')
      logical first, dtrddf
      data first /.true./
C----------------------------------------------------------------------
      cdhini = .true.
      if (.not. first) goto 999
      first = .false.
C  read in RCP file
      call EZLOC(rcpfile, lrcp)
      if (lrcp .le. 0) then
        call INRCP(rcpfile, ier)
        if (ier .ne. 0) then
          call ERRMSG('L2CDHT', 'CDHINI', 
     &    'Failed to find L2CDHT_RCP', 'F')
          cdhini = .false.
          goto 999
        endif
      endif
C book and set flag for reading rcp file
      call FLGBK('L2CDHT_RCP', 1)
      call FLGSET('L2CDHT_RCP', .true.)
  999 RETURN
      END
