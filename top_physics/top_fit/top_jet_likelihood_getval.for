      real function top_jet_likelihood_getval (id, x, y, rcp_name)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-     Return the value of 2d histogram ID at point (X, Y).
C-     Use bilinear interpolation between the bin corners.
C-     RCP_NAME is the name of the RCP file describing the histograms,
C-     in case they haven't been read in yet.
C-
C-   Returned value  :
C-     Interpolated value of histogram ID at (x, y).
C-
C-   Inputs  :
C-     id : Histogram id to retrieve values from.
C-     x  : x-coordinate to fetch.
C-     y  : y-coordinate to fetch.
C-     rcp_name : Name of RCP file describing the histograms.
C-
C-   Outputs : 
C-   Controls:
C-   Bugs:
C-     Might not do reasonable things for x,y far outside
C-     the histogram bounds.
C-
C-   Created  21-FEB-1994   scott snyder
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      integer id
      real x, y
      character*(*) rcp_name

      character*32 me
      parameter (me = 'top_jet_likelihood_getval')

      real x1, y1, x3, y3, p1, p2, p3, p4, t, u, xlo, xhi, ylo, yhi
      integer i, j
      integer nx, ny, nwrds, ptr
      character*32 msg, tit

      real hij
      logical  hexist
      external hexist, hij
C----------------------------------------------------------------------

      if (.not. hexist (id)) then
        call top_jet_likelihood_readhists (rcp_name)
      endif

      if (.not. hexist (id)) then
        call hcdir (msg, 'R')
        print *, '-->'//msg
        write (msg, '(''id: '', i)') id
        call errmsg ('can''t find histogram', me, msg, 'f')
      endif

      call hgive (id, tit, nx, xlo, xhi, ny, ylo, yhi, nwrds, ptr)
      call hxyij (id, x, y, i, j)

      if (i .gt. nx) i = nx
      if (j .gt. ny) j = ny
      
      call hijxy (id, i,   j  , x1, y1)
      call hijxy (id, i+1, j+1, x3, y3)

      if (i .ge. nx) then
        t = 0
      else if (i .lt. 1) then
        t = 1
      else
        t = (x - x1) / (x3 - x1)
      endif

      if (j .ge. ny) then
        u = 0
      else if (j .lt. 1) then
        u = 1
      else
        u = (y - y1) / (y3 - y1)
      endif

      p1 = hij (id, i,   j)
      if (i .lt. nx) then
        p2 = hij (id, i+1, j)
      else
        p2 = p1
      endif
      if (j .lt. ny) then
        p4 = hij (id, i,   j+1)
        if (i .lt. nx) then
          p3 = hij (id, i+1, j+1)
        else
          p3 = p4
        endif
      else
        p3 = p1
        p4 = p2
      endif

      top_jet_likelihood_getval = (1-t) * (1-u) * p1 + t * (1-u) * p2 +
     &                                t *     u * p3 + u * (1-t) * p4

  999 RETURN
      END

