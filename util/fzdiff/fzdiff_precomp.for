      logical function fzdiff_precomp(l1, l2, chopt)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Preliminary comparison of two zebra banks.
C-                         Comparison fails if any of the following
C-                         do not agree.  Banks descending from l1 and
C-                         and l2 are ignored if the precomparison fails.
C-
C-                         a) Bank name.
C-                         b) Number of links.
C-                         c) Number of banks following in linear 
C-                            structure.
C-                         d) Number of data words.
C-
C-   Returned value  : .TRUE.  - Comparison succeeded (banks agree).
C-                     .FALSE. - Comparison failed.
C-   Inputs  : l1 - Link of first bank.
C-             l2 - Link of second bank.
C-   Controls: chopt - Character options:
C-                     Q - Quiet.  Do not print error message if banks
C-                         diagree.  Default is to print error message.
C-                     
C-
C-   Created   8-Sep-1992   Herbert Greenlee
C-
C----------------------------------------------------------------------
      implicit none
      include 'd0$inc:zebcom.inc'
      integer l1, l2              ! Links to banks
      character*(*) chopt
      character*1 ch
      logical quiet
      integer ns1, ns2, nr1, nr2, nd1, nd2, n1, n2
      integer i
      integer nzbank
C----------------------------------------------------------------------
      fzdiff_precomp = .true.
C-
C- Scan options
C-
      quiet = .false.
      do 10 i=1,len(chopt)
        call upcase(chopt(i:i), ch)
        if(ch.eq.'Q')quiet = .true.
 10   continue
C-
C- Compare bank name.
C-
      if(iq(l1-4).ne.iq(l2-4))then
        if(.not.quiet)print 110, iq(l1-4), iq(l2-4)
 110    format(' ****** Bank names do not agree: ',a4,2x,a4)
        fzdiff_precomp = .false.
      endif
C-
C- Compare number of structural links.
C-
      ns1 = iq(l1-2)
      ns2 = iq(l2-2)
      if(ns1 .ne. ns2)then
        if(.not.quiet)print 210, ns1, ns2
 210    format(' ****** Number of structural links does not agree: ',
     &    i4,2x,i4)
        fzdiff_precomp = .false.
      endif
C-
C- Compare number of reference links.
C-
      nr1 = iq(l1-3) - ns1
      nr2 = iq(l2-3) - ns2
      if(nr1 .ne. nr2)then
        if(.not.quiet)print 310, nr1, nr2
 310    format(' ****** Number of reference links does not agree: ',
     &    i4,2x,i4)
        fzdiff_precomp = .false.
      endif
C-
C- Compare linear structures.
C-
      n1 = nzbank(ixcom, l1)
      n2 = nzbank(ixcom, l2)
      if(n1 .ne. n2)then
        if(.not.quiet)print 410, n1, n2
 410    format(' ****** Number of linear banks does not agree: ',
     &    i4,2x,i4)
        fzdiff_precomp = .false.
      endif
C-
C- Compare number of data words.
C-
      nd1 = iq(l1-1)
      nd2 = iq(l2-1)
      if(nd1 .ne. nd2)then
        if(.not.quiet)print 510, nd1, nd2
 510    format(' ****** Number of data words do not agree: ',
     &    i4,2x,i4)
        fzdiff_precomp = .false.
      endif
  999 return
      end
