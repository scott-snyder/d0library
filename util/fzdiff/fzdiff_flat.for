      subroutine fzdiff_flat(lfzhd, maxflat, lflat, precomp, nflat)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Flatten two Zebra structures in linear arrays
C-                         of corresponding banks.
C-
C-   Inputs  : lfzhd   - Header banks (2-word array)
C-             maxflat - Size of lflat array.
C-   Outputs : lflat   - Array of links of flattened structures.
C-             precomp - Precomparison status for a the pair of banks.
C-             nflat   - Number of banks in lflat array.
C-
C-   Created   8-Sep-1992   Herbert Greenlee
C-
C-   Notes:
C-
C-   1.  This routine is based on a tree-walking algorithm that makes use
C-       if the following 4 motions, which are attempted in the order
C-       given below.
C-
C-       a) Down via the first unused structural link.
C-
C-       b) Next bank in linear structure via next link.
C-
C-       c) Up via up link.
C-
C-   2.  Each pair of banks encountered is added to the flattend bank list.
C-
C-   3.  The down motion is attempted only if precomparison succeeds for
C-       a given pair of banks.  All of the other motions are attempted
C-       regardless of whether precomparison succeeds or fails.
C-
C-   4.  Tree walking finishes when no motions are possible (i.e. because 
C-       we have returned returned to the root).
C-
C-   5.  Reference links are ignored.
C-
C----------------------------------------------------------------------
      implicit none
      include 'd0$inc:zebcom.inc'
      integer lfzhd(2)            ! Two header banks
      integer maxflat             ! Size of link array
      integer lflat(2,maxflat)    ! Flattened Link array
      integer nflat               ! Number of banks in flattened array
C-
C- Tree-walking variables.
C-
      integer maxlevel            ! Maximum tree level
      parameter (maxlevel=100)
      integer level               ! Current tree level (1 = root).
      integer nl(maxlevel)        ! Number of structural links descending
                                  ! from current banks.
      integer il(maxlevel)        ! Current structural link.
      logical precomp(maxlevel)   ! Precomparison status
      integer l1, l2              ! Links of current banks
      integer nxt1, nxt2          ! Next links from current banks
C-
C- Functions
C-
      integer fzdiff_struc        ! Return next structural link
      integer fzdiff_precomp      ! Return precomparison of banks
C----------------------------------------------------------------------
      l1 = lfzhd(1)
      l2 = lfzhd(2)
      level = 1
      nflat = 0
C-
C- Top of tree walking loop.  We come here each time we encounter a new pair
C- of banks.
C-
 100  continue
C-
C- Add current banks to bank array and do precomparison.
C-
      if(nflat .ge. maxflat)then
        call errmsg('fzdiff', 'fzdiff_flat', 
     &    'Maximum number of banks exceeded', 'W')
        go to 999
      endif
      nflat = nflat + 1
      lflat(1, nflat) = l1
      lflat(2, nflat) = l2
      precomp(level) = fzdiff_precomp(l1, l2, 'q')
C-
C- Obtain number of structural links.
C-
      nl(level) = min0(iq(l1-2), iq(l2-2))
      il(level) = 0
C-
C- Here we attempt down motion via the first available structural link.
C-
 200  continue
      if(nl(level).eq.0)go to 300
      il(level) = fzdiff_struc(l1, l2, il(level))
      if(il(level).gt.nl(level))go to 300
      if(level.ge.maxlevel)then
        call errmsg('fzdiff', 'fzdiff_flat', 
     &    'Maximum level exceeded' ,'W')
        go to 300
      endif
      l1 = lq(l1-il(level))
      l2 = lq(l2-il(level))
      level = level + 1
      go to 100
C-
C- Here we attempt linear motion.
C-
 300  continue
      nxt1 = lq(l1)
      nxt2 = lq(l2)
      if(nxt1.eq.0 .or. nxt2.eq.0)go to 400
      l1 = nxt1
      l2 = nxt2
      go to 100
C-
C- Here we attempt up motion.
C-
 400  continue
      if(level.le.1)go to 999
      l1 = lq(l1+1)
      l2 = lq(l2+1)
      if(l1.eq.0 .or. l2.eq.0)call zfatal
      level = level - 1
      go to 200
  999 return
      end
