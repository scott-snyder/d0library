      Logical Function CDHEVT()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Low-level CD hit-finding for level-2.
C-   Convert CD raw FADC data to hits, where a hit contains
C-   drift time, pulse area, status information and a channel id.
C-
C-   Inputs  : CDD1,2,3,4 banks - raw FADC data from VTX, CDC,
C-             FDC and TRD.
C-   Outputs : CDH1,2,3 banks with hits instead of raw FADC data.
C-             The original raw data banks are dropped.
C-       data format: (1) crate headers and trailers are passed
C-                    unchanged.
C-                    (2) For each hit -
C-                     --------------------------------------------
C-                    | pulse area - 14 bits | drift time - 18 bits|
C-                     --------------------------------------------
C-                    | status - 16 bits     | channel id - 16 bits|
C-                     --------------------------------------------
C-
C-   Controls: none. out of control.
C-
C-   Created  26-April-1993   Chris Klopfenstein
C-   Modified 18-May-93 CK - returns false if hitfinding fails.
C----------------------------------------------------------------------
      implicit none
      logical vtxhit, cdchit, fdchit
      logical EZERROR
      integer ier
      logical first, ok
      data first /.true./
C----------------------------------------------------------------------
C
      CDHEVT = .true.
C
      if (first) then
        first = .false.
        call EZPick('L2CDHT_RCP')
        if (EZError(ier)) then
          call ErrMsg('L2CDHT', 'CDHEVT',
     &      'Unable to find bank CDHEVT_RCP', 'W')
          CDHEVT = .false.
          goto 999
        endif
        call EZGet('VTXHIT', vtxhit, ier)
        if (ier .ne. 0) vtxhit = .false.
        call EZGet('CDCHIT', cdchit, ier)
        if (ier .ne. 0) cdchit = .false.
        call EZGet('FDCHIT', fdchit, ier)
        if (ier .ne. 0) fdchit = .false.
        call EZRSET
      endif
C----------------------------------------------------------------------
C
C  book header bank L2CH
C
      call BKL2CH
      if (vtxhit) call l2vtx(ok)
      if (.not. ok) CDHEVT = .false.
      if (cdchit) call l2cdc(ok)
      if (.not. ok) CDHEVT = .false.
      if (fdchit) call l2fdc(ok)
      if (.not. ok) CDHEVT = .false.
  999 RETURN
      END
