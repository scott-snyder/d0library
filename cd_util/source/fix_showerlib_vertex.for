C CMS REPLACEMENT HISTORY, Element FIX_SHOWERLIB_VERTEX.FOR
C *1    13-OCT-1995 11:57:32 DHIMAN "by Scott Snyder"
C CMS REPLACEMENT HISTORY, Element FIX_SHOWERLIB_VERTEX.FOR
      logical function fix_showerlib_vertex()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-
C-     Try to fix the VERT bank for early showerlibrary MC data.
C-
C-     Showerlibrary is a problem for vertexing due to the small number
C-     of tracks contained in the data.  Any VERT banks built by the
C-     the vertexing algorithms are likely to be crap.  Unfortunately,
C-     different versions of reco have dealt with this in different
C-     ways:
C-
C-       - For RECO <= 11.18, there was no general convention.
C-         We leave the VERT bank alone if it exists, otherwise,
C-         we make one at z=0.
C-
C-       - For RECO 11.19, the ISAJET vertex was used by CAHITS,
C-         CAPHEL, and MURECO, but it wasn't stored in a VERT bank.
C-         We replace any existing VERT bank with one containing
C-         the ISAJET vertex.  This won't fix all the problems,
C-         since some other parts of reco used the reconstructed VERT
C-         information rather than the ISAJET vertex.  C'est la vie.
C-
C-       - For RECO 12, the ISAJET vertex was used, and it was
C-         also put into a VERT bank.  We need do nothing for this case.
C-
C-   A further problem is that we want to do the above actions only
C-   for showerlibrary Monte Carlo events.  It's easy enough to tell
C-   if the data is Monte Carlo, but there is no reliable way
C-   to find out whether or not showerlibrary was used.  So we require
C-   the user to tell us via a RCP parameter in FIX_SHOWERLIB_VERTEX_RCP.
C-   The package initialization routine will then set the flag
C-   FIX_SHLIB_VERTEX based on this parameter.  Other code could also
C-   in principle set this flag should it think it can do better.
C-
C-   The entry point fix_showerlib_vertex_oldvert gives people a way
C-   to look at what the primary version was before this package
C-   screwed with it.
C-
C-   Created  26-SEP-1995   scott snyder
C-   Updated  20-MAR-2004   sss - compile with g77
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      include 'd0$inc:zebcom.inc'
      real zv_dum, dz_dum
      logical fix_showerlib_vertex_oldvert

      integer reco_version_major, reco_version_pass
      integer lisv1, lvert, lverh, nr
      real zv, dz

      real old_zv, old_dz
      save old_zv, old_dz

      logical  monte_carlo_data, flgval
      external monte_carlo_data, flgval

      integer  gzisv1, gzverh, gzvert
      external gzisv1, gzverh, gzvert
C----------------------------------------------------------------------

      fix_showerlib_vertex = .true.
c
c *** Stash away the current Z and DZ for the primary vertex.
c
      lvert = gzvert (1)
      if (lvert .ne. 0) then
        old_zv = q(lvert+5)
        old_dz = q(lvert+8)
      else
        old_zv = 0
        old_dz = 0
      endif
c
c *** Don't do anything if this isn't MC.
c
      if (.not. monte_carlo_data ()) return
c
c *** Also don't need to do anything if this is >= v12 reco.
c
      call reco_version (reco_version_major, reco_version_pass)
      if (reco_version_major .ge. 12) return
c
c *** Don't change anything if the showerlibrary flag is off.
c
      if (.not. flgval ('FIX_SHLIB_VERTEX')) then
        call errmsg ('Reco <12',
     &               'FIX_SHOWERLIB_VERTEX',
     &               'Data assumed not SHLB',
     &               'W')
        call errmsg (' Reco <12',
     &               'FIX_SHOWERLIB_VERTEX',
     &               'Set SHOWERLIB_DATA if it is',
     &               'W')
        return
      endif

      if (reco_version_pass .le. 18) then
c
c ***   Reco version is <= 11.18.
c ***   If a VERT bank exists, just issue a warning and return without
c ***   doing anything.  Otherwise, we need to build a new VERT bank
c ***   at z=0.
c
        lvert = gzvert (1)
        if (lvert .eq. 0) then
          call errmsg ('Reco <= 11.18 SHLB',
     &                 'FIX_SHOWERLIB_VERTEX',
     &                 'No VERT found; set z=0',
     &                 'W')
          zv = 0
          dz = 100
        else
          call errmsg (' Reco <= 11.18 SHLB',
     &                 'FIX_SHOWERLIB_VERTEX',
     &                 'Proper vertex unknown; take VERT',
     &                 'W')
          return
        endif
      else
c
c ***   Ok, we're going to replace the existing primary vertex
c ***   with the ISAJET vertex.  Try to find the ISAJET vertex.
c
        lisv1 = gzisv1 ()
        if (lisv1 .ne. 0) then
          call errmsg ('RECO 11.19 showerlib',
     &                 'FIX_SHOWERLIB_VERTEX',
     &                 'Using ISAJET vertex',
     &                 'E')
          zv = q(lisv1+9)
          dz = 0
        else
          call errmsg ('RECO 11.19 showerlib',
     &                 'FIX_SHOWERLIB_VERTEX',
     &                 'No ISAJET vertex; set z=0',
     &                 'E')
          zv = 0
          dz = 100
        endif
      endif
c
c *** Get a VERT bank.  Either zero out the existing primary
c *** vertex, or, if one doesn't exist, make a new one.
c
      lvert = gzvert (1)
      if (lvert .ne. 0) then
        call vzero (iq(lvert+1), iq(lvert-1)) ! Zap data
        nr = iq(lvert-3) - iq(lvert-2)
        call vzero (lq(lvert-iq(lvert-3)), nr) ! Zap reference links
        call mzpush (ixcom, lvert, -nr, 0, 'R') ! Delete ref links
      else
        call bkvert (lvert, 0)
        if (lvert .eq. 0) return
        lverh = gzverh ()
        iq(lverh+2) = iq(lverh+2) + 1
      endif
c
c *** Fill in the ISAJET information.
c
      iq(lvert+1) = 0
      iq(lvert+2) = ibset (0, 31)
      q(lvert+5) = zv
      q(lvert+8) = dz
c
c *** Done!
c

  999 RETURN



      entry fix_showerlib_vertex_oldvert (zv_dum, dz_dum)
      zv_dum = old_zv
      dz_dum = old_dz
      fix_showerlib_vertex_oldvert = .true.
      return
      END
