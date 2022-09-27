      INTEGER FUNCTION IEVERT(lclem,z_int,err_z_int,zclus)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Loop over CDC/FDC tracks and pick the one which best
C-                         matches the given PELC/PPHO.
C-                         Return index of the vertex preferred by that
C-                         track.
C-
C-   Returned value  : Index of preferred vertex, i.e. 1, 2,...
C-                      -1 Means no good track/cluster match found
C-                       0 Means no good track/vertex match found
C-
C-   Inputs  : lclem  link to PELC or PPHO
C-   Outputs :
C-   Controls: VERTEX_FIX_RCP
C-
C-   Created   5-JUL-1995   Peter Tamburello
C-   Updated  10-JUL-1995   Meenakshi Narain  include FDC tracks also
C-                          use track match significance for matching
C-   Updated  25-SEP-1995   Meenakshi Narain  use CM3POS_PV to get
C-                          shower centroid
C-   Updated  24-SEP-1995   Srini Rajagopalan  Add CM3POS_PV iteration
C-                          argument change: Return z_int and err_z_int.
C-   Updated   3-OCT-1995   Srini Rajagopalan  Loop only over track vertices 
C-   Updated  12-OCT-1995   Srini Rajagopalan  Return newly computed 
C-                          calorimeter cluster position 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE

      INCLUDE 'd0$inc:zebcom.inc'
      INCLUDE 'd0$inc:pi.def'

      logical qcc,ok
      logical first
      save first
      data first / .true. /

C....Links
      integer lclem
      integer lrcp
      integer ldtrk, gzdtrk
      integer lfdct, gzfdct
      integer lverh,gzverh
      integer lvert,gzvert
      integer lcacl,lcash

C....Matching
      real distance, min_distance, trksig, min_trksig
      real    sig_trkmatch_cut
      real vert_impact_cut
      real    err_dist_cc(2),err_dist_ec(4)
      integer min_ltrk
      integer i,n,ioff
      integer num_vert

C....CLUSTRK_MATCH Variables
      integer iter, max_iter, ier
      real    clus_par(3), dbar(3)
      real    weight_cut, etaphi(3), detaphi(3)
      real    trk_par(5), dtrk(4)
      real    zvert, z_int, err_z_int
      real    z0,zclus,rcal,rcd,rk
      real    dzcal,dztrk,drcal,drtrk
      real    clus_z_cutoff
      real    slope,theta,th
      real    sigma_z_cc_par(4)
      DATA  sigma_z_cc_par /0.33183,0.52281E-02,0.41968E-03,0.75496E-04/

C----------------------------------------------------------------------
      if( first ) then
        first = .false.

C....Do RCP thing
        call ezloc('vertex_fix_rcp',lrcp)
        ok = lrcp .gt. 0
        if (.not. ok) then
          call inrcp('vertex_fix_rcp',ier)
          if (ier.eq.0) call ezpick('vertex_fix_rcp')
          if (ier.eq.0) call ezerr(ier)
          if(ier.ne.0) then
            call errmsg('missing vertex_fix_rcp','ievert',
     &        'em_vertex_info','f')
          endif
          call ezrset
        end if

        call ezpick('vertex_fix_rcp')
        call ezerr(ier)
        if (ier.eq.0) call ezgeta_iarr('ERR_DISTANCE_CC',0,0,0,n,ier)
        if (ier.eq.0) call ezgeta('ERR_DISTANCE_CC',1,n,1,err_dist_cc,
     &    ier)
        if (ier.eq.0) call ezgeta_arr('ERR_DISTANCE_EC',0,0,0,n,ier)
        if (ier.eq.0) call ezgeta('ERR_DISTANCE_EC',1,n,1,err_dist_ec,
     &    ier)
        if (ier.eq.0) call ezget('SIG_TRKMATCH_CUT',sig_trkmatch_cut,
     &    ier)
        if (ier.eq.0) call ezget('VERT_IMPACT_CUT',vert_impact_cut,
     &    ier)
        if (ier.eq.0) call ezget('CLUS_Z_CUTOFF',clus_z_cutoff,ier)
        if (ier.eq.0) call ezget_i('CM3POS_MAX_ITER',max_iter,ier)
        if (ier.ne.0) then
            call errmsg('incomplete_vertex_fix_rcp','ievert',
     &      ' error reading rcp parameters ','f')
        endif
        call ezrset
C
        call ezloc('caphel_rcp',lrcp)
        ok = lrcp .gt. 0
        if (.not. ok) then
          call inrcp('caphel_rcp',ier)
          if (ier.eq.0) call ezpick('caphel_rcp')
          if (ier.eq.0) call ezerr(ier)
          if(ier.ne.0) then
            call errmsg('missing caphel_rcp','ievert',
     &        'em_vertex_info','f')
          endif
          call ezrset
        endif

        call ezpick('caphel_rcp')
        call ezerr(ier)
        if (ier.eq.0) then
          if (ier.eq.0) call ezget('WEIGHT_CUT',weight_cut,ier)
          call ezrset
        else
          call errmsg(' no_caphel_rcp','ievert',
     &        ' no rcp file to work with ','f')
        endif
      end if
C----------------------------------------------------------------------

      min_trksig = sig_trkmatch_cut
      min_ltrk = 0

C....Find cluster position

      clus_par(1) = q(lclem+23)
      clus_par(2) = q(lclem+24)
      clus_par(3) = q(lclem+25)
C
      zvert = 0.0
      lvert = gzvert(1)
      if (lvert.gt.0) zvert = q(lvert+5)
C
      lcacl = lq(lclem-2)
      if (lcacl.gt.0) then
        lcash = lq(lcacl-2)
        if (lcash.gt.0) then
          call cm3pos_pv(lcash,weight_cut,clus_par,dbar,
     &                   etaphi,detaphi,zvert)
        endif
      endif

      qcc = abs(q(lclem+19)).le.12.5   ! Is this a CC or EC cluster?
      zclus = clus_par(3)
      if (zclus.gt.0.) ioff = 0
      if (zclus.lt.0.) ioff = 1

      IF (qcc) then

C....Loop over CDC tracks and find best match to EM cluster
        ldtrk = gzdtrk(0)
        do while (ldtrk.ne.0)
          trk_par(1) = q(ldtrk+6)  ! phi
          trk_par(2) = q(ldtrk+9)  ! theta
          trk_par(3) = q(ldtrk+7)  ! x0   of xy center of gravity
          trk_par(4) = q(ldtrk+8)  ! y0   of xy center of gravity
          trk_par(5) = q(ldtrk+11) ! z0   at R0 point (uncorrected)

          call clustrk_match(clus_par,trk_par,dtrk,ier)
          
          trksig = 0.
          do i=1,2
            trksig = trksig + ( dtrk(i) / err_dist_cc(i) )**2
          enddo
          if (trksig.gt.0) trksig = sqrt(trksig)

          if (trksig.lt.min_trksig) then
            min_trksig = trksig
            min_ltrk = ldtrk
          end if
          ldtrk = lq(ldtrk)
        end do

      ELSE

C....Loop over FDC tracks and find best match to EM cluster
        lfdct = gzfdct(0)
        do while (lfdct.ne.0)
          trk_par(1) = q(lfdct+6)  ! phi
          trk_par(2) = q(lfdct+22)  ! theta
          trk_par(3) = q(lfdct+4)  ! X0   of xy center of gravity
          trk_par(4) = q(lfdct+5)  ! Y0   of xy center of gravity
          call fgetz0(iq(lfdct-5),z0)  ! Z0
          trk_par(5) = z0

          call clustrk_match(clus_par,trk_par,dtrk,ier)

          trksig   = 0.
          do i=1,2
            trksig = trksig + ( dtrk(i)
     &                           /err_dist_ec(2*ioff+i))**2
          enddo
          if (trksig.gt.0) trksig = sqrt(trksig)
          if (trksig.lt.min_trksig) then
            min_trksig = trksig
            min_ltrk = lfdct
          end if
          lfdct = lq(lfdct)
        end do

      END IF
C
      if (min_ltrk.le.0) then
        ievert = -1                 ! Bad track/cluster match
        z_int = -999.
        err_z_int = -999.
        goto 999
      endif

C....Save best track parameters

      if (qcc) then
        trk_par(1) = q(min_ltrk+6)  ! phi
        trk_par(2) = q(min_ltrk+9)  ! theta
        trk_par(3) = q(min_ltrk+7)  ! X0   of xy center of gravity
        trk_par(4) = q(min_ltrk+8)  ! Y0   of xy center of gravity
        trk_par(5) = q(min_ltrk+11) ! Z0   at R0 point (uncorrected)
      else
        trk_par(1) = q(min_ltrk+6)  ! phi
        trk_par(2) = q(min_ltrk+22) ! theta
        trk_par(3) = q(min_ltrk+4)  ! X0   of xy center of gravity
        trk_par(4) = q(min_ltrk+5)  ! Y0   of xy center of gravity
        call fgetz0(iq(min_ltrk-5),z0)  ! Z0
        trk_par(5) = z0
      end if
C
      rcal = sqrt(clus_par(1)**2+clus_par(2)**2)
      rcd  = sqrt(trk_par(3)**2+trk_par(4)**2)
      rk   = rcal/(rcal-rcd)

      z_int = clus_par(3) - rk * (clus_par(3)-trk_par(5)) 

C...fine tune z_int with a maximum of 'max_iter' iterations

      zclus = clus_par(3)
      if (lcash.gt.0) then
        do iter = 1,max_iter
          call cm3pos_pv(lcash,weight_cut,clus_par,dbar,
     &                            etaphi, detaphi, z_int)
          z_int = clus_par(3) - rk * (clus_par(3)-trk_par(5))
          if (abs(clus_par(3)-zclus).lt.clus_z_cutoff) go to 200
          zclus = clus_par(3)
        enddo
      endif

  200 continue
      zclus = clus_par(3)           ! This is the new cluster position

C... the errors on z_int

      if (qcc) then
        dztrk      = q(min_ltrk+19) ! error on Z0
        drtrk      = 0.0            ! error or R0
        drcal      = 0.0
C... (from eric)
        slope = (rcal-rcd)/(clus_par(3)-trk_par(5))
        theta = atan(slope)
        if (theta.lt.0.) theta = pi + theta
        th         = abs(theta/radian - 90.)
        dzcal      = (sigma_z_cc_par(1)+sigma_z_cc_par(2)*th) +
     &               (sigma_z_cc_par(3)+sigma_z_cc_par(4)*th) *
     &               abs(clus_par(3))
      else
        dztrk      = 0.0
        dzcal      = 0.0
        drtrk      = trk_par(3)**2 * q(min_ltrk+9)  + 
     &               trk_par(4)**2 * q(min_ltrk+13) +
     &               2.*trk_par(3)*trk_par(4)*q(min_ltrk+10)

        if (drtrk.gt.0.) drtrk = sqrt(drtrk)/rcd
        drcal = 0.1
        if (rcal.gt.60.) drcal = 0.2
      endif

C

      err_z_int = (dzcal*(1-rk))**2 + (dztrk*rk)**2 +
     &            ( (rcd*drcal)**2 + (rcal*drtrk)**2 ) *
     &            ( (clus_par(3)-trk_par(5))**2 ) /(rcal-rcd)**4

      err_z_int = sqrt(err_z_int)

C.... Loop over vertices and compare to best track


      min_distance = vert_impact_cut
      lverh = gzverh()
      num_vert = iq(lverh+2)
      do i = 1,num_vert
        
        lvert = gzvert(i)
        
        zvert = q(lvert+5)

        distance = abs(z_int - zvert)

        if (distance.lt.min_distance) then
          min_distance = distance
          ievert = i
        end if

      end do

      if (min_distance.ge.vert_impact_cut) ievert = 0  ! Bad trk/vrt match

  999 return
      end
