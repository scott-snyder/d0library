      SUBROUTINE EM_VERTEX_INFO(NEM,VERT_ID,EM_VERT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return vertex information associated with EM objects.
C-
C-   Inputs  : 
C-   Outputs : NEM          Number of good EM objects found
C-             VERT_ID()    VERT number associated with each EM object
C-             EM_VERT(1,*) z,err_z,zclus of largest ET EM object with NO cuts
C-             EM_VERT(2,*) z,err_z,zclus of largest ET EM object with cuts
C-   Controls: 
C-
C-   Created  10-JUL-1995   Meenakshi Narain
C-   Updated  24-SEP-1995   Srini Rajagopalan  Add EM_VERT stuff
C-   Updated  12-OCT-1995   Srini Rajagopalan  Return new cal. cluster postion. 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      include 'd0$inc:zebcom.inc'
      include 'd0$inc:zlinkc.inc'
      integer gzpelc, gzppho
      integer ievert,kvert
      integer status,mvar,ier
      integer i,j
      integer max_em
      parameter (max_em=30)

      integer nem,vert_id(max_em)

      real    chisq,fisol,cquan(50)
      real    em_et_cut,em_et_vert_cut,chisq_cut,fisol_cut
      real    z_int,err_z_int,zclus,em_vert(2,3)
      real    max_et1,max_et2

      logical ok,elect
      logical first/.true./
C----------------------------------------------------------------------
      if (first) then
        first = .false.
        call ezpick('VERTEX_FIX_RCP')
        call ezget('EM_ET_CUT',em_et_cut,ier)
        if (ier.eq.0) call ezget('EM_ET_VERT_CUT',em_et_vert_cut,ier)
        if (ier.eq.0) call ezget('CHISQ_CUT',chisq_cut,ier)
        if (ier.eq.0) call ezget('FISOL_CUT',fisol_cut,ier)
        if (ier.ne.0)
     &     call errmsg('incomplete_vertex_fix_rcp','em_vertex_info',
     &                 ' error reading rcp parameters ','F')
        call ezrset 
      endif
C
      max_et1 = em_et_vert_cut
      max_et2 = em_et_vert_cut
      do i = 1,2
        do j = 1,3
          em_vert(i,j) = -999.        ! error condition
        enddo
      enddo
C
      lclem = gzpelc()
      if (lclem.gt.0) then
        elect  = .true.
      else
        elect = .false.
        lclem = gzppho()
      endif
      nem = 0
      call vzero(vert_id(1),max_em)
      do while (lclem.gt.0)
        CALL cleanem(lclem,0,ok,status)
        CALL cleanem_cquans(mvar,cquan)
        chisq = cquan(4)     
        fisol  = cquan(13)   
        kvert = ievert(lclem,z_int,err_z_int,zclus)
        
        if (kvert.gt.0) then
C
C store vertex information of largest ET EM object with no quality cuts
C
          if (q(lclem+7).gt.max_et1) then
            max_et1 = q(lclem+7)
            em_vert(1,1) = z_int
            em_vert(1,2) = err_z_int
            em_vert(1,3) = zclus
          endif
C
C store vertex information of largest ET EM object with cuts.
C In addition store vert_id for *all* good EM objects.
C
          if (chisq.le.chisq_cut .and. fisol.le.fisol_cut) then
            if (q(lclem+7).gt.max_et2) then
              max_et2 = q(lclem+7)
              em_vert(2,1) = z_int
              em_vert(2,2) = err_z_int
              em_vert(2,3) = zclus
            endif
C
            if (q(lclem+7).gt.em_et_cut) then
              nem = nem+1
              vert_id(nem)  = kvert
            endif
          endif

        endif
        lclem = lq(lclem)
        if (lclem.eq.0 .and. elect) then
          elect = .false.
          lclem = gzppho()
        endif
      end do

  999 RETURN
      END
