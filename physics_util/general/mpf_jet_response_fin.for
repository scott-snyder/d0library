      FUNCTION mpf_jet_response_fin()

C----------------------------------------------------------------------
C-   Purpose and Methods :  to generate final response plots from mpf analysis
C-        (ie. Rj vs. Ej), fit for parameterization of D0 jet response for
C-        different jet algorithms, and close ntuple and histogram file.
C-
C-   Created  Mar-25-1994  Bob Kehoe
C-   Updated  Jun-16-1994  Bob Kehoe -- variable bins for 600 series histo's,
C-                                      weighted means for Ejet bins with
C-                                      more than one measurement, switch to
C-                                      toggle fitting added
C-   Updated  Oct-18-1995  Bob Kehoe -- change mpf include for IBM's
C----------------------------------------------------------------------

      IMPLICIT NONE
      INCLUDE 'D0$INC:QUEST.INC'
      INCLUDE 'D0$INC:MPF_JETRES.INC'
      INTEGER rmax,nput
      PARAMETER (rmax = 100)
      PARAMETER (nput = 10)
      INTEGER i,j,k,m,n,ier,icycle,index,noent,rnum_e,rnum_et
      integer qtot_nrg(rmax),qtot_low(rmax),qtot_end(rmax)
      real rcal_low(rmax,nput),sigcal_low(rmax,nput)
      REAL rcal_nrg(rmax,nput),sigcal_nrg(rmax,nput)
      real rcal_end(rmax,nput),sigcal_end(rmax,nput)
      REAL weightsum,weight,rmean,hi,hie
      real r_recoil(nbin_et)
      REAL ejet_low(nbin_et),r_low(nbin_et),err_r_low(nbin_et)
      REAL ejet_nrg(nmax_e),r_nrg(nmax_e),err_r_nrg(nmax_e)
      REAL ejet_end(nmax_e),r_end(nmax_e),err_r_end(nmax_e)
      REAL response_low(rmax),error_response_low(rmax)
      REAL response_nrg(rmax),error_response_nrg(rmax),chisq(5)
      REAL response_end(rmax),error_response_end(rmax)
      REAL step(5),pmin(5),pmax(5),rbins_e(rmax),rbins_et(rmax)
      double precision r_par(5),e_par(5),mpf_response_fit
      LOGICAL mpf_jet_response_fin,do_fit
      EXTERNAL mpf_response_fit
      COMMON /mpf_fit/ r_par,e_par

C----------------------------------------------------------------------
C-        *** initialization and booking of finished response plots ***
      WRITE(6,*) 'NEVT, NPASS ',nevt,npass
      CALL vzero(chisq,5)
      do i=1, 5
        r_par(i) = 0
        e_par(i) = 0
      enddo
      CALL hcdir(pawcdir,' ')
      CALL hcdir(filedir,' ')
C      CALL dhshow
C      CALL hprnt(nt_id)
      CALL ezpick('MPF_JET_RESPONSE_RCP')
      CALL ezgeta_i('RBINS_ET',0,0,0,rnum_et,ier)
      IF (ier.NE.0) CALL errmsg('ezgeta error','mpf_jet_response_fin',
     &    'rnum_et','F')
      CALL ezgeta('RBINS_ET',1,rnum_et,1,rbins_et,ier)
      IF (ier.NE.0) CALL errmsg('ezgeta error','mpf_jet_response_fin',
     &    'rbins_et','F')
      CALL ezgeta_i('RBINS_E',0,0,0,rnum_e,ier)
      IF (ier.NE.0) CALL errmsg('ezgeta error','mpf_jet_response_fin',
     &    'rnum_e','F')
      CALL ezgeta('RBINS_E',1,rnum_e,1,rbins_e,ier)
      IF (ier.NE.0) CALL errmsg('ezgeta error','mpf_jet_response_fin',
     &    'rbins_e','F')
      CALL ezget('do_fit',do_fit,ier)
      IF (ier.NE.0) CALL errmsg('ezget error','mpf_jet_response_fin',
     &    'do_fit','F')
      CALL ezrset
      DO j = 1,num_algo
        CALL hbookb(600+j,'Jet Response vs E?T! of Jet',
     &    (rnum_et-1),rbins_et,0.)
        CALL hbookb(610+j,'Jet Response vs E of Jet',
     &    (rnum_e-1),rbins_e,0.)
        CALL hbookb(620+j,'Jet Response vs E of Jet (EC)',
     &    (rnum_e-1),rbins_e,0.)
        CALL vzero(r_recoil,nbin_et)
      enddo

C-        PLACE RESPONSE POINTS IN EJET ARRAY, FILL NEW PLOTS AND CLOSE
      DO k = 1,nbin_et
        r_recoil(k) = hi(501,k)
      ENDDO
      DO j = 1,num_algo
        CALL vzero(ejet_low,nbin_et)
        CALL vzero(r_low,nbin_et)
        CALL vzero(err_r_low,nbin_et)
        CALL vzero(response_low,rmax)
        CALL vzero(error_response_low,rmax)
        CALL vzero(ejet_nrg,nmax_e)
        CALL vzero(r_nrg,nmax_e)
        CALL vzero(err_r_nrg,nmax_e)
        CALL vzero(response_nrg,rmax)
        CALL vzero(error_response_nrg,rmax)
        CALL vzero(ejet_end,nmax_e)
        CALL vzero(r_end,nmax_e)
        CALL vzero(err_r_end,nmax_e)
        CALL vzero(response_end,rmax)
        CALL vzero(error_response_end,rmax)
        DO k = 1,rmax
          qtot_low(k) = 0
          qtot_nrg(k) = 0
          qtot_end(k) = 0
          DO n = 1,nput
            rcal_low(k,n) = 0.
            sigcal_low(k,n) = 0.
            rcal_nrg(k,n) = 0.
            sigcal_nrg(k,n) = 0.
            rcal_end(k,n) = 0.
            sigcal_end(k,n) = 0.
          ENDDO
        ENDDO
        DO k = 1,nbin_et
          ejet_low(k) = hi(560+j,k)    ! *** get vectors from histograms
          r_low(k) = hi(510+j,k)
          err_r_low(k) = hie(510+j,k)
          if (r_recoil(k).eq.0.) then
            r_low(k) = 0.
            err_r_low(k) = 0.
          else
            r_low(k) = r_low(k)/r_recoil(k)
          endif
          DO m = 1,rnum_et             ! *** find bin for Rj vs Ejt plot
            IF ((ejet_low(k).GE.rbins_et(m)).AND.
     &            (ejet_low(k).LT.rbins_et(m+1))) then
              index = m
              qtot_low(index) = qtot_low(index) + 1
              IF (qtot_low(index).LE.nput) THEN
                rcal_low(index,qtot_low(index)) = r_low(k)
                sigcal_low(index,qtot_low(index)) = err_r_low(k)
              ENDIF
            ENDIF
          ENDDO
        ENDDO
        DO k = 1,rnum_et      ! *** calculate low Et jet response curves
          IF (qtot_low(k).GT.nput) qtot_low(k) = nput
          rmean = 0.
          weightsum = 0.
          DO m = 1,qtot_low(k)
            IF (sigcal_low(k,m).gt.0.) then
              weight = 1.0/(sigcal_low(k,m)**2.)
              rmean = rmean + rcal_low(k,m)*weight
              weightsum = weightsum + weight
            ENDIF
          ENDDO               ! *** find weighted mean of response
          IF (weightsum.GT.0) then
            response_low(k) = rmean/weightsum
            error_response_low(k) = sqrt(1.0/weightsum)
          endif
        ENDDO
        CALL hpak(600+j,response_low)
        CALL hpake(600+j,error_response_low)
        do k = 1,nmax_e
          ejet_nrg(k) = hi(570+j,k)   ! *** get vectors from initial histograms
          r_nrg(k) = hi(520+j,k)
          err_r_nrg(k) = hie(520+j,k)
          DO m = 1,rnum_e             ! *** find bin for Rj vs Ej plot
            IF ((ejet_nrg(k).GE.rbins_e(m)).AND.(ejet_nrg(k).LT.
     &            rbins_e(m+1))) then
              index = m
              qtot_nrg(index) = qtot_nrg(index) + 1
              IF (qtot_nrg(index).LE.nput) THEN
                rcal_nrg(index,qtot_nrg(index)) = r_nrg(k)
                sigcal_nrg(index,qtot_nrg(index)) = err_r_nrg(k)
              ENDIF
            ENDIF
          ENDDO
          ejet_end(k) = hi(580+j,k)     ! *** get vectors from histograms
          r_end(k) = hi(530+j,k)
          err_r_end(k) = hie(530+j,k)
          DO m = 1,rnum_e             ! *** find bin for Rj vs Ejt plot
            IF ((ejet_end(k).GE.rbins_e(m)).AND.
     &            (ejet_end(k).LT.rbins_e(m+1))) then
              index = m
              qtot_end(index) = qtot_end(index) + 1
              IF (qtot_end(index).LE.nput) THEN
                rcal_end(index,qtot_end(index)) = r_end(k)
                sigcal_end(index,qtot_end(index)) = err_r_end(k)
              ENDIF
            ENDIF
          ENDDO
        ENDDO
        DO k = 1,rnum_e       ! *** calculate medium to high energy jet response
          IF (qtot_nrg(k).GT.nput) qtot_nrg(k) = nput
          rmean = 0.
          weightsum = 0.
          DO m = 1,qtot_nrg(k)
            IF (sigcal_nrg(k,m).gt.0.) then
              weight = 1.0/(sigcal_nrg(k,m)**2.)
              rmean = rmean + rcal_nrg(k,m)*weight
              weightsum = weightsum + weight
            ENDIF
          ENDDO               ! *** find weighted mean of response
          IF (weightsum.GT.0) then
            response_nrg(k) = rmean/weightsum
            error_response_nrg(k) = sqrt(1.0/weightsum)
          endif
        ENDDO
        CALL hpak(610+j,response_nrg)
        CALL hpake(610+j,error_response_nrg)
        CALL hnoent(610+j,noent)
        DO k = 1,rnum_e      ! *** calculate forward jet response curves
          IF (qtot_end(k).GT.nput) qtot_end(k) = nput
          rmean = 0.
          weightsum = 0.
          DO m = 1,qtot_end(k)
            IF (sigcal_end(k,m).gt.0.) then
              weight = 1.0/(sigcal_end(k,m)**2.)
              rmean = rmean + rcal_end(k,m)*weight
              weightsum = weightsum + weight
            ENDIF
          ENDDO               ! *** find weighted mean of response
          IF (weightsum.GT.0) then
            response_end(k) = rmean/weightsum
            error_response_end(k) = sqrt(1.0/weightsum)
          endif
        ENDDO
        CALL hpak(620+j,response_end)
        CALL hpake(620+j,error_response_end)
C
C-          *** fitting of final response plots ***
        IF ((noent.GT.0).AND.(do_fit)) THEN
          r_par(1) = -1.0           ! *** suggested region of interest for fit
          r_par(2) = 1.1            ! *** parameters.  two separate fits are
          iquest(11) = 1            ! *** performed -- one above 20 GeV, and
          iquest(12) = 10           ! *** one below 20 GeV ('biased' region).
          CALL hfith(600+j,mpf_response_fit,'R',2,r_par,step,pmin,
     &          pmax,e_par,chisq(j))
          do i=1, 5
            e_par(i) = 0
          enddo
          r_par(1) = 1.0
          iquest(11) = 4
          iquest(12) = rnum_e
          CALL hfith(610+j,mpf_response_fit,'R',2,r_par,step,pmin,
     &          pmax,e_par,chisq(j))
        ENDIF
      ENDDO

      CALL hrout(0,icycle,' ')
      CALL hrend(tdir)
      CLOSE (unit)
      CALL rlunit(iuser,unit,ier)
      CALL hcdir('//PAWC',' ')
      mpf_jet_response_fin = .true.

  999 RETURN
      END
