      SUBROUTINE FILL_JETS_INFO
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : setup array with jets related information
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  10-APR-1995   Meenakshi Narain
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKC.INC'
      INCLUDE 'D0$INC:pi.def'
      INCLUDE 'D0$INC:TOP_DILEP_ANALYSIS.INC'
      INTEGER I,K,NPARTICLE
      INTEGER JET_COUNT
      REAL    MTM_VEC(4,MAX_PARTICLE),JET(4,NWANT_JET)
      REAL    XDATA(1000),OLDETA,OLDPHI,OLDE(5),OLDEMF
      REAL    NEWEN,NEWETA,NEWPHI,NEWE(5)
      REAL    ET_CORR,ET_CORRLO,ET_CORRHI,ETAD,THETA,UNCORR_ET
      INTEGER IER, KMAX, NTAGS
      LOGICAL OVERWRITE
C----------------------------------------------------------------------
      nparticle = 0
      DO i = 1,4
        ht(i) = 0
      ENDDO
      htej = 0
      jet_count = 0
      CALL gtslink('TOP_JETS', nwant_jet, ntot_jet, jets_link)
      njet = min(nwant_jet, ntot_jet)
      ixjet(1) = njet
      DO i=1,njet
        ljets = jets_link(i)
        CALL ucopy(q(ljets+2),olde,5)
        oldphi = q(ljets+8)
        oldeta = q(ljets+9)
        oldemf = q(ljets+14)
        newe(1) = q(ljets+2)
        newe(2) = q(ljets+3)
        newphi  = q(ljets+8)
        neweta  = q(ljets+9)
        newen   = q(ljets+5)
        et_corr = sqrt(newe(1)**2+newe(2)**2)
        et_corrhi = 0.
        et_corrlo = 0.
        overwrite = .false.
        CALL UNCORRECT_JETS_BANK(LJETS,OVERWRITE,IER)
        CALL GET_UNCORRECT_JETS_BANK(OLDE,OLDPHI,THETA,OLDETA,OLDEMF)
        uncorr_et = sqrt(olde(1)**2+olde(2)**2)
        xjet(i+1) = uncorr_et
        xjet(i+1*nwant_jet+1) = et_corr     ! corrected Et
        xjet(i+2*nwant_jet+1) = newe(1)     ! Ec_x
        xjet(i+3*nwant_jet+1) = newe(2)     ! Ec_y
        xjet(i+4*nwant_jet+1) = newphi      ! phi
        xjet(i+5*nwant_jet+1) = neweta      ! eta
        xjet(i+6*nwant_jet+1) = q(ljets+17) ! ficd
        xjet(i+7*nwant_jet+1) = oldemf      ! emf
        ixjet(i+8*nwant_jet+1) = iq(ljets+16)!ncell
        theta    = 2*atan(exp(-neweta))
        CALL det_eta(zvert,theta,etad)
        xjet(i+9*nwant_jet+1) = etad       ! det_eta
        ixjet(i+10*nwant_jet+1) = iq(ljets+21) ! Ntowers 90%
        xjet(i+11*nwant_jet+1) =  q(ljets+18) ! CH fraction
        xjet(i+12*nwant_jet+1) =  q(ljets+19) ! Ratio of hottest Et - next
        xjet(i+13*nwant_jet+1) =  q(ljets+12) ! rms eta
        xjet(i+14*nwant_jet+1) =  q(ljets+13) ! rms phi
        xjet(i+15*nwant_jet+1) =  q(ljets+27) ! jet pointing word
C count total particles
        nparticle = nparticle + 1
        IF(nparticle.LE.max_particle)THEN
          DO k=1,3
            mtm_vec(k,nparticle) = newe(k)
            jet(k,nparticle) = newe(k)
          ENDDO
          mtm_vec(4,nparticle) = newen
          jet(4,nparticle) = newen
        ENDIF
        nparticle_jets = nparticle
        if (et_corr.ge.15) then
          ht(1) = ht(1) + et_corr
          IF (abs(neweta).LE.2.5) ht(2) = ht(2) + et_corr
          IF (abs(neweta).LE.2.0) ht(3) = ht(3) + et_corr
          IF (abs(neweta).LE.1.0) ht(4) = ht(4) + et_corr
          IF (abs(neweta).LE.2.5) then
            htej = htej + et_corr
          endif
        endif
      ENDDO
  999 RETURN
C...........................................................................
      ENTRY JETS_INFO(NVAR,XDATA)
      nvar = nwant_jet*nvar_jet+1
      CALL ucopy(xjet,xdata,nvar)
      RETURN
C...........................................................................
      ENTRY JETS_TAGS(NTAGS,TJETS,KMAX)
      KMAX = nwant_jet
      ntags = nvar_jet+1
      TJETS(1)='NJETS'
      TJETS(2)='ETJ'
      TJETS(3)='ETCJ'
      TJETS(4)='ETCXJ'
      TJETS(5)='ETCYJ'
      TJETS(6)='PHIJ'
      TJETS(7)='ETAJ'
      TJETS(8)='FICDJ'
      TJETS(9)='EMFJ'
      TJETS(10)='NCELLJ:I'
      TJETS(11)='DETAJ'
      TJETS(12)='NT90J:I'
      TJETS(13)='CHFJ'
      TJETS(14)='HOTETJ'
      TJETS(15)='RMSETAJ'
      TJETS(16)='RMSPHIJ'
      TJETS(17)='VERTINFOJ'
      RETURN
      END
