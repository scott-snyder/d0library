      SUBROUTINE MUOT_FILT(ETA,PHI,FILTPT,FILTQUAL,ESUM_OR_MUOT)
C----------------------------------------------------------------------
C-
C-   Purpose: Match by eta and phi with FILT banks
C-
C-   Inputs  :                [R] ETA,PHI of muon track
C-             
C-   Outputs : FILTPT         [R] Pt of muon from ESUM of FILT MUOT      
C-             FILTQUAL       [I] IFW4 flag of matched muon from ESUM           
C-                                or FILT MUOT
C-             ESUM_OR_MUOT   [I] = 0 (ESUM)
C-                                = 1 (FILT MUOT)
C-
C-   Created  17-JUL-1995   Paul Quintas
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------

      REAL eta, phi, filtpt, filteta, filtphi, deltamin, delta

      INTEGER filtqual, esum_or_muot
C
      CHARACTER*4 old_path
      INTEGER ntracks,i,nwam,nsam,quad,ifw1,ifw2,ifw3,ifw4
      REAL    xi,yi,zi,xmagc,ymagc,zmagc
      REAL    xcosim,ycosim,zcosim,xcosom,ycosom,zcosom
      REAL    chsqbv,chsqnb,mom,momer,elcal,elfe,bdl,ct
      REAL    px, py, pz, ptmu, theta, etamu, etamu2, phimu
C
      INCLUDE 'd0$params:esum.params'
      INTEGER nfound(id_all:last_type)
      INTEGER iflag,ier
C
      deltamin = 0.1
      filtpt = 0.
      filteta = 0.
      filtphi = 0.
      filtqual = 0
      esum_or_muot = 0
C
C IF THE FILT ESUM BANK EXISTS, USE THAT
C
      CALL gtesum_counts('FILT',nfound,ier)
      IF (nfound(id_muon).GT.0) THEN
        DO i = 1, nfound(id_muon)
          CALL gtesum('FILT',id_muon,i,ptmu,etamu,etamu2,phimu,
     &          iflag,ier)
          delta = (etamu-eta)*(etamu-eta) + (phimu-phi)*(phimu-phi)
          IF (delta.LT.deltamin) THEN
            deltamin = delta
            filtpt = ptmu
            filteta = etamu
            filtphi = phimu
            filtqual = iflag - 1
          ENDIF
        ENDDO
      ENDIF
      IF (filtpt.GT.0) RETURN
C
C IF THE ESUM BANK DOES NOT EXIST, USE THE FILT MUOT BANK
C
      CALL pathgt(old_path)
      CALL pathst('FILT')

      CALL gtmtrh(ntracks)
      IF (ntracks.GT.0) THEN
        DO i = 1, ntracks
          CALL gtmuot(i,nwam,nsam,quad,ifw1,ifw2,ifw3,ifw4,
     x      xi,yi,zi,xmagc,ymagc,zmagc,
     x      xcosim,ycosim,zcosim,xcosom,ycosom,zcosom,
     x      chsqbv,chsqnb,mom,momer,elcal,elfe,bdl,ct)

          px = abs(mom)*xcosim
          py = abs(mom)*ycosim
          pz = abs(mom)*zcosim
          ptmu = sqrt(px**2.+py**2.)
          theta = atan2(ptmu,pz)
          etamu = -alog(tan(theta/2.0))
          phimu = atan2(py,px)

          delta = (etamu-eta)*(etamu-eta) + (phimu-phi)*(phimu-phi)
          IF (delta.LT.deltamin) THEN
            deltamin = delta
            filtpt = ptmu
            filteta = etamu
            filtphi = phimu
            filtqual = ifw4
            esum_or_muot = 1
          ENDIF

        ENDDO
      ENDIF

      CALL pathst(old_path)

      RETURN
      END
