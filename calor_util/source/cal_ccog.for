      SUBROUTINE CAL_CCOG(lcash,sigma,r_cog,phi_cog,z_cog,
     &                                r_ccog,phi_ccog,z_ccog,error)

C----------------------------------------------------------------------
C-
C-   Purpose and Methods : This routine is intended to calculate the 
C-                         center of electromagnetic showers in the 
C-                         D0 EM calorimetry. The energies and packed
C-                         addresses of the cells comprising the cluster
C-                         are contained in the CASH bank. This routine
C-                         uses the "corrected center-of-gravity (ccog)" 
C-                         algorithm to improve the position resolution. 
C-                         (see below for reference.)
C-                         As written, it is intended for use in both 
C-                         the EC and CC EM calorimeters. Some tuning
C-                         will still be required.
C-
C-   Inputs  : lcash        pointer to CASH bank
C-             sigma        corresponds to the exponential radial falloff 
C-                          of the shower
C-
C-   Outputs : r_ccog(4)    radial position of EM1-4
C-             phi_ccog(4)  phi position of EM1-4
C-             z_ccog(4)    z position of EM1-4
C-             error(4)     0 if OK
C-
C-   Controls:
C-
C-   Created  14-APR-1992   Norman A. Graf
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:PI.DEF'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INTEGER lcash,i,j,ncells,pointer,pakadr,floor
      INTEGER ieta,iphi,layer
      INTEGER ieta_hot(4),iphi_hot(4),layer_hot(4)
      INTEGER iok,error(4)
      REAL xcell,ycell,zcell,xcog(4),ycog(4),zcog(4)
      REAL energy,wtsum(4),sigma,sigmaphi
      REAL z_cog(4),r_cog(4),phi_cog(4)
      REAL cenz,delz,cenr,delr,cenphi,delphi
      REAL z_hi,z_lo,r_hi,r_lo,phi_hi,phi_lo
      REAL zdist,rdist,phidist
      REAL z(4),r(4),phi(4)
      REAL z_ccog(4),r_ccog(4),phi_ccog(4)
      REAL arcsinh
      LOGICAL skip
C----------------------------------------------------------------------
C
C ****  zero some things...
C
      DO i = 1,4
        r(i)       = 0.
        r_ccog(i)  = 0.
        phi(i)     = 0.
        phi_ccog(i)= 0.
        z(i)       = 0.
        z_ccog(i)  = 0.
        wtsum(i)   = 0.
        xcog(i)    = 0.
        ycog(i)    = 0.
        zcog(i)    = 0.
        error(i)   = 0
      ENDDO
C
C ****  calculate energy-weighted center-of-gravity
C
      ncells = iq(lcash+2)
      pointer=1
C
C ****  loop over cells..
C
      DO i = 1,ncells
        skip = .false.
        pointer = pointer+2
        pakadr = iq(lcash+pointer)
        energy = q(lcash+pointer+1)
        CALL CAEP_INDICES(PAKADR,IETA,IPHI,LAYER)
C
C ****  Which floor? Condense EM3, skip if not EM.
C
        IF(layer.LT.lyem3a) THEN
          floor = layer
        ELSEIF(layer.GE.lyem3a .AND. layer.LE.lyem3d) THEN
          floor = 3
        ELSEIF(layer.EQ.mxlyem) THEN
          floor = 4
        ELSE
          skip = .true.
        ENDIF
        if (.NOT. skip) then
C
C ****  get position of cell center from geometry database
C
          CALL CELXYZ(ieta,iphi,layer,Xcell,Ycell,Zcell,IOK)
          xcog(floor) = xcog(floor) + xcell*energy
          ycog(floor) = ycog(floor) + ycell*energy
          zcog(floor) = zcog(floor) + zcell*energy
          wtsum(floor) = wtsum(floor) + energy
        endif
      ENDDO
      DO i = 1,4
        IF(wtsum(i) .GT. 0) THEN
          xcog(i) = xcog(i)/wtsum(i)
          ycog(i) = ycog(i)/wtsum(i)
          zcog(i) = zcog(i)/wtsum(i)
        ELSE
          error(i) = 1
        ENDIF
      ENDDO
C
C ****  Now for the corrections...
C
C
C ****  Xccog = Xcog + sigma*arcsinh[(Xcog/Delta)*sinh(Delta/sigma)]
C       where:
C
C       sigma is a parameter corresponding to the exponential radial
C             falloff of the shower
C
C       Delta is the module half-width
C
C       Xcog is taken modulo the interval [-Delta,Delta]
C
C ****  G.A. Akopjanov et al. NIM 140(1977) 441.
C
      DO i = 1,4
        IF(error(i).EQ.0) THEN
C
C ****  get calorimeter indices for cell containing c.o.g.
C
          CALL CPOSPH(Xcog(i),Ycog(i),Zcog(i),
     &              IETA_hot(i),IPHI_hot(i),LaYeR_hot(i),IOK)
          IF(iok.NE.0) THEN
            error(i) = 1
          ELSE
C
C ****  get coordinates and coordinate extent of cell containing c.o.g
C
            CALL CALPHI( IPHI_hot(i), IETA_hot(i), CENPHI, DELPHI, IOK)
            CALL sub_cell_width(ieta_hot(i),iphi_hot(i),layer_hot(i),1,
     &        cenr,delr,cenz,delz,iok)
C
C ****  get coordinates of c.o.g.
C
            if(xcog(i).eq.0 .and. ycog(i).eq.0) then
              phi_cog(i) = 0.
            else  
              phi_cog(i) = ATAN2(ycog(i),xcog(i))
              if(phi_cog(i).lt.0) phi_cog(i) = phi_cog(i) + twopi
            endif
            r_cog(i) = sqrt(xcog(i)**2 + ycog(i)**2)
            z_cog(i) = zcog(i)
C
C ****  now find (r,phi,z) coordinate modulo [-Delta,Delta]
C
            zdist = z_cog(i) - cenz
            rdist = r_cog(i) - cenr
            phidist = phi_cog(i) - cenphi
C
C ****  Now to correct...
C
            z_ccog(i) = zdist/(delz/2)*sinh((delz/2)/sigma)
            z_ccog(i) = cenz + sigma*arcsinh(z_ccog(i))
c
            r_ccog(i) = rdist/(delr/2)*sinh((delr/2)/sigma)
            r_ccog(i) = cenr + sigma*arcsinh(r_ccog(i))
c
C::: n.b. need to use r*phi here, to get units correct
c
            sigmaphi = sigma/r_ccog(i)
            phi_ccog(i) = phidist/(delphi/2)*sinh((delphi/2)/sigmaphi)
            phi_ccog(i) = cenphi + sigmaphi*arcsinh(phi_ccog(i))
c
          ENDIF
        ENDIF
      ENDDO
  999 RETURN
      END
