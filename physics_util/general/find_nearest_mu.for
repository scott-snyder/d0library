      SUBROUTINE FIND_NEAREST_MU(eta, phi, lisal, dr)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Find the nearest muon (ISAL) to the specified
C-                         eta, phi.
C- 
C-   Inputs:  eta - Eta
C-            phi - Phi
C-   Outputs: lisal - Link of nearest isal
C-            dr  - Dr to nearest muon.
C-   
C-
C----------------------------------------------------------------------
      implicit none
      include 'd0$inc:zebcom.inc'
      integer lisal, lisalmin
      real eta, phi, dr, drmin, dphi, deta, phimu, etamu
      integer pid
      integer gzisal
      real proxim
C----------------------------------------------------------------------
C-
C- Loop over ISAL banks.
C-
      drmin = 1000.
      lisal = gzisal()
      do while (lisal.gt.0)
        pid = iq(lisal+1)
        if(abs(pid).eq.14)then
          etamu = q(lisal+9)
          phimu = q(lisal+7)
          deta = eta - etamu
          dphi = proxim(phi-phimu, 0.)
          dr = sqrt(deta**2 + dphi**2)
          if(dr.lt.drmin)then
            drmin = dr
            lisalmin = lisal
          endif
        endif
        lisal = lq(lisal)
      enddo
      dr = drmin
      lisal = lisalmin
 999  return
      end
