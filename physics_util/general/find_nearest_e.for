      SUBROUTINE FIND_NEAREST_E(eta, phi, lisal, dr)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Find the nearest electron (ISAL) to the specified
C-                         eta, phi.
C- 
C-   Inputs:  eta - Eta
C-            phi - Phi
C-   Outputs: lisal - Link of nearest isal
C-            dr  - Dr to nearest electron.
C-   
C-
C----------------------------------------------------------------------
      implicit none
      include 'd0$inc:zebcom.inc'
      integer lisal, lisalmin
      real eta, phi, dr, drmin, dphi, deta, phie, etae
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
        if(abs(pid).eq.12)then
          etae = q(lisal+9)
          phie = q(lisal+7)
          deta = eta - etae
          dphi = proxim(phi-phie, 0.)
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
