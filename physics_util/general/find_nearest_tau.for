      SUBROUTINE FIND_NEAREST_TAU(eta, phi, lisaq, dr)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Find the nearest tau (ISAQ) to the specified
C-                         eta, phi.
C- 
C-   Inputs:  eta - Eta
C-            phi - Phi
C-   Outputs: lisaq - Link of nearest jets bank.
C-            dr  - Dr to nearest jet.
C-   
C-
C----------------------------------------------------------------------
      implicit none
      include 'd0$inc:zebcom.inc'
      integer lisaq, lisaqmin
      real eta, phi, dr, drmin, dphi, deta, phit, etat
      integer pid
      integer gzisaq
      real proxim
C----------------------------------------------------------------------
C-
C- Loop over ISAQ banks.
C-
      drmin = 1000.
      lisaq = gzisaq()
      do while (lisaq.gt.0)
        pid = iq(lisaq+1)
        if(abs(pid).eq.16)then
          etat = q(lisaq+9)
          phit = q(lisaq+7)
          deta = eta - etat
          dphi = proxim(phi-phit, 0.)
          dr = sqrt(deta**2 + dphi**2)
          if(dr.lt.drmin)then
            drmin = dr
            lisaqmin = lisaq
          endif
        endif
        lisaq = lq(lisaq)
      enddo
      dr = drmin
      lisaq = lisaqmin
 999  return
      end
