      SUBROUTINE FIND_NEAREST_OBJECT(eta, phi, nobj, lobj, eta_index, 
     &  phi_index, lobj_near, dr)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Generic nearest object finder in eta, phi
C-                         space.
C- 
C-   Inputs:  eta  - Eta
C-            phi  - Phi
C-            nobj - Number of objects.
C-            lobj - Links of objects.
C-            eta_index - Eta index in object bank.
C-            phi_index - Phi index in object bank.
C-   Outputs: lobj_near - Link of nearest object.
C-            dr   - Dr to nearest electron.
C-   
C-
C----------------------------------------------------------------------
      implicit none
      include 'd0$inc:zebcom.inc'
      integer nobj, lobj_near
      integer lobj(*)
      integer eta_index, phi_index
      integer i, l
      real eta, phi, dr, drmin, dphi, deta, phiobj, etaobj
      real proxim
C----------------------------------------------------------------------
C-
C- Loop over object banks.
C-
      drmin = 1000.
      do i = 1,nobj
        l = lobj(i)
        etaobj = q(l + eta_index)
        phiobj = q(l + phi_index)
        deta = eta - etaobj
        dphi = proxim(phi-phiobj, 0.)
        dr = sqrt(deta**2 + dphi**2)
        if(dr.lt.drmin)then
          drmin = dr
          lobj_near = l
        endif
      enddo
      dr = drmin
 999  return
      end
