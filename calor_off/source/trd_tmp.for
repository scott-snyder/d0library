      SUBROUTINE TRD_TMP(LCACL,ERROR,ENERGY)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : reads the TRD energy 
C-                         (temporary version, no correction)
C-                          
C-
C-   Inputs  : LCACL (integer)  link to PELC bank
C-   Outputs : ERROR (logical)  true if OK, false if problem (for instance
C-                              ERROR will be false for electrons which
C-                              do not cross the TRD)
C-             ENERGY (real(5)) ENERGY(1) energy in layer 1
C-                              ENERGY(2) energy in layer 2             
C-                              ENERGY(3) energy in layer 3             
C-                              ENERGY(3) total energy            
C-                              ENERGY(3) truncated energy
C-   
C-   Controls: none
C-
C-   Created  10-FEB-1994   Alain PLUQUET
C-
C----------------------------------------------------------------------
      implicit none
      integer layer,lcacl,lztrk,ltrdt,ltprl
      real energy(5)
      logical error
      include 'd0$inc:zebcom.inc'
      call vfill(energy,5,0.)
      if (lcacl.gt.0) then
        lztrk=lq(lcacl-6)
        if (lztrk.gt.0) then
          ltrdt=lq(lztrk-9)          
          if (ltrdt.gt.0) then
            do layer=1,3
              ltprl=lq(ltrdt-layer)
              if (ltprl.gt.0) then
                energy(layer)=q(ltprl+12)
              endif
            enddo
            energy(4)=energy(1)+energy(2)+energy(3)
            energy(5)=energy(4)-max(energy(1),energy(2),energy(3))
            error=.true.
          else
            error=.false.
          endif
        else
          error=.false.          
        endif
      else
        error=.false.
      endif

      end
