      subroutine emstuff(lpoint,vect)
c
      include 'd0$inc:zebcom.inc'
c
      integer lpoint
      real vect(*)
c
      integer iname,lvert,gzvert
      character*4 cname
      real zvtx
c
      equivalence (iname,cname)
c
      iname = iq(lpoint-4)
      if (cname.ne.'PELC'.and.cname.ne.'PPHO') return
c
      if (cname.eq.'PELC') then
c
c       electrons
c
        vect(1) = iq(lpoint+1)     !version
        vect(2) = iq(lpoint+2)     !id
        vect(3) = q(lpoint+3)      !ex
        vect(4) = q(lpoint+4)      !ey
        vect(5) = q(lpoint+5)      !ez
        vect(6) = q(lpoint+6)      !e
        vect(7) = q(lpoint+7)      !et
        vect(8) = q(lpoint+8)      !theta
        vect(9) = q(lpoint+9)      !eta
        vect(10) = q(lpoint+10)      !phi
        vect(11) = q(lpoint+11)      !(sigEx)**2
        vect(12) = q(lpoint+12)      !(sigEy)**2
        vect(13) = q(lpoint+13)      !sigEt
        vect(14) = q(lpoint+14)      !em e in cluster outside cent. tower
        vect(15) = q(lpoint+15)      !tot e in core cone
        vect(16) = q(lpoint+16)      !tot e in iso cone
        vect(17) = q(lpoint+17)      !em e in core cone
        vect(18) = q(lpoint+18)      !em e in iso cone
        vect(20) = q(lpoint+21)      !# tracks in cone
        vect(21) = q(lpoint+22)      !dca
        vect(22) = q(lpoint+23)      !x shower
        vect(23) = q(lpoint+24)      !y shower
        vect(24) = q(lpoint+25)      !z shower
        if (iq(lpoint+1).eq.3) then
          vect(19) = q(lpoint+19)    !cal eta
          vect(25) = iq(lpoint+30)   !quality flag
          vect(26) = iq(lpoint+31)   !e cor status
        else
          lvert = gzvert(1)
          if (lvert.gt.0) then
            zvtx = q(lvert+5)
          else
            zvtx = 0.
          endif
          call det_eta(zvtx,vect(8),vect(19))
          vect(25) = -999.
          vect(26) = -999.
        endif
      else
c
c       photons
c
        vect(1) = iq(lpoint+1)     !version
        vect(2) = iq(lpoint+2)     !id
        vect(3) = q(lpoint+3)      !ex
        vect(4) = q(lpoint+4)      !ey
        vect(5) = q(lpoint+5)      !ez
        vect(6) = q(lpoint+6)      !e
        vect(7) = q(lpoint+7)      !et
        vect(8) = q(lpoint+8)      !theta
        vect(9) = q(lpoint+9)      !eta
        vect(10) = q(lpoint+10)      !phi
        vect(11) = q(lpoint+11)      !(sigEx)**2
        vect(12) = q(lpoint+12)      !(sigEy)**2
        vect(13) = q(lpoint+13)      !sigEt
        vect(14) = q(lpoint+14)      !em e in cluster outside cent. tower
        vect(15) = q(lpoint+15)      !tot e in core cone
        vect(16) = q(lpoint+16)      !tot e in iso cone
        vect(17) = q(lpoint+17)      !em e in core cone
        vect(18) = q(lpoint+18)      !em e in iso cone
        vect(19) = q(lpoint+19)      !cal eta
        vect(20) = -999.             !# tracks in cone
        vect(21) = -999.             !dca
        vect(22) = q(lpoint+20)      !x shower
        vect(23) = q(lpoint+21)      !y shower
        vect(24) = q(lpoint+22)      !z shower
        if (iq(lpiont+1).eq.3) then
          vect(25) = q(lpoint+30)      !quality flag
          vect(26) = q(lpoint+31)      !e cor status
        else
          vect(25) = q(lpoint+23)
          vect(26) = -999.
        endif
      endif
c
      return
      end
