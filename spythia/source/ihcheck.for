c****************************************************************************
      function ihcheck(IIN)
      implicit none
      integer IIN
      integer ihcheck
C
      if(IIN.eq.25) THEN
       ihcheck=72
      elseif(iin.eq.35) then
       ihcheck=73
      elseif(iin.eq.36) then
       ihcheck=74
      elseif(iin.eq.37) then
       ihcheck=75
      else
       ihcheck=iin
      endif
      return
      end
