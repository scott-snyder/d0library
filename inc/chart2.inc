C----------------------------------------------------------------------
C-
C-   Created  28-AUG-1990   Robert Hatcher
C-
C----------------------------------------------------------------------
      integer       maxsym, maxxref, minlev, maxlev
      parameter    (maxsym = 5000, maxxref = 15000)
      parameter    (minlev = 2,     maxlev = 20)
      integer       novisit, marktrunc, markleaf, markextrn
      parameter    (novisit = 0, marktrunc = -99, markleaf = -1)
      parameter    (markextrn = -5)

      character*32  sym(maxsym), transfer
      integer       nsym, nref, istart(0:127), istop(0:127), longsym
      integer       ncall(0:maxsym)
      integer       iptrcall(0:maxsym)
      integer       listcall(0:maxxref)
      integer       lvisit(maxsym,3), lstline, menu, levls
      logical       xfor, xlib, xmth, xsys, xots, xstr, 
     &              xsmg, xdtk, xppl
      logical       trim, compress, nohang, tight, forceup
      logical       process, showops, linenum 

      COMMON /CHART2/ nsym, nref, istart, istop, longsym,
     &                ncall, iptrcall, listcall, lvisit,
     &                lstline, menu, levls,
     &                xfor, xlib, xmth, xsys, xots, xstr,
     &                xsmg, xdtk, xppl,
     &                trim, compress, nohang, tight, forceup,
     &                process, showops, linenum
      COMMON /CHART2C/ sym, transfer
