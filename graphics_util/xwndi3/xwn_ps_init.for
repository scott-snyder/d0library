      SUBROUTINE PS_INIT
C  OPEN POSTSCRIPT FILE AND
C  PRODUCE ALL THE HEADER INFORMATION FOR B&W OR COLOR POSTSCRIPT
      INCLUDE 'D0$GRAPHICS_UTIL$XWNDI3:XWNEMU.INC'
      IPP=1
      WRITE(IDRUNI,10)
   10 FORMAT(
     &'%!PS-Adobe-2.0 ',/,
     &' initgraphics ',/,
     &' /#copies 1 def',/,
     &' /bop {erasepage 612 0 translate 90 rotate',/,
     &' 0.24 0.24 scale 93 78 translate 2 setlinewidth',
     &' 0 setlinecap 2 setlinejoin} def',/,
     &' /f {fill} def /ff {findfont} def /gr {grestore} def',/,
     &' /gs {gsave} def /l {lineto} def /lw {setlinewidth} def',/,
     &' /mf {makefont} def /m {moveto} def /n {newpath} def',/,
     &' /sf {setfont} def /rgb {setrgbcolor} def /g {setgray} def',/,
     &' /sd {setdash} def',/,
     &' /sh {show} def /p {showpage} def /s {stroke} def',/
     &' bop',/)
      RETURN
C
      ENTRY PS_PAGE
      ENTRY PS_FIN
      CALL PS_FORCE
      WRITE(IDRUNI,100)
  100 FORMAT(' p bop',/)
      RETURN
      END
