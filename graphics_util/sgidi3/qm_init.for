      SUBROUTINE QM_INIT
C  OPEN QMS FILE AND
C  PRODUCE ALL THE HEADER INFORMATION FOR B&W OR COLOR QMS
      INCLUDE 'D0$GRAPHICS_UTIL$SGIDI3:DI3GL.INC'
      IPP=1
      WRITE(IDRUNI,10)
   10 FORMAT('^IGV^PW01')
      RETURN
C
      ENTRY QM_FIN
      WRITE(IDRUNI,100)
  100 FORMAT('IGE^O^-')
      RETURN
      END