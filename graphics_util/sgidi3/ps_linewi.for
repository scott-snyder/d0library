      SUBROUTINE PS_LINEWI
C  Change the line style
      INCLUDE 'D0$GRAPHICS_UTIL$SGIDI3:DI3GL.INC'
      SAVE
      IF(IDLWID.EQ.IDLOLD) RETURN
      CALL PS_FORCE
      IDD=IDLWID
      IF(IDD.LE.1) IDD=1
      IF(IDD.GT.9999) IDD=1
      WRITE(IDRUNI,10) IDD
   10 FORMAT(' ',I5,' lw')
      IDLOLD=IDLWID
      END
