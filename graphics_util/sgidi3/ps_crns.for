      SUBROUTINE PS_CRNS(P1,P2,P3,P4)
      INCLUDE 'D0$GRAPHICS_UTIL$SGIDI3:DI3GL.INC'
      P1=300.
      P2=200.
      P3=2800.
      P4=2300.
C!!! KLUDGE TO MATCH THE ASPECT RATIO FOR NOW
      P3=P4/RASP
      END
