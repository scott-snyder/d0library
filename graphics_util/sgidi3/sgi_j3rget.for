C DEC/CMS REPLACEMENT HISTORY, Element J3RGET.FOR
C *1     3-JUN-1992 13:04:43 LUPE "Release of DI3GL after split"
C DEC/CMS REPLACEMENT HISTORY, Element J3RGET.FOR
      SUBROUTINE J3RGET(ICOD,V1,V2,V3)
      INCLUDE 'D0$GRAPHICS_UTIL$SGIDI3:DI3GL.INC'
      V1=RJ3(1,ICOD)
      V2=RJ3(2,ICOD)
      V3=RJ3(3,ICOD)
      IF(IDEBUG.GT.0) TYPE *,' J3RGET-ICOD,V1,V2,V3:',
     &                                ICOD,V1,V2,V3
      END
