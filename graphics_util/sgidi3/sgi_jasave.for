C DEC/CMS REPLACEMENT HISTORY, Element JASAVE.FOR
C *1     3-JUN-1992 13:11:39 LUPE "Release of DI3GL after split"
C DEC/CMS REPLACEMENT HISTORY, Element JASAVE.FOR
      SUBROUTINE JASAVE(IARRAY)
      INCLUDE 'D0$GRAPHICS_UTIL$SGIDI3:DI3GL.INC'
      INTEGER IARRAY(23)
      DO I=1,10
        IARRAY(I)=IJ1(I+4)
      ENDDO
      DO I=1,10
        IARRAY(I)=IJ1(I+4)
      ENDDO
      DO I=1,10
        IARRAY(I+10)=IJ1(I+15)
      ENDDO
      DO I=1,3
        IARRAY(I+20)=IJ1(I+31)
      ENDDO
      RETURN
      END