      SUBROUTINE JCOTBL(DSPDEV,COUNT,INDEXS,HUES,SATS,LIGHTS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Download portions of a color table on a specific
C-   device.
C-
C-   Inputs  : DSPDEV   [I]:  Display device
C-             COUNT    [I]:  Number of elements of the device level color table
C-                            that are to be defined.  
C-                            COUNT must be 1 <= COUNT <= 32767
C-             INDEXS [I(*)]: The array of color table indices to be defined.
C-                            Each entry in the INDEXS array must be in the
C-                            range 0 <= COUNT(i) <= 32767
C-             HUES   [I(*)]: The array of color hues for each table index.  Hue
C-                            is the color attribute which makes it possible to
C-                            classify color as red, green, blue.
C-             SATS   [I(*)]: The array of color saturations for each color 
C-                            table index.  The range: 0<= SATS(i) <= 32767
C-             LIGHTS [I(*)]: The array of lightness attributes for each color
C-                            table index.  Range 0 <= LIGHTS(i) <= 32767.
C-
C-   Outputs : None
C-
C----------------------------------------------------------------------

      INTEGER DSPDEV,COUNT
      INTEGER INDEXS(*),HUES(*),SATS(*),LIGHTS(*)
      INTEGER ICNDXX
      INCLUDE 'D0$GRAPHICS_UTIL$XWNDI3:XWNEMU.INC'
C------------------------------------------------------------------------------
CC      WRITE(6,*)' XWN_JCOTBL DUMY CALLED !'
CC      CONV=3.1415927/180.
CCC
CC      COUNT = 9
CC      DO 10 J=1,COUNT
CC       ICINDX=INDEXS(J)
CC       IF(ICINDX.LT.0.OR.ICINDX.GT.50) GO TO 10
CC       HANG=FLOAT(HUES(J))-120.
CC       IF(HANG.LT.0.) HANG=HANG+360.
CC       SAT=FLOAT(SATS(J))/32767.
CC       IF(SAT.LE.0.) SAT=.01
CC       ALIGHT=FLOAT(LIGHTS(J))/32767.
CC       IF(ALIGHT.LE.0.) ALIGHT=.01
CC       write(2,15)J,HANG,J,ALIGHT,J,SAT,J,RV,J,GV,J,BV
CC   15  FORMAT(' HANG(',I2,')=',F10.5,
CC     &      ' ALIGHT(',I2,')=',F10.5,
CC     &         ' SAT(',I2,')=',F10.5,
CC     &          ' RV(',I2,')=',F10.5,
CC     &          ' GV(',I2,')=',F10.5,
CC     &          ' BV(',I2,')=',F10.5)
CC       CALL J_HLS_TO_RGB(HANG,ALIGHT,SAT,RV,GV,BV)
CC       RVEC(ICINDX)=RV*255.
CC       BVEC(ICINDX)=BV*255.
CC       GVEC(ICINDX)=GV*255.
CC       ICNDXX=ICINDX+1
CC   10 ENDDO
CC      WRITE(2,*)
CC      DO  J =  1, COUNT
CC        WRITE(2,20)J,RVEC(J),J,BVEC(J),J,GVEC(J)
CC   20   FORMAT(' RVEC(',I2,')=',F10.5,
CC     &         ' BVEC(',I2,')=',F10.5,
CC     &         ' GVEC(',I2,')=',F10.5)
CC      ENDDO
      END
