      SUBROUTINE MOTNUM(JREG,JDIP,JOTC,JQUAD)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : return otc number from jreg,jdip
C-                         or jreg,jdip from otc number (if jreg=0)
C-
C-   Input/Output   : jreg = region of otc crate (1-5 for 021-061)
C-                    jdip = otc dip switch number from ktable
C-                    jotc = corresponding otc number
C-                    jquad= otc quadrant number
C-   Controls: 
C-
C-   Created   28-JAN-1994   M. Fortner
C-   Modified   5-JUL-1994   MF add otc -> jreg,jdip
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER J,IOTC
      INTEGER JREG,JDIP,JOTC,JQUAD
      INTEGER OTCNUM(5,0:15),OTCQUA(5,0:15),OTCCRT(200)
C
      DATA (OTCNUM(1,J),J=0,15)/707,700,701,702,703,704,705,706,8*0/
      DATA (OTCNUM(2,J),J=0,15)/730,720,710,716,726,736,
     1                          732,722,712,714,724,734,4*0/
      DATA (OTCNUM(3,J),J=0,15)/760,750,740,746,756,766,
     1                          762,752,742,744,754,764,4*0/
      DATA (OTCNUM(4,J),J=0,15)/770,776,772,774,
     1                          600,602,604,606,
     2                          610,612,614,616,
     3                          660,662,664,666/
      DATA (OTCNUM(5,J),J=0,15)/780,786,782,784,
     1                          630,632,634,636,
     2                          640,642,644,646,
     3                          670,672,674,676/
C
      DATA (OTCQUA(1,J),J=0,15)/7,0,1,2,3,4,5,6,8*0/
      DATA (OTCQUA(2,J),J=0,15)/0,0,0,3,3,3,1,1,1,2,2,2,4*0/
      DATA (OTCQUA(3,J),J=0,15)/0,0,0,3,3,3,1,1,1,2,2,2,4*0/
      DATA (OTCQUA(4,J),J=0,15)/0,3,1,2,12*0/
      DATA (OTCQUA(5,J),J=0,15)/0,3,1,2,12*0/
C
      DATA OTCCRT/
     1        404,0,405,0,406,0,407,3*0,408,0,409,0,410,0,411,13*0,
     2        504,0,505,0,506,0,507,3*0,508,0,509,0,510,0,511,13*0,
     3        412,0,413,0,414,0,415,3*0,512,0,513,0,514,0,515,23*0,
     4        101,102,103,104,105,106,107,100,0,0,
     5        202,0,208,0,209,0,203,3*0,201,0,207,0,210,0,204,3*0,
     6        200,0,206,0,211,0,205,3*0,302,0,308,0,309,0,303,3*0,
     7        301,0,307,0,310,0,304,3*0,300,0,306,0,311,0,305,3*0,
     8        400,0,402,0,403,0,401,3*0,500,0,502,0,503,0,501,13*0/
C
C----------------------------------------------------------------------
C
      IF (JREG.GT.0) THEN
          JOTC = 0
          IF (JREG.GT.5.OR.JDIP.LT.0.OR.JDIP.GT.15) GOTO 999
          JOTC = OTCNUM(JREG,JDIP)
          JQUAD = OTCQUA(JREG,JDIP)
      ELSE IF (JREG.EQ.0) THEN
          IOTC = JOTC - 599
          IF (IOTC.LE.0.OR.IOTC.GT.200) GOTO 999
          JREG = OTCCRT(IOTC)/100
          JDIP = OTCCRT(IOTC) - JREG*100
      ENDIF
C
  999 RETURN
      END
