      SUBROUTINE HMTX_INFO(LPELC,CTAG)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Get a HMTE(H matrix) Bank information.
C-
C-   Inputs  : LPELC - Address of PELC Bank.
C-             CTAG  - No use so far.
C-
C-   Created  24-MAR-1993   Nobuaki Oshima
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      REAL XMAX,XMIN,YMAX,YMIN
      REAL YTOP,YBOT,X0,Y0
      INTEGER LPELC,LHMTX
      REAL CSQTR,CSQR  
      REAL STEPX,STEPY,XSIZ,YSIZ,YPT
      CHARACTER*15 STRING
      CHARACTER*1 CTAG
      INCLUDE 'D0$INC:ZEBCOM.INC'
C--------------------------------------------------------------------------
      CALL J4RGET(1,XMIN,XMAX,YMIN,YMAX)
C
      CALL JOPEN
      XSIZ = (XMAX-XMIN)*0.0575
      YSIZ = (XMAX-XMIN)*0.0215
      YTOP = YMAX - YMAX*0.55-YSIZ*12.5
      YPT = YTOP - (YMAX - YMIN)*0.04
      CALL JMOVE(XMIN,YTOP)
      CALL JDRAW(XMAX,YTOP)
      LHMTX = LQ(LPELC-1)
      CSQR = Q(LHMTX+5)
      CSQTR = Q(LHMTX+7)
      CALL JFONT(5)
      CALL JSIZE(XSIZ,YSIZ)
      CALL JJUST(1,1)
      X0 = XMIN+(XMAX-XMIN)*.035
      Y0 = YTOP-YSIZ*1.5
      CALL JMOVE(X0,Y0)
      CALL J1STRG('H-MATRIX INFO :')
      YPT = YPT - YSIZ*2
      Y0  = YPT
      CALL JMOVE(X0,Y0)

      IF(CSQR .GT. 9999) THEN
        STRING = 'Csqr = *******'
      ELSE
        WRITE(STRING,101)CSQR
      ENDIF
      CALL J1STRG(STRING)
      YPT = YPT - YSIZ*2
      Y0 = YPT
      CALL JMOVE(X0,Y0)
      IF (CSQTR .GT. 9999) THEN
        STRING = 'Csqtr = *******'
      ELSE
        WRITE(STRING,102)CSQTR
      ENDIF
      CALL J1STRG(STRING)
      YPT = YPT - YSIZ*2
      Y0 = YPT
  101 FORMAT('Csqr  = ',f7.2)
  102 FORMAT('Csqtr = ',f7.2)
      CALL JCLOSE
C
  999 RETURN
      END
