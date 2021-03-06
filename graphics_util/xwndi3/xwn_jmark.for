      SUBROUTINE JMARK(X,Y)
      INCLUDE 'D0$GRAPHICS_UTIL$XWNDI3:XWNEMU.INC'
      CHARACTER*1 MARK(5)
      DATA MARK/'.','+','*','O','X'/
      IMRK=MOD(IMARK-1,5)+1
      IF(IMRK.LT.1) IMRK=1
      IF(IMRK.GT.5) IMRK=5
      XPOSN=X
      YPOSN=Y
      IF(HCPY) THEN
        CALL DEV_MARK(X,Y,0.)
        RETURN
      ENDIF
C  SEE IF STROKE FONTS ONLY
      IF(STFONT) THEN
        CALL J_TR2XY(XPOSN,YPOSN,XPR,YPR)
        DEL=XSIZE/2.
        IF(IMRK.EQ.1) DEL=DEL/3.
        CALL J_TR2XY(0.,0.,CENTR,YDUMMY)
        CALL J_TR2XY(DEL,0.,DELPR,YDUMMY)
        DELPR=DELPR-CENTR
        XLO=XPR-DELPR
        XHI=XPR+DELPR
        YLO=YPR-DELPR
        YHI=YPR+DELPR
        IATB=IFONT
        GO TO (100,200,200,100,300) IMRK
  100   CALL J_LINE2(XLO,YLO,XHI,YLO)
        CALL J_LINE2(XLO,YLO,XLO,YHI)
        CALL J_LINE2(XHI,YLO,XHI,YHI)
        CALL J_LINE2(XLO,YHI,XHI,YHI)
        RETURN
  200   CALL J_LINE2(XPR,YLO,XPR,YHI)
        CALL J_LINE2(XLO,YPR,XHI,YPR)
        IF(IMRK.EQ.3) GO TO 300
        RETURN
  300   CALL J_LINE2(XLO,YLO,XHI,YHI)
        CALL J_LINE2(XLO,YHI,XHI,YLO)
        RETURN
      ELSE
C  THE CHARACTER CELL STARTS AT THE UPPER LEFT (UIS)
        CALL J_TR2XY(XPOSN-.5*XSIZE,YPOSN+.5*YSIZE,CRTX,CRTY)
C  NORMAL CHARACTERS ARE 78 CHARS BY 34 LINES.
C  ASSUME NORMAL PIXEL DIMENSIONS ARE 13 IN X BY 23 IN Y.
        CALL J_TEXT(MARK(IMRK),CRTX,CRTY)
      ENDIF
      IF(.NOT.PUTS)RETURN
      IF(PUTSUP)RETURN
      CALL J_PUTSG(I2MARK,X)
      CALL J_PUTSG(-I2MARK,Y)
      END
