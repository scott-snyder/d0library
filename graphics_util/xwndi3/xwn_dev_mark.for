      SUBROUTINE DEV_MARK(X,Y,Z)
C  Draw stroke marks  (.,+,*,O,X)
      INCLUDE 'D0$GRAPHICS_UTIL$XWNDI3:XWNEMU.INC'
      DATA IMARK/2/
      SAVE
C
      IMRK=MOD(IMARK-1,5)+1
      IF(IMRK.LT.1) IMRK=1
      IF(IMRK.GT.5) IMRK=5
      XPOSN=X
      YPOSN=Y
      ZPOSN=Z
      CALL DEV_TRANSF(XPOSN,YPOSN,ZPOSN,XPR,YPR)
      DEL=XSIZEH/2.
      IF(IMRK.EQ.1) DEL=DEL/3.
      CALL DEV_TRANSF(0.,0.,0.,CENTR,DUM)
      CALL DEV_TRANSF(DEL,0.,0.,DELPR,DUM)
      DELPR=DELPR-CENTR
      XLO=XPR-DELPR
      XHI=XPR+DELPR
      YLO=YPR-DELPR
      YHI=YPR+DELPR
      GO TO (100,200,200,100,300) IMRK
  100 CALL DEV_MOVSCR(XLO,YLO)
      CALL DEV_DRASCR(XLO,YHI)
      CALL DEV_DRASCR(XHI,YHI)
      CALL DEV_DRASCR(XHI,YLO)
      CALL DEV_DRASCR(XLO,YLO)
      RETURN
  200 CALL DEV_MOVSCR(XPR,YLO)
      CALL DEV_DRASCR(XPR,YHI)
      CALL DEV_MOVSCR(XLO,YPR)
      CALL DEV_DRASCR(XHI,YPR)
      IF(IMRK.EQ.3) GO TO 300
      RETURN
  300 CALL DEV_MOVSCR(XLO,YLO)
      CALL DEV_DRASCR(XHI,YHI)
      CALL DEV_MOVSCR(XLO,YHI)
      CALL DEV_DRASCR(XHI,YLO)
      RETURN
C
      ENTRY DEV_SET_MARK(JMARK)
      IMARK=JMARK
      END
