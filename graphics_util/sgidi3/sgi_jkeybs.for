C DEC/CMS REPLACEMENT HISTORY, Element JKEYBS.FOR
C *1     3-JUN-1992 14:03:30 LUPE "Release of DI3GL after split"
C DEC/CMS REPLACEMENT HISTORY, Element JKEYBS.FOR
      SUBROUTINE JKEYBS(DSPDEV,PHYDEV,ECHOLV,MAXCHR,STRING,ACTUAL)
C  INPUT A CHARACTER STRING FROM THE KEYBOARD
      INTEGER DSPDEV,PHYDEV,ECHOLV,MAXCHR,ACTUAL
      INCLUDE 'fgl.h'
      INCLUDE 'fdevice.h'
      INCLUDE 'D0$GRAPHICS_UTIL$SGIDI3:DI3GL.INC'
      INTEGER*2 QVAL
      CHARACTER*1 STRING(*),ICH,ICHOLD
      INTEGER*2 ICX,ICY,ICXIN,ICYIN
C
      CALL GETORI(IX1,IY1)
      CALL GETCPO(ICXIN,ICYIN)
C  PROMPT
      CALL CHARST('>',1)
      NCHR=0

C  WAIT AND LOOP.
   10 CONTINUE
      IDEV=QREAD(QVAL)
      IF(IDEV.NE.KEYBD) GO TO 10
      IF(QVAL.GE.32.AND.QVAL.LT.127) THEN
        ICH=CHAR(QVAL)
        IF(ECHOLV.EQ.0) GO TO 15
        XTEMP=XTEM
        YTEMP=YTEM
        CALL CHARST(ICH,1)
        ICHOLD=ICH
C        CALL JMOVE(XTEMP+1.2*XSIZE*FLOAT(NCHR+1),YTEMP)
C        CALL J3STRG(ICH)
   15   NCHR=NCHR+1
        STRING(NCHR)=ICH
        IF(NCHR.EQ.256) THEN
          NCHR=NCHR-1
          GO TO 10
        ENDIF
        IF(NCHR.LT.MAXCHR) GO TO 10
        ACTUAL=NCHR
      ELSEIF(QVAL.EQ.127.OR.QVAL.EQ.8) THEN    ! DEL OR BACKSPACE
        NCHR=NCHR-1
        WID=FLOAT(STRWID(ICHOLD,1))
        CALL GETCPO(ICX,ICY)
        CX4=FLOAT(ICX-IX1)-WID
        CY4=FLOAT(ICY-IY1)
        CALL CMOV2(CX4,CY4)
        ICL=GETCOL()
        CALL COLOR(IBCKCL)
        CALL CHARST(ICHOLD,1)
        CALL COLOR(ICL)
        CALL CMOV2(CX4,CY4)
        GO TO 10
      ELSEIF(QVAL.EQ.13.OR.QVAL.EQ.10) THEN    ! END ON CR OR LF
        ACTUAL=NCHR
        RETURN
      ENDIF
      GO TO 10
      END