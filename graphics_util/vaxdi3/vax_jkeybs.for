      SUBROUTINE JKEYBS(DSPDEV,PHYDEV,ECHOLV,MAXCHR,STRING,ACTUAL)
C  INPUT A CHARACTER STRING FROM THE KEYBOARD
      INTEGER DSPDEV,PHYDEV,ECHOLV,MAXCHR,ACTUAL
      INCLUDE 'D0$INC:DI3INC.INC'
      CHARACTER*1 STRING(*),ICH
C  PROMPT
      XTEM=XPOSN
      YTEM=YPOSN
      CALL J3STRG('>')
      NCHR=0
C  WAIT AND LOOP.  J_TTYIN FALSE WHEN A NON-ALPHABETIC CHARACTER TYPED
   10 IF(.NOT.(J_TTYIN(ICH,ICTRL))) GO TO 20
      IF(ECHOLV.EQ.0) GO TO 15
      XTEMP=XTEM
      YTEMP=YTEM
      CALL JMOVE(XTEMP+1.2*XSIZE*FLOAT(NCHR+1),YTEMP)
      CALL J3STRG(ICH)
   15 NCHR=NCHR+1
      STRING(NCHR)=ICH
      IF(NCHR.EQ.256) GO TO 20
      IF(NCHR.LT.MAXCHR) GO TO 10
      ACTUAL=NCHR
      RETURN
   20 IF(ICTRL.EQ.127) THEN
          NCHR=NCHR-1
          GO TO 10
        ELSE
          ACTUAL=NCHR
          RETURN
      ENDIF
      END
