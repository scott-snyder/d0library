      SUBROUTINE MUZBN2(QQUAD,XI,YI,ZI,XO,YO,ZO,
     A  DIX,DIY,DIZ,DOX,DOY,DOZ,XYZIN,XYZOUT,XYZBEND)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CC  determines entrance/exit to iron plus center of bend
CC     INPUT: QQUAD  0=basement, 1-4=central, 5-12=ends
CC            XI,YI,ZI position at A-layer
CC            X,Y,Z  POSITION MAGNET CENTER (FROM B_C LAYER)
CC            DIX-DOZ, direction cosines inside and outside
CC     OUTPUT: ZIN,ZOUT inside outside entrance to iron
CC             ZBEND average of BDL
CC
CC      D. Hedin March 11, 1993
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      IMPLICIT NONE
      INTEGER QUAD,I,QQUAD,NMED
      REAL XO,YO,ZO,DIX,DIY,DIZ,DOX,DOY,DOZ,BX,BY,BZ
      REAL XI,YI,ZI,BX1,BY1,BZ1,XYZ(3),BC(21),BIN,BB(3),
     A  XX,YY,ZZ,VECT(3),XYZIN(3),XYZOUT(3),XYZBEND(3)
C
      QUAD=MOD(QQUAD,100)
C  FIND ENTRANCE POINT TO MAGNET. DIRECTION COSINES ALWAYS POINT OUT
C    START WITH 25 CM STEPS
      DO I=1,20
        XYZ(1)=XI+25.*I*DIX
        XYZ(2)=YI+25.*I*DIY
        XYZ(3)=ZI+25.*I*DIZ
        CALL MAGLOC(XYZ,NMED,VECT)
        IF(NMED.GT.0) GO TO 10
      ENDDO
10    CONTINUE     ! DO 5 CM STEPS IN REVERSE DIRECTION
      DO I=1,20
        XYZ(1)=XYZ(1)-5.*DIX
        XYZ(2)=XYZ(2)-5.*DIY
        XYZ(3)=XYZ(3)-5.*DIZ
        CALL MAGLOC(XYZ,NMED,VECT)
        IF(NMED.EQ.0) GO TO 20
      ENDDO
20    CONTINUE     ! DO 1 CM STEPS IN FORWARD DIRECTION
      DO I=1,20
        XYZ(1)=XYZ(1)+1.*DIX
        XYZ(2)=XYZ(2)+1.*DIY
        XYZ(3)=XYZ(3)+1.*DIZ
        CALL MAGLOC(XYZ,NMED,VECT)
        IF(NMED.GT.0) GO TO 30
      ENDDO
30    CONTINUE     ! DO 2 MM STEPS IN REVERSE DIRECTION
      DO I=1,20
        XYZ(1)=XYZ(1)-.2*DIX
        XYZ(2)=XYZ(2)-.2*DIY
        XYZ(3)=XYZ(3)-.2*DIZ
        CALL MAGLOC(XYZ,NMED,VECT)
        IF(NMED.EQ.0) GO TO 40
      ENDDO
40    XYZIN(1)=XYZ(1)
      XYZIN(2)=XYZ(2)
      XYZIN(3)=XYZ(3)
C  FIND EXIT     POINT TO MAGNET. DIRECTION COSINES ALWAYS POINT OUT
C    START WITH 25 CM STEPS
      DO I=1,20
        XYZ(1)=XO+25.*I*DOX
        XYZ(2)=YO+25.*I*DOY
        XYZ(3)=ZO+25.*I*DOZ
        CALL MAGLOC(XYZ,NMED,VECT)
        IF(NMED.EQ.0) GO TO 60
      ENDDO
60    CONTINUE     ! DO 5 CM STEPS IN REVERSE DIRECTION
      DO I=1,50
        XYZ(1)=XYZ(1)-5.*DOX
        XYZ(2)=XYZ(2)-5.*DOY
        XYZ(3)=XYZ(3)-5.*DOZ
        CALL MAGLOC(XYZ,NMED,VECT)
        IF(NMED.GT.0) GO TO 70
      ENDDO
70    CONTINUE     ! DO 1 CM STEPS IN FORWARD DIRECTION
      DO I=1,20
        XYZ(1)=XYZ(1)+1.*DOX
        XYZ(2)=XYZ(2)+1.*DOY
        XYZ(3)=XYZ(3)+1.*DOZ
        CALL MAGLOC(XYZ,NMED,VECT)
        IF(NMED.EQ.0) GO TO 80
      ENDDO
80    CONTINUE     ! DO 2 MM STEPS IN REVERSE DIRECTION
      DO I=1,20
        XYZ(1)=XYZ(1)-.2*DOX
        XYZ(2)=XYZ(2)-.2*DOY
        XYZ(3)=XYZ(3)-.2*DOZ
        CALL MAGLOC(XYZ,NMED,VECT)
        IF(NMED.GT.0) GO TO 90
      ENDDO
90    XYZOUT(1)=XYZ(1)
      XYZOUT(2)=XYZ(2)
      XYZOUT(3)=XYZ(3)
CCC   find center of bend by integrating between the entrance
CCC   and exit point. use straight line
      BX=0.
      DO I=1,21
        BIN=0.5+(I-1)
        VECT(1)=XYZIN(1)+(XYZOUT(1)-XYZIN(1))/21.*BIN
        VECT(2)=XYZIN(2)+(XYZOUT(2)-XYZIN(2))/21.*BIN
        VECT(3)=XYZIN(3)+(XYZOUT(3)-XYZIN(3))/21.*BIN
        CALL GTMFLD(QUAD,VECT,BB)
        BZ=SQRT(BB(1)**2+BB(2)**2)   ! SKIP Z COMPONENT
        BX=BX+BZ
        BC(I)=BZ
      ENDDO
      BY=0.
      DO I=1,21
        IF(BY+BC(I).GT.BX/2.) THEN
          BIN=(I-1)+(BX/2.-BY)/BC(I)
          GO TO 77
        ENDIF
        BY=BY+BC(I)
      ENDDO
77    CONTINUE
      XYZBEND(1)=XYZIN(1)+(XYZOUT(1)-XYZIN(1))/21.*BIN
      XYZBEND(2)=XYZIN(2)+(XYZOUT(2)-XYZIN(2))/21.*BIN
      XYZBEND(3)=XYZIN(3)+(XYZOUT(3)-XYZIN(3))/21.*BIN
      RETURN
      END
