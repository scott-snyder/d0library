      SUBROUTINE GISAMU(NPART,ID,ID2,PX,PY,PZ,P,PHI,THETA,ETA,X,Y,Z)
C------------------------------------------------------------------------
C-                                                                      -
C     RETURNS PARTICLE INFO FROM BANKS ISP1 AND ISP2 FOR MUONS
C-                                                                      -
C-    OUTPUT: NPART = number of MUONS
C-    ID = isajet ID
C     ID2  = 10=DECAY    >=1 Prompt (with ID)
C      g  u  d  s  c  b  t W Z
C      9  1  2  3  4  5  6 7 8
C-    PX,PY,PZ,P   momentums of ith particle                            -
C-    PHI,THETA,ETA mass and angles of ith particle
C          DH   8-90                              -
C-                                                                      -
C------------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$LINKS:IZISP1.LINK'
      INCLUDE 'D0$LINKS:IZISP2.LINK'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER SI,KPARENT,IDD,DECISA
      PARAMETER (SI=100)
      INTEGER NPART,ID(SI),ID2(SI),IDA
      REAL PA(4),PHIA,THA,ETAA,X(SI),Y(SI),Z(SI),XA,YA,ZA
      REAL PX(SI),PY(SI),PZ(SI),P(SI),PHI(SI),THETA(SI),ETA(SI)
C
      INTEGER LISAE,LISAL,LSUP,LISV1,LISP1,LSUP2
      NPART=0
CC   LOOP OVER PROMPT PARTICLES
      LSUP=0
    1 CALL GTISV1(LSUP,LISV1,IDA,PA,XA,YA,ZA)
      IF(LISV1.NE.0) THEN
        LSUP=LISV1
        LSUP2=LSUP-IZISP1
    2   CALL GTISP1(LSUP2,LISP1,IDA,PA,PHIA,THA,ETAA)
        IF(LISP1.NE.0) THEN
          LSUP2=LISP1
          IF(IABS(IDA).EQ.14.AND.PA(4).GT..3) THEN
            IF(NPART.LT.SI) THEN
              NPART=NPART+1
              ID(NPART)=  IDA     ! particle id
              ID2(NPART)=100                ! PROMPT MUON
              IDD=IQ(LISV1+1)
              KPARENT=DECISA(IDD)
              IF(KPARENT.NE.0) ID2(NPART)=KPARENT
              PX(NPART)=   PA(1)
              PY(NPART)=   PA(2)
              PZ(NPART)=   PA(3)
              P(NPART)=    PA(4)
              PHI(NPART)=  PHIA
              THETA(NPART)=THA
              ETA(NPART)=  ETAA
              X(NPART) = XA
              Y(NPART) = YA
              Z(NPART) = ZA
            ENDIF
          ENDIF
          GO TO 2
        ENDIF
        GO TO 1
      ENDIF
CCCC  LOOP OVER GEANT PARTICLES
      LSUP=0
    3 CALL GTISV2(LSUP,LISV1,IDA,PA,XA,YA,ZA)
      IF(LISV1.NE.0) THEN
        LSUP=LISV1
        LSUP2=LSUP-IZISP2
    4   CALL GTISP2(LSUP2,LISP1,IDA,PA,PHIA,THA,ETAA)
        IF(LISP1.NE.0) THEN
          LSUP2=LISP1
          IF(IABS(IDA).EQ.14.AND.PA(4).GT.1.) THEN
            IF(NPART.LT.SI) THEN
              NPART=NPART+1
              ID(NPART)=  IDA     ! particle id
              IF(SQRT(XA**2+YA**2).GT.75..OR.ABS(ZA).GT.150.) THEN
                ID2(NPART)=-1                ! 'PUNCH'
              ELSE
                ID2(NPART)=10               ! 'DECAY'
              ENDIF
              PX(NPART)=   PA(1)
              PY(NPART)=   PA(2)
              PZ(NPART)=   PA(3)
              P(NPART)=    PA(4)
              PHI(NPART)=  PHIA
              THETA(NPART)=THA
              ETA(NPART)=  ETAA
              X(NPART) = XA
              Y(NPART) = YA
              Z(NPART) = ZA
            ENDIF
          ENDIF
          GO TO 4
        ENDIF
        GO TO 3
      ENDIF
      RETURN
      END
