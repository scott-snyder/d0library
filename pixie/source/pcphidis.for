      SUBROUTINE PCPHIDIS(PHIBIT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Draw selected CAL PHI MODULE in Side View.
C-
C-   Inputs  : PHIBIT(64) - 1 for selected IPHI, 0 for didn't.
C-   Outputs :
C-
C-   Updated  17-JAN-1992   Nobuaki Oshima - change PHI style
C-   Created  22-JUL-1991   Nobuaki Oshima
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
C
      REAL    R1,R2,XBASE,YBASE,XSTA,YSTA
      REAL    XMIN,XMAX,YMIN,YMAX, XSIZ,YSIZ
      REAL    CPHI1,SPHI1,CPHI2,SPHI2,X(4),Y(4)
      REAL    CENPHI,DELPHI,PHI1,PHI2
      REAL    PHIBIT(64)
      REAL    VWSAVE(85),RLEVEL
      INTEGER JPHI,ARGSOK
      LOGICAL FLGVAL
C-
C---
      IF ( FLGVAL('ZOOMING') ) GO TO 999
      IF ( FLGVAL('BLOW_UP') ) GO TO 999
      CALL JVSAVE(VWSAVE)
      CALL JUPVEC(0.,1.,0.)
      CALL JNORML(0.,0.,-1.)
      CALL J4RGET(1, XMIN, XMAX, YMIN, YMAX)
      R1 = (XMAX-XMIN)* .0211
      R2 = (XMAX-XMIN)* .0544
      XSIZ = R1*.5
      YSIZ = XSIZ*1.7
      XBASE = XMIN + (XMAX-XMIN)*0.075
      YBASE = YMIN + (YMAX-YMIN)*0.14
C-
C--- DRAW PHI SEGMENTS
C-
      CALL JIQDIL(RLEVEL)
      CALL JOPEN
      IF(RLEVEL .EQ. -2.) THEN
        CALL PXCOLR('FOR')
      ELSE
        CALL JPINTR(0)
      ENDIF
      CALL JCIRCL(XBASE,YBASE,0.,R1,0)
      CALL JCIRCL(XBASE,YBASE,0.,R2,0)
C-
      DO 100 JPHI=1,NPHIL
        CALL CALPHI(JPHI,1,CENPHI,DELPHI,ARGSOK)
        PHI1=CENPHI-DELPHI/2.
        PHI2=CENPHI+DELPHI/2.
C-
        CPHI1=COS(PHI1)
        SPHI1=SIN(PHI1)
        CPHI2=COS(PHI2)
        SPHI2=SIN(PHI2)
        X(1)=R1*CPHI1 + XBASE
        Y(1)=R1*SPHI1 + YBASE
        X(2)=R1*CPHI2 + XBASE
        Y(2)=R1*SPHI2 + YBASE
        X(3)=R2*CPHI2 + XBASE
        Y(3)=R2*SPHI2 + YBASE
        X(4)=R2*CPHI1 + XBASE
        Y(4)=R2*SPHI1 + YBASE
        IF(PHIBIT(JPHI) .EQ. 1.) THEN
          IF(RLEVEL .EQ. -2.) THEN
            CALL PXCOLR('RED')
          ELSE
            CALL PXCOLFILL('RED')
          ENDIF
          CALL JPOLGN(X,Y,4)
        ENDIF
  100 CONTINUE
C-
      XSTA = XBASE
      YSTA = YBASE
      CALL PXCOLR('GRE')
      CALL JJUST(2,2)
      CALL JSIZE(XSIZ,YSIZ)
      CALL JFONT(5)
      CALL J3MOVE(XSTA, YSTA, 0.)
      CALL JHSTRG('PHI')
C-
      CALL JCLOSE
      CALL JVLOAD(VWSAVE)
  999 RETURN
      END
