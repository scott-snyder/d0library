      SUBROUTINE TRTABL
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-  COMPUTE THE TABLES FOR X RAY YIELD IN NSTREF BINS IN THETA
C-  AND NGAREF IN GAMMA (=E PART./M PART.).
C-  FILL THE ARRAY FOR ENERGY SPECTRUM PROBX(I,J,K) AND THE ABSOLUTE
C-  YIELD XNUMB(J,K) WITH I= ENERGY BIN,J= BIN IN THETA,K=BIN IN GAMMA
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created                A.ZYLBERSTEJN
C-   Updated  19-SEP-1988   J.R. HUBBARD/A. ZYLBERSTEJN
C-   Updated   1-MAR-1990   A. Zylberstejn  Put back the possibility to
C-                                         generate Transition X rays according
C-                                         to Garibian with variable gaps
C-                                         This existed at one time but
C-                                         disappeared for unknown reasons
C-
C----------------------------------------------------------------------
C
C
      IMPLICIT NONE
      INCLUDE 'D0$INC:GCUNIT.INC/LIST'
      INCLUDE 'D0$INC:TABX.INC/LIST'
      INCLUDE 'D0$INC:GCONST.INC/LIST'
      INCLUDE 'D0$INC:WORKSP.INC/LIST'
      INCLUDE 'D0$INC:XRAY.INC/LIST'
      INCLUDE 'D0$INC:XSPECT.INC/LIST'
      INCLUDE 'D0$INC:D0LOG.INC/LIST'
      INCLUDE 'D0$INC:raddsc.INC'
C
      INTEGER I,IG,IB
      REAL    GAMMA,DTET,STETA,TET,GASTEP,ETOT,THET0
C
      THET0=40.
      DXGAP=.1*STRD(6)
      IF(DXGAP.NE.0.)DXFOIL=.1
      DTET=(90.-THET0)/FLOAT(NSTREF-1)
      IF (PTRD.GT.1) THEN
        WRITE(LOUT,*)'ENTER TRTABL WITH NSTREF=',NSTREF,' DTET',DTET
      ENDIF
C
C  DEFINE REFERENCE SIN(THETA)
      DO 81 I =1  ,  NSTREF
        TET=THET0+(I-1)*DTET
        TET=TET*DEGRAD
        STETA=SIN(TET)
        STREF(I)=STETA
   81 CONTINUE
C  DEFINE REFERENCE GAMMA(=E/M)
      GASTEP=GSTEP
      GAMMA=1000.-GSTEP
      DO 91 IG =  1,  NGAREF
        IF(GAMMA.GE.4000.)GASTEP=2000.
        GAMMA=GAMMA+GASTEP
        GAMREF(IG)=GAMMA
   91 CONTINUE
      DO 100 I=1,NSTREF
        STETA=STREF(I)
        DO 90  IG=1,NGAREF
          GAMMA=GAMREF(IG)
          CALL TRDYLD(STETA,GAMMA)!TRD YIELD FOR THAT PARTICULAR STETA AND G
          XNUMB(I,IG)=XNOBT
          ETOT=0.
          DO 80 IB=1,NSTEP
            ETOT=ETOT+XFONC(IB)*(EDOWN+(FLOAT(IB)-.5)*XSTEP)
            PROBX(IB,I,IG)=XFONC(IB)
            IF(IB.EQ.1)THEN
              WS(IB)=XFONC(IB)
            ELSE
              WS(IB)=WS(IB-1)+XFONC(IB)
            ENDIF
   80     CONTINUE
          IF (PTRD.GE.1) THEN
            TET=ASIN(STETA)*RADDEG
            WRITE(LOUT,7785)TET,GAMMA,XNUMB(I,IG),ETOT
          ENDIF
   90   CONTINUE
  100 CONTINUE
      RETURN
 7742 FORMAT(' BINNING ',F10.4,I4,F10.4)
 7763 FORMAT(10F7.4)
 7785 FORMAT(' THETA',G10.4,' GAMMA ',F7.0,' # OF XRAYS',G10.4,
     +' ETOT',G10.4)
      END
