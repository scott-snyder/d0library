      SUBROUTINE MUFTBX(NN,FLAG,ZBND,XV,ZV,ADD,YT,XT,XD,TC,XM,
     &  ZM,SLO,XI,ZI,SLI,CI,CO,C12,IOUT,IVTX)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : fit in bend view in order to determine
C      the left/right drift combinations. The plan is to first
C      fit the outside hits to a straight line. Next to use a
C      pseudo point in the center of the magnet (with a 'large'
C      weight) to aid in determining the L/R solutions in the A-layer.
C-   SAME AS MUFTBD BUT HANDLES MIXED END-CENTRAL CORNERS
C
C-   Inputs  : NN = number of points on track
C              FLAG = =0 end (L/R in X) =1 central (L/R in Z)
C              ZBND = magnet bend point
C              XV   = vertex point (bend view)
C              ZV   = vertex point (between planes)
C              ADD = wire address for nth hit
C              YT = location between planes for nth hit
C              XT = wire position
C              XD = drift distance solution 1,2
C              TC  = cut if point > this donot use SEE MUFTSL
C-   Outputs : XM,ZM,SLO,C0 = x,z, slope and sqrt(chi**2) outside
C              magnet (B-C layer)--XM,ZM at magnet center
C              XI,ZI,SLI inside (A-layer) at minimu Z
C              C12 is combined quality of fit
C             IOUT = solution for nth hit (-2,-1,0,1,2)
C-
C-   Created   March 3, 1993   David Hedin
C     DH 4/94 change vertex weight
C     DH 7/94 ALLOW FOR MISSING DRIFT TIMES
C     DH 1/95 init N correctly
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER N,I,J,NN,NFIT,NMAX,IVER,N1,IVTX
      PARAMETER (NFIT=12)
      PARAMETER (NMAX=40)
      REAL YT(NMAX),XT(NMAX),XD(2,NMAX),TC,X1,Z1,SLI,SLO,C1,ZBND
      REAL XM,ZM,CI,CO,X2,Z2,SL2,SL1,C2,WTMG,XI,ZI,ZMIN,TVER(3)
      REAL WT(NFIT),Y(12,NFIT),X(NFIT),XX(NFIT),D(12,NFIT),RATIO
      REAL X3(NFIT),XG,ZG,SLG,CHIG,WTMG2,SLIG,SLOG,XIG,XMG,Y3(NFIT)
      REAL WTVTX,X0A,SLA,ZA,CHIMX,CHI,VXG,VSL,XV,ZV,C12,XP,XH,CMAX
      INTEGER JJ(NFIT),IOUT(NMAX),FLAG(NMAX),IS(NFIT),JS(NFIT),FIRST
      INTEGER ISF(NFIT,12,2),ADD(NMAX),F(NMAX),NJ,MOD,PLN,WIR,ERR,K,M,P
      INTEGER NV,IPASS,IMAX,JVTX,CHOICE,FF(NMAX),M1,M2
      DATA CHOICE,WTMG,WTVTX,WTMG2,FIRST/0,5.0,5.0,1.,0/ 
      IF(FIRST.EQ.0) THEN
        FIRST=1
        CALL VERXYZ(IVER,TVER,NV)
      ENDIF
C
C
      IVTX=0
      IPASS=1
      N=NN
      DO I=1,N
        FF(I)=0
      ENDDO
1000  CONTINUE
      M1=0
      M2=0
      DO I=1,N
        IOUT(I)=0
        IF(FF(I).EQ.0.AND.FLAG(I).EQ.0) M1=M1+1
        IF(FF(I).EQ.0.AND.FLAG(I).NE.0) M2=M2+1
      ENDDO
CCC DONOT USE POINTS IF ONLY 2 IN GIVEN ORIENTATION
      IF(M1.LE.2) THEN
        DO I=1,N
          IF(FLAG(I).EQ.0) FF(I)=-1
        ENDDO
      ENDIF
      IF(M2.LE.2) THEN
        DO I=1,N
          IF(FLAG(I).NE.0) FF(I)=-1
        ENDDO
      ENDIF
      DO I=1,N
        F(I)=FF(I)
        IF(ABS(XD(1,I)).GT.100.) F(I)=100.
      ENDDO
      CI=0.
      CO=0.
      IF(N.LE.3.OR.N.GE.41) GO TO 999       ! too few/many for fit
CCC ONLY DO A+B
      IF(CHOICE.EQ.0) GO TO 998
      DO I=1,NFIT
        JJ(I)=1
        DO J=1,12
          Y(J,I)=-999999.
        ENDDO
        WT(I)=1.
      ENDDO
C
      J=0
C
      DO I=1,N
        CALL MUADD(ADD(I),MOD,PLN,WIR,ERR)
        IF(MOD.GE.100) THEN
          IF(F(I).EQ.0) THEN
            J=J+1
            IF(J.LE.NFIT) THEN
              IF(FLAG(I).EQ.0) THEN   ! END
                Y(1,J)=YT(I)
                Y(2,J)=YT(I)
                D(1,J)=XT(I)-XD(1,I)
                D(2,J)=XT(I)+XD(1,I)
              ELSE                    ! CENTRAL
                Y(1,J)=YT(I)-XD(1,I)
                Y(2,J)=YT(I)+XD(1,I)
                D(1,J)=XT(I)
                D(2,J)=XT(I)
              ENDIF
              NJ=2
              ISF(J,1,1)=I
              ISF(J,2,1)=I
              ISF(J,1,2)=-1
              ISF(J,2,2)=1
              IF(ABS(XD(2,I)).LT.6.) THEN     ! SECOND TIME
                IF(FLAG(I).EQ.0) THEN
                  Y(3,J)=YT(I)
                  Y(4,J)=YT(I)
                  D(3,J)=XT(I)-XD(2,I)
                  D(4,J)=XT(I)+XD(2,I)
                ELSE
                  Y(3,J)=XT(I)-XD(2,I)
                  Y(4,J)=XT(I)+XD(2,I)
                  D(3,J)=YT(I)
                  D(4,J)=YT(I)
                ENDIF
                NJ=NJ+2
                ISF(J,3,1)=I
                ISF(J,4,1)=I
                ISF(J,3,2)=-2
                ISF(J,4,2)=2
              ENDIF
CCC     SEE IF OTHER HITS IN THIS PLANE
              DO K=1,N
                IF(K.NE.I.AND.F(K).EQ.0) THEN 
                  CALL MUADD(ADD(K),M,P,WIR,ERR)
                  IF(M/10.EQ.MOD/10.AND.P.EQ.PLN) THEN
                    F(K)=99                       ! TURN 'OFF' HIT
                    IF(NJ.LT.11) THEN
                      NJ=NJ+2
                      IF(FLAG(K).EQ.0) THEN
                        Y(NJ-1,J)=YT(I)
                        Y(NJ,J)=YT(I)
                        D(NJ-1,J)=XT(K)-XD(1,K)
                        D(NJ,J)=XT(K)+XD(1,K)
                      ELSE
                        Y(NJ-1,J)=XT(I)-XD(1,K)
                        Y(NJ,J)=XT(I)+XD(1,K)
                        D(NJ-1,J)=YT(K)
                        D(NJ,J)=YT(K)
                      ENDIF
                      ISF(J,NJ-1,1)=K
                      ISF(J,NJ,1)=  K
                      ISF(J,NJ-1,2)=-1
                      ISF(J,NJ,2)=   1
                      IF(ABS(XD(2,K)).LT.6..AND.NJ.LT.11) THEN     ! SECOND TIME
                      IF(FLAG(K).EQ.0) THEN
                        Y(NJ+1,J)=YT(I)
                        Y(NJ+2,J)=YT(I)
                        D(NJ+1,J)=XT(K)-XD(2,K)
                        D(NJ+2,J)=XT(K)+XD(2,K)
                      ELSE
                        Y(NJ+1,J)=XT(I)-XD(2,K)
                        Y(NJ+2,J)=XT(I)+XD(2,K)
                        D(NJ+1,J)=YT(K)
                        D(NJ+2,J)=YT(K)
                      ENDIF
                        NJ=NJ+2
                        ISF(J,NJ-1,1)=K
                        ISF(J,NJ,1)=  K
                        ISF(J,NJ-1,2)=-2
                        ISF(J,NJ,2)=   2
                      ENDIF
                    ENDIF
                  ENDIF
                ENDIF
              ENDDO
              JJ(J)=NJ                    ! NUMBER OF HITS IN PLANE
            ENDIF
          ENDIF
        ENDIF
      ENDDO
      IF(J.LE.3.OR.J.GT.NFIT) GO TO 998
C
      CALL MUFIT4(J,JJ,Y,D,WT,IS,X1,Z1,SL1,C1)
C      SAVE DRIFT SOLUTIONS IN PACKED FORM
C
      DO I=1,J
        IOUT(ISF(I,IS(I),1))=ISF(I,IS(I),2)
      ENDDO
C
C    fit A layer, use pseudo point in magnet center
C
      DO I=1,NFIT
        JJ(I)=1
        DO J=1,12
        Y(J,I)=-999999.
        ENDDO
        WT(I)=1.
      ENDDO
C
CC    MAGNET PSEUDO POINT-----FIRST POINT
      Y(1,1)=ZBND
      D(1,1)=X1+SL1*(ZBND-Z1)
      JJ(1)=1
      WT(1)=WTMG
      J=1
C
      ZMIN=99999999.
C
      DO I=1,N
        CALL MUADD(ADD(I),MOD,PLN,WIR,ERR)
        IF(MOD.LT.100) THEN
          IF(F(I).EQ.0) THEN
            J=J+1
            IF(J.LE.NFIT) THEN
              IF(ABS(YT(I)).LT.ABS(ZMIN)) ZMIN=YT(I)
              IF(FLAG(I).EQ.0) THEN
              Y(1,J)=YT(I)
              Y(2,J)=YT(I)
              D(1,J)=XT(I)-XD(1,I)
              D(2,J)=XT(I)+XD(1,I)
              ELSE
              Y(1,J)=XT(I)-XD(1,I)
              Y(2,J)=XT(I)+XD(1,I)
              D(1,J)=YT(I)
              D(2,J)=YT(I)
              ENDIF
              NJ=2
              ISF(J,1,1)=I
              ISF(J,2,1)=I
              ISF(J,1,2)=-1
              ISF(J,2,2)=1
              IF(ABS(XD(2,I)).LT.6.) THEN     ! SECOND TIME
              IF(FLAG(I).EQ.0) THEN
              Y(3,J)=YT(I)
              Y(4,J)=YT(I)
              D(3,J)=XT(I)-XD(2,I)
              D(4,J)=XT(I)+XD(2,I)
              ELSE
              Y(3,J)=XT(I)-XD(2,I)
              Y(4,J)=XT(I)+XD(2,I)
              D(3,J)=YT(I)
              D(4,J)=YT(I)
              ENDIF
                NJ=NJ+2
                ISF(J,3,1)=I
                ISF(J,4,1)=I
                ISF(J,3,2)=-2
                ISF(J,4,2)=2
              ENDIF
CCC     SEE IF OTHER HITS IN THIS PLANE
              DO K=1,N
                IF(K.NE.I.AND.F(K).EQ.0) THEN 
                  CALL MUADD(ADD(K),M,P,WIR,ERR)
                  IF(M/10.EQ.MOD/10.AND.P.EQ.PLN) THEN
                    F(K)=99                      ! TURN 'OFF' HIT
                    IF(NJ.LT.11) THEN
                      NJ=NJ+2
              IF(FLAG(K).EQ.0) THEN
              Y(NJ-1,J)=YT(K)
              Y(NJ,J)=YT(K)
              D(NJ-1,J)=XT(K)-XD(1,K)
              D(NJ,J)=XT(K)+XD(1,K)
              ELSE
              Y(NJ-1,J)=XT(K)-XD(1,K)
              Y(NJ,J)=XT(K)+XD(1,K)
              D(NJ-1,J)=YT(K)
              D(NJ,J)=YT(K)
              ENDIF
                      ISF(J,NJ-1,1)=K
                      ISF(J,NJ,1)=  K
                      ISF(J,NJ-1,2)=-1
                      ISF(J,NJ,2)=   1
                      IF(ABS(XD(2,K)).LT.6..AND.NJ.LT.11) THEN     ! SECOND TIME
              IF(FLAG(K).EQ.0) THEN
              Y(NJ+1,J)=YT(K)
              Y(NJ+2,J)=YT(K)
              D(NJ+1,J)=XT(K)-XD(2,K)
              D(NJ+2,J)=XT(K)+XD(2,K)
              ELSE
              Y(NJ+1,J)=XT(K)-XD(2,K)
              Y(NJ+2,J)=XT(K)+XD(2,K)
              D(NJ+1,J)=YT(K)
              D(NJ+2,J)=YT(K)
              ENDIF
                        NJ=NJ+2
                        ISF(J,NJ-1,1)=K
                        ISF(J,NJ,1)=  K
                        ISF(J,NJ-1,2)=-2
                        ISF(J,NJ,2)=   2
                      ENDIF
                    ENDIF
                  ENDIF
                ENDIF
              ENDDO
              JJ(J)=NJ                    ! NUMBER OF HITS IN PLANE
            ENDIF
          ENDIF
        ENDIF
      ENDDO
      IF(J.LE.2.OR.J.GT.NFIT) THEN   ! CAN'T FIT A-LAYER
        X2=D(1,1)       
        Z2=Y(1,1)         
        C2=0.
        ZMIN=0.
        IF(IVER.EQ.0) THEN        ! BEAM
          IVTX=1
          SL2=(D(1,1)-XV)/(Y(1,1)-ZV)  ! PROJECT TO VERTEX
        ELSE                      ! RANDOM COSMIC
          SL2=SL1
        ENDIF
        XG=X2
        ZG=Z2
        SLG=SL2
      ELSE
        JVTX=0
        IF(IVER.EQ.0.AND.J.EQ.3) THEN ! BEAM + 2 ALAYER
          JVTX=1
          IVTX=1
          J=J+1
          Y(1,J)=ZV
          D(1,J)=XV
          JJ(J)=1
          WT(J)=WTVTX
        ENDIF
        CALL MUFIT4(J,JJ,Y,D,WT,IS,X2,Z2,SL2,C2)
C     REFIT WITH WTMG=1
        WT(1)=WTMG2
        DO I=1,J
          Y3(I)=Y(IS(I),I)
          X3(I)=D(IS(I),I)
        ENDDO
        CALL LINFIT(J,X3,Y3,WT,XG,ZG,SLG,VXG,VSL,CHIG)
C
C      SAVE DRIFT SOLUTIONS IN PACKED FORM
C
        IF(JVTX.EQ.0) THEN        
          DO I=2,J
            IOUT(ISF(I,IS(I),1))=ISF(I,IS(I),2)
          ENDDO
        ELSE
          DO I=2,J-1
            IOUT(ISF(I,IS(I),1))=ISF(I,IS(I),2)
          ENDDO
        ENDIF
      ENDIF
CCC    COMBINE 2 TRACKS
C
      XM=X1+SL1*(ZBND-Z1)
      XMG=XM
      ZM=ZBND
      SLO=SL1
      SLI=SL2
      SLIG=SLG
      SLOG=SL1
      ZI=ZMIN
      XI=X2+SL2*(ZMIN-Z2)
      XIG=XG+SLG*(ZMIN-ZG)
      CI=C2
      CO=C1
      GO TO 997
C----------------------------------------------------------------------
  998 CONTINUE     ! UNABLE TO FIT IN BC VIEW, TRY ANOTHER APPROACH
      DO I=1,N
        IOUT(I)=0
        F(I)=FF(I)
        IF(ABS(XD(1,I)).GT.100.) F(I)=100.
      ENDDO
C
C    fit A layer, use pseudo point AT VERTEX    IF BEAM
C
      DO I=1,NFIT
        JJ(I)=1
        DO J=1,12
        Y(J,I)=-999999.
        ENDDO
        WT(I)=1.
      ENDDO
C
CC    VERTEX PSEUDO POINT-----FIRST POINT
      IF(IVER.EQ.0) THEN
        IVTX=1
        Y(1,1)=ZV
        D(1,1)=XV
        JJ(1)=1
        WT(1)=WTVTX
        J=1
      ELSE
        J=0
      ENDIF
C
      ZMIN=99999999.
C
      DO I=1,N
        CALL MUADD(ADD(I),MOD,PLN,WIR,ERR)
        IF(MOD.LT.100) THEN
          IF(F(I).EQ.0) THEN
            J=J+1
            IF(J.LE.NFIT) THEN
              IF(ABS(YT(I)).LT.ABS(ZMIN)) ZMIN=YT(I)
              IF(FLAG(I).EQ.0) THEN   ! END
                Y(1,J)=YT(I)
                Y(2,J)=YT(I)
                D(1,J)=XT(I)-XD(1,I)
                D(2,J)=XT(I)+XD(1,I)
              ELSE                    ! CENTRAL
                Y(1,J)=XT(I)-XD(1,I)
                Y(2,J)=XT(I)+XD(1,I)
                D(1,J)=YT(I)
                D(2,J)=YT(I)
              ENDIF
              NJ=2
              ISF(J,1,1)=I
              ISF(J,2,1)=I
              ISF(J,1,2)=-1
              ISF(J,2,2)=1
              IF(ABS(XD(2,I)).LT.6.) THEN     ! SECOND TIME
                IF(FLAG(I).EQ.0) THEN
                  Y(3,J)=YT(I)
                  Y(4,J)=YT(I)
                  D(3,J)=XT(I)-XD(2,I)
                  D(4,J)=XT(I)+XD(2,I)
                ELSE
                  Y(3,J)=XT(I)-XD(2,I)
                  Y(4,J)=XT(I)+XD(2,I)
                  D(3,J)=YT(I)
                  D(4,J)=YT(I)
                ENDIF
                NJ=NJ+2
                ISF(J,3,1)=I
                ISF(J,4,1)=I
                ISF(J,3,2)=-2
                ISF(J,4,2)=2
              ENDIF
CCC     SEE IF OTHER HITS IN THIS PLANE
              DO K=1,N
                IF(K.NE.I.AND.F(K).EQ.0) THEN 
                  CALL MUADD(ADD(K),M,P,WIR,ERR)
                  IF(M/10.EQ.MOD/10.AND.P.EQ.PLN) THEN
                    F(K)=99                      ! TURN 'OFF' HIT
                    IF(NJ.LT.11) THEN
                      NJ=NJ+2
                      IF(FLAG(K).EQ.0) THEN
                        Y(NJ-1,J)=YT(I)
                        Y(NJ,J)=YT(I)
                        D(NJ-1,J)=XT(K)-XD(1,K)
                        D(NJ,J)=XT(K)+XD(1,K)
                      ELSE
                        Y(NJ-1,J)=XT(I)-XD(1,K)
                        Y(NJ,J)=XT(I)+XD(1,K)
                        D(NJ-1,J)=YT(K)
                        D(NJ,J)=YT(K)
                      ENDIF
                      ISF(J,NJ-1,1)=K
                      ISF(J,NJ,1)=  K
                      ISF(J,NJ-1,2)=-1
                      ISF(J,NJ,2)=   1
                      IF(ABS(XD(2,K)).LT.6..AND.NJ.LT.11) THEN     ! SECOND TIME
                      IF(FLAG(K).EQ.0) THEN
                        Y(NJ+1,J)=YT(I)
                        Y(NJ+2,J)=YT(I)
                        D(NJ+1,J)=XT(K)-XD(2,K)
                        D(NJ+2,J)=XT(K)+XD(2,K)
                      ELSE
                        Y(NJ+1,J)=XT(I)-XD(2,K)
                        Y(NJ+2,J)=XT(I)+XD(2,K)
                        D(NJ+1,J)=YT(K)
                        D(NJ+2,J)=YT(K)
                      ENDIF
                        NJ=NJ+2
                        ISF(J,NJ-1,1)=K
                        ISF(J,NJ,1)=  K
                        ISF(J,NJ-1,2)=-2
                        ISF(J,NJ,2)=   2
                      ENDIF
                    ENDIF
                  ENDIF
                ENDIF
              ENDDO
              JJ(J)=NJ                    ! NUMBER OF HITS IN PLANE
            ENDIF
          ENDIF
        ENDIF
      ENDDO
      IF(J.LE.2.OR.J.GT.NFIT) GO TO 999   ! CAN'T FIT A-LAYER
      CALL MUFIT4(J,JJ,Y,D,WT,IS,X2,Z2,SL2,C2)
C
C      SAVE DRIFT SOLUTIONS IN PACKED FORM
C
        IF(IVER.EQ.0) THEN        ! BEAM
          DO I=2,J
            IOUT(ISF(I,IS(I),1))=ISF(I,IS(I),2)
          ENDDO
        ELSE
          DO I=1,J
            IOUT(ISF(I,IS(I),1))=ISF(I,IS(I),2)
          ENDDO
        ENDIF
C
C    fit b-c layer
C
      DO I=1,NFIT
        JJ(I)=1
        DO J=1,12
        Y(J,I)=-999999.
        ENDDO
        WT(I)=1.
      ENDDO
C
CC    MAGNET PSEUDO POINT-----FIRST POINT
      Y(1,1)=ZBND
      D(1,1)=X2+SL2*(ZBND-Z2)
      JJ(1)=1
      WT(1)=WTMG
      J=1
C
      DO I=1,N
        CALL MUADD(ADD(I),MOD,PLN,WIR,ERR)
        IF(MOD.GE.100) THEN
          IF(F(I).EQ.0) THEN
            J=J+1
            IF(J.LE.NFIT) THEN
              IF(FLAG(I).EQ.0) THEN   ! END
                Y(1,J)=YT(I)
                Y(2,J)=YT(I)
                D(1,J)=XT(I)-XD(1,I)
                D(2,J)=XT(I)+XD(1,I)
              ELSE                    ! CENTRAL
                Y(1,J)=XT(I)-XD(1,I)
                Y(2,J)=XT(I)+XD(1,I)
                D(1,J)=YT(I)
                D(2,J)=YT(I)
              ENDIF
              NJ=2
              ISF(J,1,1)=I
              ISF(J,2,1)=I
              ISF(J,1,2)=-1
              ISF(J,2,2)=1
              IF(ABS(XD(2,I)).LT.6.) THEN     ! SECOND TIME
                IF(FLAG(I).EQ.0) THEN
                  Y(3,J)=YT(I)
                  Y(4,J)=YT(I)
                  D(3,J)=XT(I)-XD(2,I)
                  D(4,J)=XT(I)+XD(2,I)
                ELSE
                  Y(3,J)=XT(I)-XD(2,I)
                  Y(4,J)=XT(I)+XD(2,I)
                  D(3,J)=YT(I)
                  D(4,J)=YT(I)
                ENDIF
                NJ=NJ+2
                ISF(J,3,1)=I
                ISF(J,4,1)=I
                ISF(J,3,2)=-2
                ISF(J,4,2)=2
              ENDIF
CCC     SEE IF OTHER HITS IN THIS PLANE
              DO K=1,N
                IF(K.NE.I.AND.F(K).EQ.0) THEN 
                  CALL MUADD(ADD(K),M,P,WIR,ERR)
                  IF(M/10.EQ.MOD/10.AND.P.EQ.PLN) THEN
                    F(K)=99                       ! TURN 'OFF' HIT
                    IF(NJ.LT.11) THEN
                      NJ=NJ+2
                      IF(FLAG(K).EQ.0) THEN
                        Y(NJ-1,J)=YT(I)
                        Y(NJ,J)=YT(I)
                        D(NJ-1,J)=XT(K)-XD(1,K)
                        D(NJ,J)=XT(K)+XD(1,K)
                      ELSE
                        Y(NJ-1,J)=XT(I)-XD(1,K)
                        Y(NJ,J)=XT(I)+XD(1,K)
                        D(NJ-1,J)=YT(K)
                        D(NJ,J)=YT(K)
                      ENDIF
                      ISF(J,NJ-1,1)=K
                      ISF(J,NJ,1)=  K
                      ISF(J,NJ-1,2)=-1
                      ISF(J,NJ,2)=   1
                      IF(ABS(XD(2,K)).LT.6..AND.NJ.LT.11) THEN     ! SECOND TIME
                      IF(FLAG(K).EQ.0) THEN
                        Y(NJ+1,J)=YT(I)
                        Y(NJ+2,J)=YT(I)
                        D(NJ+1,J)=XT(K)-XD(2,K)
                        D(NJ+2,J)=XT(K)+XD(2,K)
                      ELSE
                        Y(NJ+1,J)=XT(I)-XD(2,K)
                        Y(NJ+2,J)=XT(I)+XD(2,K)
                        D(NJ+1,J)=YT(K)
                        D(NJ+2,J)=YT(K)
                      ENDIF
                        NJ=NJ+2
                        ISF(J,NJ-1,1)=K
                        ISF(J,NJ,1)=  K
                        ISF(J,NJ-1,2)=-2
                        ISF(J,NJ,2)=   2
                      ENDIF
                    ENDIF
                  ENDIF
                ENDIF
              ENDDO
              JJ(J)=NJ                    ! NUMBER OF HITS IN PLANE
            ENDIF
          ENDIF
        ENDIF
      ENDDO
      IF(J.LE.2.OR.J.GT.NFIT) GO TO 999
C
      CALL MUFIT4(J,JJ,Y,D,WT,IS,X1,Z1,SL1,C1)
C     REFIT WITH WTMG=1
        WT(1)=WTMG2
        DO I=1,J
          Y3(I)=Y(IS(I),I)
          X3(I)=D(IS(I),I)
        ENDDO
        CALL LINFIT(J,X3,Y3,WT,XG,ZG,SLG,VXG,VSL,CHIG)
C      SAVE DRIFT SOLUTIONS IN PACKED FORM
C
      DO I=2,J
        IOUT(ISF(I,IS(I),1))=ISF(I,IS(I),2)
      ENDDO
CCC    COMBINE 2 TRACKS
C
      XM=X1+SL1*(ZBND-Z1)
      XMG=XG+SLG*(ZBND-ZG)
      ZM=ZBND
      SLO=SL1
      SLOG=SLG
      SLIG=SL2
      SLI=SL2
      ZI=ZMIN
      XI=X2+SL2*(ZMIN-Z2)
      XIG=XI
      CI=C2
      CO=C1
997   CONTINUE
C   CALCULATE QUALITY OF FIT; USE LARGE WTMG
      CMAX=0.
      IMAX=0
      C12=0.
      N1=0
      DO I=1,N
        IF(ABS(XT(I)).LT.9999.) THEN   
          IF(IOUT(I).NE.0) THEN
        IF(FLAG(I).EQ.0) THEN
          IF(ABS(YT(I)).GT.ABS(ZBND)) THEN
            XP=XM+SLO*(YT(I)-ZM)
          ELSE IF(ABS(YT(I)).LT.ABS(ZBND)) THEN
            XP=XI+SLI*(YT(I)-ZI)
          ENDIF
            XH=XT(I)+IOUT(I)/IABS(IOUT(I))*XD(IABS(IOUT(I)),I)
         ELSE
          IF(ABS(XT(I)).GT.ABS(ZBND)) THEN
            XP=ZM+(YT(I)-XM)/SLO
          ELSE IF(ABS(XT(I)).LT.ABS(ZBND)) THEN
            XP=ZI+(YT(I)-XI)/SLI
          ENDIF
            XH=XT(I)+IOUT(I)/IABS(IOUT(I))*XD(IABS(IOUT(I)),I)
          ENDIF
            N1=N1+1
            C12=C12 + (XP-XH)**2
            IF(ABS(XP-XH).GT.CMAX) THEN
              CMAX=ABS(XP-XH)
              IMAX=I
            ENDIF
          ENDIF
        ENDIF
      ENDDO
      RATIO=CMAX**2/C12
      IF(N1.GT.2) THEN
        C12=SQRT(C12/(N1-2))
      ELSE
        C12=-1.
      ENDIF
      IF(N1.GT.5) THEN
        IF(IPASS.EQ.1) THEN
          IPASS=2
          IF(C12.GT.0.3) THEN   
            IF((CMAX.GT.1..AND.RATIO.GT..3).OR.
     A         (CMAX.GT..6.AND.RATIO.GT..6)) THEN
              FF(IMAX)=-1.
CCC   ALSO WIPE OUT OTHER POINTS IN THE SAME PLANE
              CALL MUADD(ADD(IMAX),MOD,PLN,WIR,ERR)
              DO K=1,NN
                CALL MUADD(ADD(K),M,P,WIR,ERR)
                IF(M.EQ.MOD.AND.P.EQ.PLN) FF(K)=-1.
              ENDDO
              GO TO 1000
            ENDIF
          ENDIF
        ENDIF
      ENDIF
      XM=XMG
      XI=XIG
      SLI=SLIG
      SLO=SLOG
      RETURN
C----------------------------------------------------------------------
  999 CONTINUE
CCCCCCCCCCCCCCC    UNABLE TO FIT
      IVTX=0
      C12=9999.
      CI=9999.
      CO=9999.
      XM=9999.
      XI=9999.
      ZI=9999.
      ZM=ZBND
      SLI=9999.
      SLO=9999.
      RETURN
      END
