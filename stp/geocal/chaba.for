      SUBROUTINE CHABA(IEST,NPNEW,UVW,NPOLD,XYZ,TOL,
     &  RM,KA,OM,PH,TR,SCL,XYZN,MI,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :  TRANSFER THREE DIMENSIONAL COORDINATES FROM ONE
C-            SYSTEM (POSITION) TO ANOTHER BY USING HELMERT TRANSFORMATION
C-            WITH CALCULATION OF THREE ROTATION ANGLE, THREE TRANSLATION,
C-            AND SCALE FACTOR
C-
C-            OBSERVATION EQUATION : Xnew = S * Ry * Rx * Rz * Xold + Xo
C-
C----------------------------------------------------------------------
C-     TRANSLATED AND DOCUMENTED BY  MAY CHAU
C-*********************************************************************
C-     DERNIERE VERSION 11.4.89 DU CHANGEMENT DE BASE EN FORTRAN 77
C-*********************************************************************
C-
C-   Inputs  : IEST(10) [I]   control for fitting
C-             NPNEW    [I]   NUMBER OF NEW POINTS
C-             UVW(3,*) [R*8] NEW COORDINATE POINTS
C-             NPOLD    [I]   NUMBER OF OLD POINTS (NPOLD>NPNEW)
C-             XYZ(3,*) [R*8] OLD COORDINATE POINTS
C-             TOL      [R*8] Rejection tolerance
C-
C-   Outputs : RM(3,3)  [R*8] ROTATION MATRIX
C-             KA       [R*8] ROTATION angle (radians) around Z
C-             OM       [R*8] ROTATION angle (radians) around Y
C-             PH       [R*8] ROTATION angle (radians) around X
C-             TR(3)    [R*8] Translation vector
C-             SCL      [R*8] Scale factor
C-             XYZN(3,*)[R*8] OLD points transformed to NEW system
C-             MI(*)    [I]   flag points out of tolerance =1
C-             IER      [I]   0=OK
C-   Controls:
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IEST(10),NPNEW,NPOLD,IER,MI(*)
      REAL*8 UVW(3,*),XYZ(3,*),XYZN(3,*),RM(3,3),KA,OM,PH,TR(3),SCL,TOL
C
      INTEGER TESTIT,OPTECH,B
      INTEGER I,J,K,IUNK,JIT,IM,II,JJ
      REAL*8  X(200),Y(200),Z(200),VAL(10),P
      REAL*8  W2(200),W3(200),W4(200),DW2(200),DW3(200),DW4(200)
      REAL*8  W7(200),DW(200),DX(200),DY(200),DZ(200)
      REAL*8  DD(200),U(200),V(200),W(200),A(10),EN(10,11)
      REAL*8  KAPAGR,OMEGR,PHIGR,L,DD2,EOPT
      REAL*8  KA1,OM1,PH1,ECH,ECHXY,ECHXZ,ECHYZ,TX,TY,TZ,XYZG
      REAL*8  PI,ROGR,GRRO,DR1O,DR2O,DR3O
      REAL*8  P1,P2,P3,Q1,Q2,Q3,R1,R2,R3,DP1O,DP2O,DP3O,DQ1O,DQ2O,DQ3O
C----------------------------------------------------------------------
      PI=4.0D0*DATAN(1.0D0)
      ROGR = 200D0/PI
      GRRO = PI/200D0
      CALL VZERO(RM(1,1),18)
      CALL VZERO(TR(1),6)
      IER = 0
C
C ****  OPTIONS
C
      EOPT = 0D0
      IUNK = 0
      DO 55,I = 1,10
   55 IF(IEST(J).EQ.0)  IUNK = IUNK + 1
      IF (IEST(1) .EQ. 1) EOPT    = 1D0
      IF (IEST(1).EQ.0 .OR. IEST(8).EQ.0 .OR. IEST(9).EQ.0 .OR.
     +         IEST(10).EQ.0) EOPT = 0D0
      IF (IEST(1) .EQ. 0) THEN
        IF(IUNK.GT.7) IUNK = 7
        DO 540 I=8,10
          IF (IEST(I) .EQ. 0) THEN
            IEST(I)  = 1
          END IF
  540   CONTINUE
      END IF
C
      DO I = 1, NPNEW
        U(I) = UVW(1,I)
        V(I) = UVW(2,I)
        W(I) = UVW(3,I)
      END DO
      DO I = 1, NPOLD
        X(I) = XYZ(1,I)
        Y(I) = XYZ(2,I)
        Z(I) = XYZ(3,I)
      END DO
      PH   = 0D0
      OM   = 0D0
      KA   = 0D0
      ECH  = 1D0
      TX   = 0D0
      TY   = 0D0
      TZ   = 0D0
      ECHXY  = 1D0
      ECHXZ  = 1D0
      ECHYZ  = 1D0
C
      IF(TOL .EQ. 0D0)THEN
C        WRITE(2,*)' VOUS N AVEZ PAS DONNE DE TOLERANCE DE REJET'
        WRITE(2,*)' ERROR -- REJECTION TOLERANCE IS NOT ASSIGNED '
        STOP '          CHECK DATA FILE'
      END IF
C
      XYZG = 1D0
      DO 120 I=1,NPOLD
         IF (MI(I) .NE. 0) THEN
            XYZG = XYZG + 1D0
         END IF
120   CONTINUE
      IF (XYZG.LT.2 .OR. (XYZG.LE.2 .AND. IUNK.EQ.7)) THEN
        CALL ERRMSG('INSUFFICIENT NUMBER OF (PASSIVE) POINTS','CHABA',
     &    'IN THE OLD SYSTEM','W')
        IER = -1
        GOTO 999
      ENDIF
C
C * CALCUL ITERATIF DES 13 INCONNUES *
C
      DO 225 OPTECH=1,2
110      DO 210 JIT =1,50
            DO 115 I=1,10
               DO 115 J=1,11
                  EN(I,J) = 0D0
115         CONTINUE
            TESTIT=0
C
C FORMATION OF OBSERVATIONS EQUATIONS
C
            P1 = DCOS(KA)*DCOS(PH)+DSIN(KA)*DSIN(OM)*DSIN(PH)
            P2 = DCOS(KA)*DSIN(OM)*DSIN(PH)-DSIN(KA)*DCOS(PH)
            P3 = -DCOS(OM)*DSIN(PH)
            Q1 = DSIN(KA)*DCOS(OM)
            Q2 = DCOS(KA)*DCOS(OM)
            Q3 = DSIN(OM)
            R1 = DCOS(KA)*DSIN(PH)-DSIN(KA)*DSIN(OM)*DCOS(PH)
            R2 = -DSIN(KA)*DSIN(PH)-DCOS(KA)*DSIN(OM)*DCOS(PH)
            R3 = DCOS(OM)*DCOS(PH)
            DP1O = DSIN(KA)*DSIN(PH)*DCOS(OM)
            DP2O = DCOS(KA)*DSIN(PH)*DCOS(OM)
            DP3O = DSIN(OM)*DSIN(PH)
            DQ1O = -DSIN(KA)*DSIN(OM)
            DQ2O = -DCOS(KA)*DSIN(OM)
            DQ3O = DCOS(OM)
            DR1O = -DSIN(KA)*DCOS(OM)*DCOS(PH)
            DR2O = -DCOS(KA)*DCOS(OM)*DCOS(PH)
            DR3O = -DSIN(OM)*DCOS(PH)
            KA1 = KA
            OM1 = OM
            PH1 = PH
C
C FORMATION OF NORMAL EQUATIONS      (FORMATION DES EQUATIONS NORMALES)
C
            DO 130 I=1,NPOLD
               IF (MI(I) .NE. 0) THEN
                  CALL VZERO(A,20)
                  L = U(I) - ECH*(P1*(X(I)) +
     +                P2*(Y(I)) + P3*(Z(I))) - TX
                  IF (IEST(8).EQ.0.OR.IEST(9).EQ.0.OR.IEST(10).EQ.0)THEN
                    CALL ERRMSG('IEST=0 8,9,10 NOT SUPPORTED','CHABA',
     &             'OOPS','W')
                     L = U(I) - (ECHXY*ECHXZ*P1*(X(I)) +
     +                                ECHXY*ECHYZ*P2*(Y(I)) +
     +                                ECHXZ*ECHYZ*P3*(Z(I))) - TX
                  END IF
                  IF (IEST(2) .NE. 1) A(2) = ECH*(P2*(X(I)) -
     +                                       P1*(Y(I)))
                  IF (IEST(3) .NE. 1) A(3) = ECH*(DP1O*(X(I)) +
     +                                  DP2O*(Y(I)) + DP3O*(Z(I)))
                  IF (IEST(4) .NE. 1) A(4) = ECH*(-R1*(X(I)) -
     +                                      R2*(Y(I)) - R3*(Z(I)))
                  IF (OPTECH .NE. 1) THEN
                     IF (IEST(1) .NE. 1) A(1) = P1*(X(I)) +
     +                                      P2*(Y(I)) + P3*(Z(I))
                     IF (IEST(8) .NE. 1) A(8) = P1*(X(I)) +
     +                                          P2*(Y(I))
                     IF (IEST(9) .NE. 1) A(9) = P1*(X(I)) +
     +                                          P3*(Z(I))
                     IF (IEST(10) .NE. 1) A(10) = P2*(Y(I)) +
     +                                            P3*(Z(I))
                  END IF
                  IF (IEST(5) .NE. 1) A(5) = 1D0
C                  CALL EQNOR(A,EN,L,B,IUNK)
                  DO II=1,10
                    DO JJ=1,10
                      EN(II,JJ) = A(II)*A(JJ) + EN(II,JJ)
                    END DO
                    EN(II,11) = A(II)*L + EN(II,11)
                  END DO
                  CALL VZERO(A,20)
                  L = V(I) -  ECH*(Q1*(X(I)) + Q2*(Y(I)) +
     +                Q3*(Z(I))) - TY
                  IF (IEST(8).EQ.0.OR.IEST(9).EQ.0.OR.IEST(10).EQ.0)THEN
                    CALL ERRMSG('IEST=0 8,9,10 NOT SUPPORTED','CHABA',
     &             'OOPS','W')
                     L = V(I)  - (ECHXY*ECHXZ*Q1*(X(I)) +
     +                                ECHXY*ECHYZ*Q2*(Y(I)) +
     +                                ECHXZ*ECHYZ*Q3*(Z(I))) - TY
                  END IF
                  IF (IEST(2) .NE. 1) A(2) = ECH*(Q2*(X(I)) -
     +                                       Q1*(Y(I)))
                  IF (IEST(3) .NE. 1) A(3) = ECH*(DQ1O*(X(I)) +
     +                                  DQ2O*(Y(I)) + DQ3O*(Z(I)))
                  IF (OPTECH .NE. 1) THEN
                     IF (IEST(1) .NE. 1) A(1) = Q1*(X(I)) +
     +                                      Q2*(Y(I)) + Q3*(Z(I))
                     IF (IEST(8) .NE. 1) A(8) = Q1*(X(I)) +
     +                                          Q2*(Y(I))
                     IF (IEST(9) .NE. 1) A(9) = Q1*(X(I)) +
     +                                          Q3*(Z(I))
                     IF (IEST(10) .NE. 1) A(10) = Q2*(Y(I)) +
     +                                            Q3*(Z(I))
                  END IF
                  IF (IEST(6) .NE. 1) A(6) = 1D0
                  DO II=1,10
                    DO JJ=1,10
                      EN(II,JJ) = A(II)*A(JJ) + EN(II,JJ)
                    END DO
                    EN(II,11) = A(II)*L + EN(II,11)
                  END DO
                  CALL VZERO(A,20)
                  L = W(I) - ECH*(R1*(X(I)) + R2*(Y(I)) +
     +                R3*(Z(I))) - TZ
                  IF (IEST(8).EQ.0.OR.IEST(9).EQ.0.OR.IEST(10).EQ.0)THEN
                     L = W(I) - (ECHXY*ECHXZ*R1*(X(I)) +
     +                                ECHXY*ECHYZ*R2*(Y(I)) +
     +                                ECHXZ*ECHYZ*R3*(Z(I))) - TZ
                  END IF
                  IF (IEST(2) .NE. 1) A(2) = ECH*(R2*(X(I)) -
     +                                       R1*(Y(I)))
                  IF (IEST(3) .NE. 1) A(3) = ECH*(DR1O*(X(I)) +
     +                                  DR2O*(Y(I)) + DR3O*(Z(I)))
                  IF (IEST(4) .NE. 1) A(4) = ECH*(P1*(X(I)) +
     +                                      P2*(Y(I)) + P3*(Z(I)))
                  IF (OPTECH .NE. 1) THEN
                     IF (IEST(1) .NE. 1) A(1) = R1*(X(I)) +
     +                                      R2*(Y(I)) + R3*(Z(I))
                     IF (IEST(8) .NE. 1) A(8) = R1*(X(I)) +
     +                                          R2*(Y(I))
                     IF (IEST(9) .NE. 1) A(9) = R1*(X(I)) +
     +                                          R3*(Z(I))
                     IF (IEST(10) .NE. 1) A(10) = R2*(Y(I)) +
     +                                            R3*(Z(I))
                  END IF
                  IF (IEST(7) .NE. 1) A(7) = 1D0
                  DO II=1,10
                    DO JJ=1,10
                      EN(II,JJ) = A(II)*A(JJ) + EN(II,JJ)
                    END DO
                    EN(II,11) = A(II)*L + EN(II,11)
                  END DO
               END IF
130         CONTINUE
C
C CALCULATE THE RESIDUAL FROM THE NORMAL MATRIX, AND NEW PARAMETERS OF THE
C TRANSFORMATION (CALCULATE SOLUTION OF NORMAL MATRIX AND NEW TRANSFORMATION
C PARAMETERS)
C SOLVE A SET OF NORMAL EQUATION BY USING GAUSS ELIMINATION METHOD GIVEN
C THE NORMAL MATRIX TERMS AND THE SOLUTION ON THE RIGHT HAND SIDE
C
            DO 150 I=1,10
               IF (EN(I,I) .NE. 0D0) THEN
                  P = 1/EN(I,I)
                  DO 135 J=I,11
                     EN(I,J) = P*EN(I,J)
135               CONTINUE
C
                  DO 145 K=1,10
                     IF (K .NE. I) THEN
                        P = -EN(K,I)
                        DO 140 J=I,11
                           EN(K,J) = EN(K,J) + P*EN(I,J)
140                     CONTINUE
                     END IF
145               CONTINUE
               END IF
150         CONTINUE
C
C  UPDATE KAPPA ROTATION
C
            IF (IEST(2) .EQ. 0) THEN
               KA = KA + EN(2,11)
               DO WHILE (KA .LT. 0D0)
                  KA = KA + 2D0*PI
               END DO
               DO WHILE (KA .GT. 2D0*PI)
                  KA = KA - 2D0*PI
               END DO
            END IF
            KAPAGR = KA*ROGR
C
C  UPDATE OMEGA ROTATION
C
            IF (IEST(3) .EQ. 0) THEN
               OM = OM + EN(3,11)
               DO WHILE (OM .LT. 0D0)
                  OM = OM + 2D0*PI
               END DO
               DO WHILE (OM .GT. 2D0*PI)
                  OM = OM - 2D0*PI
               END DO
            END IF
            OMEGR = OM*ROGR
C
C  UPDATE PHI ROTATION
C
            IF (IEST(4) .EQ. 0) THEN
               PH = PH + EN(4,11)
               DO WHILE (PH .LT. 0D0)
                  PH = PH + 2D0*PI
               END DO
               DO WHILE (PH .GT. 2D0*PI)
                  PH = PH - 2D0*PI
               END DO
            END IF
            PHIGR = PH*ROGR
C
C  SCALE & TRANSLATION UPDATES
C
            IF (IEST(1) .EQ. 0D0) ECH = ECH + EN(1,11)
            IF (IEST(8) .EQ. 0D0) ECHXY = ECHXY + EN(8,11)
            IF (IEST(9) .EQ. 0D0) ECHXZ = ECHXZ + EN(9,11)
            IF (IEST(10) .EQ. 0D0) ECHYZ = ECHYZ + EN(10,11)
            IF (IEST(5) .EQ. 0D0) TX = TX + EN(5,11)
            IF (IEST(6) .EQ. 0D0) TY = TY + EN(6,11)
            IF (IEST(7) .EQ. 0D0) TZ = TZ + EN(7,11)
C
C CRITERE ITERATIF
C ----------------
            DO 185 I=1,NPOLD
               IF (IEST(1).EQ.0 .OR. (IEST(8).EQ.1.AND.IEST(9).EQ.1
     +                                        .AND.IEST(10).EQ.1)) THEN
                  W2(I) = ECH*(P1*(X(I)) + P2*(Y(I)) +
     +                    P3*(Z(I))) + TX
                  W3(I) = ECH*(Q1*(X(I)) + Q2*(Y(I)) +
     +                    Q3*(Z(I))) + TY
                  W4(I) = ECH*(R1*(X(I)) + R2*(Y(I)) +
     +                    R3*(Z(I))) + TZ
               ELSE
                 CALL ERRMSG('IEST 8,9,10 NOT SUPPORTED','CHABA',
     &             'OOPS','W')
                  W2(I) = ECHXY*ECHXZ*P1*(X(I)) +
     +                    ECHXY*ECHYZ*P2*(Y(I)) +
     +                    ECHXZ*ECHYZ*P3*(Z(I)) + TX
                  W3(I) = ECHXY*ECHXZ*Q1*(X(I)) +
     +                    ECHXY*ECHYZ*Q2*(Y(I)) +
     +                    ECHXZ*ECHYZ*Q3*(Z(I)) + TY
                  W4(I) = ECHXY*ECHXZ*R1*(X(I)) +
     +                    ECHXY*ECHYZ*R2*(Y(I)) +
     +                    ECHXZ*ECHYZ*R3*(Z(I)) + TZ

               END IF
               DD2 = DSQRT((DW2(I)-W2(I))**2 + (DW3(I)-W3(I))**2 +
     +               (DW4(I)-W4(I))**2)
               IF (DD2 .GT. 1D-7) TESTIT = 1
185         CONTINUE
            IF (TESTIT.EQ.1) THEN
               DO 190 I=1,NPOLD
                  DW2(I) = W2(I)
                  DW3(I) = W3(I)
                  DW4(I) = W4(I)
190            CONTINUE
            ELSE
               IF ((EOPT.EQ.0D0).AND.(OPTECH.EQ.1)) GOTO 225
               DO 195 I=1,NPOLD
                  IF (MI(I) .EQ. 0) THEN
                     W7(I) = 0D0
                  ELSE
                     W7(I) = DSQRT((U(I)-W2(I))**2 + (V(I)-W3(I))**2 +
     +                       (W(I)-W4(I))**2)
                  ENDIF
195            CONTINUE
               IM =1
               DO 205, J=2,NPOLD
  205          IF(W7(J).GT.W7(IM)) IM=J
               IF (W7(IM) .GT. TOL) THEN
                  IF (NPNEW.LT.3 .OR. (NPNEW.EQ.3.AND.IUNK.EQ.7)) THEN
                     CALL ERRMSG('TOO MANY POINTS EXCEED TOLERANCE',
     &                 'CHABA','CHECK THE DEFAULT VALUE','W' )
                     IER = -1
                     GOTO 999
                  ELSE
                     NPNEW    = NPNEW - 1
                     MI(IM) = 0
                     GO TO 110
                  END IF
               END IF
               GO TO 225
            END IF
210      CONTINUE
         IF (EOPT.EQ.0D0 .AND. OPTECH.EQ.1) GOTO 225
         WRITE(2,215)
215      FORMAT(4X,'WARNING : CONVERGENCE CAN NOT BE REACH, ',
     1          'ERROR IN RESULT' )
         IER = -1
         GO TO 999
225   CONTINUE
C
      RM(1,1) =  P1
      RM(1,2) =  P2
      RM(1,3) =  P3
      RM(2,1) =  Q1
      RM(2,2) =  Q2
      RM(2,3) =  Q3
      RM(3,1) =  R1
      RM(3,2) =  R2
      RM(3,3) =  R3
      KA = KA1
      OM = OM1
      PH = PH1
      DO I=1,NPOLD
        XYZN(1,I) = W2(I)
        XYZN(2,I) = W3(I)
        XYZN(3,I) = W4(I)
      END DO
      TR(1) = TX
      TR(2) = TY
      TR(3) = TZ
      SCL  = ECH
  999 RETURN
      END
