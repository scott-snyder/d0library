      SUBROUTINE ZKINEM(PELEC1,PELEC2,PTOT,ARESUL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C
C  Calculate invariant mass and other kinematic quantites
C
C inputs:  PELEC1(): lepton+- 3-momentum
C          PELEC2(): 2nd elec 3-mom
C
C outputs: ARESUL(1): pair mass
C          ARESUL(2): elec1    Et
C          ARESUL(3): elec2    Et
C          ARESUL(4): Z pt total
C          ARESUL(5): Z ptx
C          ARESUL(6): Z pty
C          ARESUL(7): Z pt eta
C          ARESUL(8): Z pt xi
C          ARESUL(9-12) : elec1    4-momentum
C          ARESUL(13-16) : elec2    4-momentum
C          ARESUL(17-20) : Z        4-momentum
C          ARESUL(21): Z rapidity
C          ARESUL(22-25) : elec1    4-mom in Z rest frame
C          ARESUL(26): cos(theta*), ele decay angle in Z r.f.
C          ARESUL(27): trans angle between ele (Z.r.f) and Z pt
C          ARESUL(28): pt recoil (total)
C          ARESUL(29): pt recoil (eta component)
C          ARESUL(30): pt recoil (xi component)
C          ARESUL(31): pt-con mass (using leg 1 as good leg)
C          ARESUL(32): pt-con mass (using leg 2 as good leg)
C          ARESUL(33): pt-con peta (using leg 1 as good leg)
C          ARESUL(34): pt-con peta (using leg 2 as good leg)
C          ARESEL(35): pt recoil (component par to e1)
C          ARESEL(36): pt recoil (component perp to e1)
C          ARESEL(37): pt recoil (component par to e2)
C          ARESEL(38): pt recoil (component perp to e2)
C          ARESEL(39:50): spares
C
C-
C-   Created  20-SEP-1992   Darien R. Wood
C-
C--------------------------------------------------------------------
C
      IMPLICIT NONE
C
      INTEGER IAMAX
      PARAMETER(IAMAX=50)
      REAL ARESUL(IAMAX)
      REAL PELEC1(3),PELEC2(3),PTOT(3)
      REAL PREC(3),APTREC,PTRETA,PTRXI
      REAL P4EL1(4),P4EL2(4),P4Z(4),P4ERST(4)
      REAL VNEL1(2),VNEL2(2),VNETA(2),VNXI(2)
      REAL PTETA,PTXI,DTEST,PHIZ,APTZ
      REAL ETE1,ETE2,AM2,AM
      REAL PTOTZ
      REAL VDOT,VMOD,VDOTN
      REAL CSTHST,COSCOR,ANGCOR
      REAL YIVB
      REAL PTOTXI,PXI1,PXI2,ALPH1,ALPH2,APCON1,APCON2
      REAL P4CON1(4),P4CON2(4),PZCON1(4),PZCON2(4)
      REAL AMCON1,AMCON2,PCETA1,PCETA2,AM2C
      REAL UPAR1,UPAR2,UPER1,UPER2
      REAL COS_ALPH
C
C
      EXTERNAL VDOT,VMOD,VDOTN,UCOPY,VADD,VUNIT,LORENF
C
C--------------------------------------------------------------
C
      CALL VZERO(ARESUL,IAMAX)
C
      ETE1 = SQRT(PELEC1(1)**2 + PELEC1(2)**2)
      ETE2 = SQRT(PELEC2(1)**2 + PELEC2(2)**2)
C protection against nonsense input
      IF(ETE1.EQ.0. .OR. ETE2.EQ.0.) GOTO 77777
      COS_ALPH = VDOTN(PELEC1,PELEC2,3)
      IF(COS_ALPH.GT.0.999) GOTO 77777
C
C turn 3-mom into 4-mom
      CALL UCOPY(PELEC1,P4EL1,3)
      P4EL1(4) = VMOD(P4EL1,3)
      CALL UCOPY(PELEC2,P4EL2,3)
      P4EL2(4) = VMOD(P4EL2,3)
C calc 4-mom of Z
      CALL VADD(P4EL1,P4EL2,P4Z,4)
      PTOTZ = VMOD(P4Z,3)
      AM2 = P4Z(4)**2-PTOTZ**2
      IF(AM2.GT.0.) THEN
        AM = SQRT(AM2)
      ELSE
        PRINT *,' NEGATIVE INVARIANTMASSSQ:',AM2
        PRINT *,' P4ELE1:',P4EL1
        PRINT *,' P4ELE2:',P4EL2
        PRINT *,' P4Z:   ',P4Z
        AM = 0.
      ENDIF
C  Z pt calculation
      APTZ = VMOD(P4Z,2)
      PHIZ = ATAN2(P4Z(2),P4Z(1))
C  set up the eta-xi coordinate system
      CALL VUNIT(PELEC1,VNEL1,2)
      CALL VUNIT(PELEC2,VNEL2,2)
      CALL VADD(VNEL1,VNEL2,VNETA,2)
      DTEST = VMOD(VNETA,2)
      IF(DTEST.EQ.0.) THEN
C preciscely back-to-back, sign of eta axis is arbitrary
        VNETA(1) = VNEL1(2)
        VNETA(2) = -VNEL1(1)
      ELSE
C eta axis is well-determined  
        CALL VUNIT(VNETA,VNETA,2)
      ENDIF  
      VNXI(1) = VNETA(2)
      VNXI(2) = -VNETA(1)
      DTEST = VDOT(VNXI,VNEL1,2)
      IF(DTEST.LT.0) THEN
        VNXI(1) = -VNXI(1)
        VNXI(2) = -VNXI(2)
      ENDIF
      PTETA = VDOT(P4Z,VNETA,2)
      PTXI = VDOT(P4Z,VNXI,2)
C examine recoil
      CALL VSUB(PTOT,P4Z,PREC,3)
      APTREC = VMOD(PREC,2)
      PTRETA = VDOT(PREC,VNETA,2)
      PTRXI = VDOT(PREC,VNXI,2)
      UPAR1 = VDOT(PREC,VNEL1,2)
      UPAR2 = VDOT(PREC,VNEL2,2)
      UPER1 = PREC(1)*VNEL1(2) - PREC(2)*VNEL1(1)
      UPER2 = PREC(1)*VNEL2(2) - PREC(2)*VNEL2(1)
C calculate ptconstrained quantities
      PTOTXI = PTXI+PTRXI
      PXI1 = VDOT(VNXI,P4EL1,2)
      PXI2 = VDOT(VNXI,P4EL2,2)
      IF(PXI1.NE.0. .AND. PXI2.NE.0.) THEN
        ALPH1 = 1. - PTOTXI/PXI1
        IF(ALPH1.LT.0.) ALPH1 = 0.
        ALPH2 = 1. - PTOTXI/PXI2
        IF(ALPH2.LT.0.) ALPH2 = 0.
        CALL VSCALE(P4EL1,ALPH1,P4CON1,4)
        CALL VSCALE(P4EL2,ALPH2,P4CON2,4)
        CALL VADD(P4EL1,P4CON2,PZCON1,4)
        CALL VADD(P4CON1,P4EL2,PZCON2,4)
        APCON1 = VMOD(PZCON1,3)
        APCON2 = VMOD(PZCON2,3)
        IF(ALPH2.GT.0.) THEN
          AM2C = PZCON1(4)**2-APCON1**2
          IF(AM2C.GT.0.) THEN
            AMCON1 = SQRT(AM2C)
          ELSE
            AMCON1 = 0.
          ENDIF  
        ELSE
          AMCON1 = 0.
        ENDIF
        IF(ALPH2.GT.0.) THEN
          AM2C = PZCON2(4)**2-APCON2**2
          IF(AM2C.GT.0.) THEN
            AMCON2 = SQRT(AM2C)
          ELSE
            AMCON2 = 0.
          ENDIF  
        ELSE
          AMCON2 = 0.
        ENDIF
        PCETA1 = VDOT(PZCON1,VNETA,2)
        PCETA2 = VDOT(PZCON2,VNETA,2)
      ENDIF
C boost the lepton+- to the Z rest frame
      CALL LORENF(AM,P4Z,P4EL1,P4ERST)
      IF(P4ERST(4).GT.0.) THEN
        CSTHST = P4ERST(3)/P4ERST(4)
        COSCOR = VDOTN(P4ERST,P4Z,2)
        ANGCOR = ACOS(COSCOR)
      ENDIF  
C
C  fill output array
      ARESUL(1) = AM
      ARESUL(2) = ETE1
      ARESUL(3) = ETE2
      ARESUL(4) = APTZ
      ARESUL(5) = P4Z(1)
      ARESUL(6) = P4Z(2)
      ARESUL(7) = PTETA
      ARESUL(8) = PTXI
      CALL UCOPY(P4EL1,ARESUL(9),4)
      CALL UCOPY(P4EL2,ARESUL(13),4)
      CALL UCOPY(P4Z,  ARESUL(17),4)
      IF(P4Z(4)-P4Z(3).NE.0.) THEN
        YIVB = 0.5*LOG((P4Z(4)+P4Z(3))/(P4Z(4)-P4Z(3)))
        ARESUL(21) = YIVB
      ENDIF
      CALL UCOPY(P4ERST,ARESUL(22),4)
      ARESUL(26) = CSTHST
      ARESUL(27) = ANGCOR
      ARESUL(28) = APTREC
      ARESUL(29) = PTRETA
      ARESUL(30) = PTRXI
      ARESUL(31) = AMCON1
      ARESUL(32) = AMCON2
      ARESUL(33) = PCETA1
      ARESUL(34) = PCETA2
      ARESUL(35) = UPAR1
      ARESUL(36) = UPER1
      ARESUL(37) = UPAR2
      ARESUL(38) = UPER2
C
77777 RETURN
      END
