      SUBROUTINE WKINEM(PELECT,PTOTAL,ARESUL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C  Calculate transverse mass and other kinematic quantites
C
C inputs:  PELECT(): lepton+- 3-momentum
C          PTOTAL(): event total 3-mometum (incl lepton+-)
C
C outputs: ARESUL(1): transverse mass
C          ARESUL(2): lepton+- Et
C          ARESUL(3): missing  Et
C          ARESUL(4): W pt total
C          ARESUL(5): W ptx
C          ARESUL(6): W pty
C          ARESUL(7): W pt eta
C          ARESUL(8): W pt xi
C          ARESUL(9-12) : lepton+- 4-momentum
C          ARESUL(13-16) : neutrino 4-momentum
C          ARESUL(17-20) : W        4-momentum
C          ARESEL(21): W rapididy
C          ARESUL(22-25) : lepton+- 4-mom in W rest frame
C          ARESEL(26): cos(theta*), ele decay angle in W r.f.
C          ARESEL(27): trans angle between ele (W.r.f) and W pt
C          ARESEL(28:30): spares
C
C-
C-   Created  20-SEP-1992   Darien R. Wood
C-
C----------------------------------------------------------------------
C
      IMPLICIT NONE
C
      REAL ARESUL(30)
      REAL PELECT(3),PTOTAL(3),PNU(3)
      REAL P4ELE(4),P4NEU(4),P4W(4),P4ERST(4)
      REAL CTNEU(2)
      REAL VNELE(2),VNNU(2),VNETA(2),VNXI(2),PTW(2)
      REAL PTETA,PTXI,DTEST,PHIW,APTW
      REAL ETE,ETN,TM2,TM
      REAL AMTEST,DENOM,PTOTW
      REAL VDOT,VMOD,VDOTN
      REAL CTHELE,SN2ELE,CSALPH,SNALPH
      REAL WMASS,CSTHST,COSCOR,ANGCOR,PTERST
      REAL PLNTST,PLWTST,PLWMIN,YIVB
      INTEGER IC
C
      PARAMETER (WMASS=80.2)
C
      EXTERNAL VDOT,VMOD,VDOTN,UCOPY,VADD,VUNIT,LORENF
      EXTERNAL VZERO
C
C---------------------------------------------------------------------------
C
      CALL VZERO(ARESUL,27)
      CALL UCOPY(PELECT,P4ELE,3)
      P4ELE(4) = VMOD(P4ELE,3)
      IF(P4ELE(4).LT.0.001) GOTO 77777
C
      ETE = SQRT(PELECT(1)**2 + PELECT(2)**2)
      ETN = SQRT(PTOTAL(1)**2 + PTOTAL(2)**2)
      IF(ETE.LE.0. .OR. ETN.LE.0.) THEN
        PRINT *,' NONPOSITIVE ETE,ETN:',ETE,ETN
        TM = 0.
        GOTO 77777
      ENDIF
C     TM2 = (ETE + ETN)**2 - (PELECT(1)-PTOTAL(1))**2 -
C    + (PELECT(2)-PTOTAL(2))**2
      TM2 = 2.*(ETE*ETN+PELECT(1)*PTOTAL(1)+PELECT(2)*PTOTAL(2))
      IF(TM2.GT.0.) THEN
        TM = SQRT(TM2)
      ELSE
        PRINT *,' NEGATIVE TRANSVERSMASSSQ:',TM2
        TM = 0.
        GOTO 77777
      ENDIF
C  W pt calculation
      PNU(1) = -PTOTAL(1)
      PNU(2) = -PTOTAL(2)
      CALL UCOPY(PNU,P4NEU,3)
      PNU(3) = 0.
      CALL VADD(PELECT(1),PNU,PTW,2)
      APTW = VMOD(PTW,2)
      IF(APTW.GT.0.) THEN
        PHIW = ATAN2(PTW(2),PTW(1))
C  set up the eta-xi coordinate system
        CALL VUNIT(PELECT(1),VNELE,2)
        CALL VUNIT(PNU,VNNU,2)
        CALL VADD(VNELE,VNNU,VNETA,2)
        CALL VUNIT(VNETA,VNETA,2)
        VNXI(1) = VNETA(2)
        VNXI(2) = -VNETA(1)
        DTEST = VDOT(VNXI,VNELE,2)
        IF(DTEST.LT.0) THEN
          VNXI(1) = -VNXI(1)
          VNXI(2) = -VNXI(2)
        ENDIF
        PTETA = VDOT(PTW,VNETA,2)
        PTXI = VDOT(PTW,VNXI,2)
      ELSE
        PHIW = 0.
        PTETA = 0.
        PTXI = 0.
      ENDIF  
C  calculate longitudinal mometum of the neutrino
      CTHELE = P4ELE(3)/P4ELE(4)
      IF(TM.LE.0.) THEN
        GOTO 66666
      ELSEIF(TM.GE.WMASS) THEN
C if TM is greater than W mass, choose Pl-neu to minimize mass
        P4NEU(3) = ETN*CTHELE/SQRT(1.-CTHELE**2)
      ELSE
C otherwise, choose Pl-neu to give the correct W mass
        CSALPH = 1./(1.+((WMASS**2-TM**2)/(2.*ETE*ETN)))
        SNALPH = SQRT(1.-CSALPH**2)
        SN2ELE = 1.-CTHELE**2
        DENOM = SN2ELE + (CTHELE*CSALPH)**2
        CTNEU(1) = (CTHELE*CSALPH**2 + SNALPH*SN2ELE)/DENOM
        CTNEU(2) = (CTHELE*CSALPH**2 - SNALPH*SN2ELE)/DENOM
        PLWMIN = 315.
        DO 100 IC=1,2
          IF(ABS(CTNEU(IC)).LT.1.) THEN
            PLNTST = ETN*CTNEU(IC)/SQRT(1.-CTNEU(IC)**2)
            PLWTST = ABS(PLNTST + P4ELE(3))
C if two solutions are possible, choose the one which gives smaller Pl-W
            IF(PLWTST.LT.PLWMIN) THEN
              P4NEU(3) = PLNTST
              PLWMIN = PLWTST
            ENDIF
          ENDIF
 100    CONTINUE
      ENDIF
      P4NEU(4) = VMOD(P4NEU,3)
      CALL VADD(P4ELE,P4NEU,P4W,4)
      PTOTW = VMOD(P4W,3)
      AMTEST = SQRT(P4W(4)**2-PTOTW**2)
C boost the lepton+- to the W rest frame
      CALL LORENF(AMTEST,P4W,P4ELE,P4ERST)
      IF(P4ERST(4).GT.0.) THEN
        CSTHST = P4ERST(3)/P4ERST(4)
      ELSE
        CSTHST = 1.
      ENDIF  
      PTERST = VMOD(P4ERST,2)
      IF(APTW.GT.0. .AND. PTERST.GT.0.) THEN
        COSCOR = VDOTN(P4ERST,P4W,2)
        ANGCOR = ACOS(COSCOR)
      ELSE
        COSCOR = 1.
        ANGCOR = 0.
      ENDIF  
C     PRINT *,'PTMISS,TRANSVERSE MASS,PLNEU,PAIR MASS',
C    > ETN,TM,P4NEU(3),AMTEST
C
66666 CONTINUE
C  fill output array
      ARESUL(1) = TM
      ARESUL(2) = ETE
      ARESUL(3) = ETN
      ARESUL(4) = APTW
      ARESUL(5) = PTW(1)
      ARESUL(6) = PTW(2)
      ARESUL(7) = PTETA
      ARESUL(8) = PTXI
      CALL UCOPY(P4ELE,ARESUL(9),4)
      CALL UCOPY(P4NEU,ARESUL(13),4)
      CALL UCOPY(P4W,  ARESUL(17),4)
      YIVB = 0.5*LOG((P4W(4)+P4W(3))/(P4W(4)-P4W(3)))
      ARESUL(21) = YIVB
      CALL UCOPY(P4ERST,ARESUL(22),4)
      ARESUL(26) = CSTHST
      ARESUL(27) = ANGCOR
C
77777 RETURN
      END
