      Subroutine EMVTX(LPELC,XBAR,ZV,RV,PrbRZ,PrbXY,Prb,NVBST)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods:
C-        Calculate the cluster projection based on the logarithm of the
C-        ratio of the integral of the energy from
C-        a padedge to infinity and the total energy for each floor
C-        separately.
C-
C-   Inputs:  LPELC    = Pointer to the EM (PELC/PPHO) cluster
C-   Outputs: XBAR(4,3)= X,Y,Z-coordinates of shower center in 4 layers
C-            Zv       = Z-coordinate of the cluster projection to the beam
C-            Rv       = XY-impact parameter of the cluster projection
C-
C-   Created  19-APR-1995   Gregory L. Landsberg
C-   Updated  17-MAY-1995   Gregory L. Landsberg
C-
C----------------------------------------------------------------------
      Implicit None
      Include 'D0$PARAMS:DCPARA.DEF'
      Include 'D0$INC:ZEBCOM.INC'
      Include 'D0$INC:PI.DEF'
C
      Integer LPELC,LCACL,LCASH
      Integer ETA,PHI,LYR,I,NVBST
      Integer NHITS,IDEPTH,KDEPTH,POINTER,PACKED_WORD
      Integer ETAMX(4),PHIMX(4),ILR3MX
      Real ZPOS(5),ENDPTH(4),ETOT(4),W(5),TMP1(4),TMP2(4)
      Real ETOTAL,CELL(4),ZZ,PROB
      Real ENERGY,EMAX(4),PADEDG(4),RPOS(5),X,Y,Z,ZV,A
      Real WT,W0(4),DEPTH(4),PHILW,PHIUP,ETALW,ETAUP,DZVR(14)
      Real XBAR(4,3),RV,XPOS(5),YPOS(5),PhiC,ZVER(14)
      Real ZVTX_INFO(3,14)
      Real CHIS,SA,SZ,COR,B,BETA(4),PrbRZ,PrbXY,Prb
      Integer IOK,IDPHI,IDETA,KETA,KPHI,JDEPTH,IERR,NVER
      Logical L_EC, OKZ
      Data W0 /4., 4., 4., 4./
      Data KETA /3/, KPHI /2/
C
      ZV = 999.
      RV = 999.
      CALL VZERO(ZVER,14)
      CALL VERTEX_INFO(14,NVER,ZVTX_INFO,OKZ) ! Vertex from tracking
      IF(NVER.EQ.0 .or. (.not. OKZ)) THEN
        NVER = 1
        ZVER(1) = 0.
        DZVR(1) = 29.
      ELSE
        NVER = MIN(14,NVER)
        DO I = 1, NVER
          ZVER(I) = ZVTX_INFO(1,I)
          DZVR(I) = ZVTX_INFO(2,I)
        ENDDO
      ENDIF
C
      If (LPELC .le. 0) Then
C        Type *,'LPELC <= 0'
        Return
      End If
      If (Q(LPELC+23)**2+Q(LPELC+24)**2+Q(LPELC+25)**2 .gt. 0) Then
        ZZ = ATAN2(SQRT(Q(LPELC+23)**2+Q(LPELC+24)**2),Q(LPELC+25))
      Else
C        Type *,'Position info for cluster is wrong'
        Return
      End If
      ZZ = ABS(-ALOG(TAN(ZZ/2.)))
      If (ZZ .gt. 1.2) Then
        L_EC = .True.
        W(5) = W(5)*TAN(Q(LPELC+8))
      Else
        L_EC = .False.
      End If
C
      LCACL = LQ(LPELC-2)
      If (LCACL .le. 0) Then
C        Type *,'LCACL <= 0'
        Return
      End If
      LCASH = LQ(LCACL-2)
      If (LCASH .le. 0) Then
C        Type *,'LCASH <= 0'
        Return
      End If
C
C -- Get cluster parameters here
C
      Call CEMCLS(LCASH,ENDPTH,ETOTAL,EMAX,ETAMX,PHIMX,ILR3MX)
      Call GETPOS(ETAMX,PHIMX,ILR3MX,PADEDG,DEPTH,IOK,CELL)
      If (IOK .ne. 0) Then
C        Type *,'IOK from GETPOS: ',IOK
        Return
      End If
C
      Call VZERO(TMP1,4)
      Call VZERO(TMP2,4)
      Call VZERO(ETOT,4)
      Call VZERO(BETA,4)
      Call VZERO(ZPOS,5)
      Call VZERO(RPOS,5)
      Call VZERO(XPOS,5)
      Call VZERO(YPOS,5)
C
      NHITS = IQ(LCASH+2)
      Do 5 I = 1,NHITS
        POINTER = LCASH + 2*(I-1)
        ENERGY = Q(POINTER+4)
        If (ENERGY .le. 0.) Go To 5
        PACKED_WORD = IQ(POINTER+3)
        Call CAEP_INDICES(PACKED_WORD,ETA,PHI,LYR)
        If (LYR .gt. 7) Go To 5
        If (ABS(ETA) .gt. 12 .XOR. L_EC) Go To 5
        IDEPTH = 0
        JDEPTH = 0
C
        If (LYR.eq.1) Then
C
C---- layer 1 eta, phi
C
          If (IDPHI(PHI,PHIMX(1)).lt.KPHI .AND.
     &        IDETA(ETA,ETAMX(1)).lt.KETA) IDEPTH=1
          If (IDPHI(PHI,PHIMX(1)).lt.KETA .AND.
     &        IDETA(ETA,ETAMX(1)).lt.KPHI) JDEPTH=1
        Else If (LYR.eq.2) Then
C
C---- layer 2 eta, phi
C
          If (IDPHI(PHI,PHIMX(2)).lt.KPHI .AND.
     &        IDETA(ETA,ETAMX(2)).lt.KETA) IDEPTH=2
          If (IDPHI(PHI,PHIMX(2)).lt.KETA .AND.
     &        IDETA(ETA,ETAMX(2)).lt.KPHI) JDEPTH=2
        Else If (LYR.ge.3 .and. LYR.le.6) Then
C
C---- layer 3 eta
C
          If (ABS(ETA) .lt. 27) Then
            If (IDPHI(PHI,PHIMX(3)).lt.KPHI .AND.
     &         IDETA(ETA,ETAMX(3)).lt.KETA) THEN
              IDEPTH = 3
              PHILW = PHIMX(3)-KPHI+1
              If (PHILW .le. 0)  PHILW = PHILW + 64
              PHIUP = PHIMX(3)+KPHI-1
              If (PHIUP .gt. 64) PHIUP = PHIUP - 64
              ETALW = ETAMX(3)-KETA+1
              If (ETALW*ETA .le. 0)  ETALW = ETALW - 1
              ETAUP = ETAMX(3)+KETA-1
              If (ETAUP*ETA .le. 0)  ETAUP = ETAUP + 1
C
              If (ILR3MX .eq. 3 .or. ILR3MX .eq. 5) Then
                If (PHI .eq. PHIUP .and. (LYR .eq. 4 .or. LYR .eq. 6))
     &            IDEPTH = 0
              Else
                If (PHI .eq. PHILW .and. (LYR .eq. 3 .or. LYR .eq. 5))
     &            IDEPTH = 0
              End If
              If (ILR3MX .eq. 3 .or. ILR3MX .eq. 4) Then
                If (ETA .eq. ETAUP .and. (LYR .eq. 5 .or. LYR .eq. 6))
     &            IDEPTH = 0
              Else
                If (ETA .eq. ETALW .and. (LYR .eq. 3 .or. LYR .eq. 4))
     &            IDEPTH = 0
              End If
            Endif
            If (IDPHI(PHI,PHIMX(3)).lt.KETA .AND.
     &           IDETA(ETA,ETAMX(3)).lt.KPHI) THEN
              JDEPTH = 3
              PHILW = PHIMX(3)-KETA+1
              If (PHILW .le. 0)  PHILW = PHILW + 64
              PHIUP = PHIMX(3)+KETA-1
              If (PHIUP .gt. 64) PHIUP = PHIUP - 64
              ETALW = ETAMX(3)-KPHI+1
              If (ETALW*ETA .le. 0)  ETALW = ETALW - 1
              ETAUP = ETAMX(3)+KPHI-1
              If (ETAUP*ETA .le. 0)  ETAUP = ETAUP + 1
C
              If (ILR3MX .eq. 3 .or. ILR3MX .eq. 5) Then
                If (PHI .eq. PHIUP .and. (LYR .eq. 4 .or. LYR .eq. 6))
     &            JDEPTH = 0
              Else
                If (PHI .eq. PHILW .and. (LYR .eq. 3 .or. LYR .eq. 5))
     &            JDEPTH = 0
              End If
              If (ILR3MX .eq. 3 .or. ILR3MX .eq. 4) Then
                If (ETA .eq. ETAUP .and. (LYR .eq. 5 .or. LYR .eq. 6))
     &            JDEPTH = 0
              Else
                If (ETA .eq. ETALW .and. (LYR .eq. 3 .or. LYR .eq. 4))
     &            JDEPTH = 0
              End If
            End If
          Else
            If (IDPHI(PHI,PHIMX(4)).lt.KPHI .AND.
     &          IDETA(ETA,ETAMX(4)).lt.KETA) IDEPTH = 3
            If (IDPHI(PHI,PHIMX(4)).lt.KETA .AND.
     &          IDETA(ETA,ETAMX(4)).lt.KPHI) JDEPTH = 3
          Endif
        Else If (LYR.eq.7) Then
C
C---- layer 4 eta, phi
C
          If (IDPHI(PHI,PHIMX(4)).lt.KPHI .AND.
     &        IDETA(ETA,ETAMX(4)).lt.KETA) IDEPTH = 4
          If (IDPHI(PHI,PHIMX(4)).lt.KETA .AND.
     &        IDETA(ETA,ETAMX(4)).lt.KPHI) JDEPTH = 4
        End If
C
        If (IDEPTH + JDEPTH .ne. 0) Then
          Call CELXYZ_FAST(ETA,PHI,LYR,X,Y,Z,IOK)
          KDEPTH = MAX(IDEPTH,JDEPTH)
          If (ENDPTH(KDEPTH) .le. 0.) Then
            WT = 0.
          Else
            WT = MAX(W0(KDEPTH) + LOG(ENERGY/ENDPTH(KDEPTH)),0.)
          End If
          If (IDEPTH .ne. 0) Then
            TMP1(KDEPTH) = TMP1(KDEPTH) + WT
            ZPOS(KDEPTH) = ZPOS(KDEPTH) + Z*WT
            RPOS(KDEPTH) = RPOS(KDEPTH) + SQRT(X**2+Y**2)*WT
          End If
          If (JDEPTH .ne. 0) Then
            TMP2(KDEPTH) = TMP2(KDEPTH) + WT
            XPOS(KDEPTH) = XPOS(KDEPTH) + X*WT
            YPOS(KDEPTH) = YPOS(KDEPTH) + Y*WT
          End If
          ETOT(KDEPTH)=ETOT(KDEPTH)+ENERGY
        End If
C
    5 Continue
C
C Log (Ei/E) weighting is implemented here
C
      Do I = 4,1,-1
        If (TMP1(I) .ne. 0.) Then
          ZPOS(I) = ZPOS(I)/TMP1(I)
          RPOS(I) = RPOS(I)/TMP1(I)
        Else
          ZPOS(I) = ZPOS(I+1)
          RPOS(I) = RPOS(I+1)
        End If
        If (TMP2(I) .ne. 0.) Then
          XPOS(I) = XPOS(I)/TMP2(I)
          YPOS(I) = YPOS(I)/TMP2(I)
        Else
          XPOS(I) = XPOS(I+1)
          YPOS(I) = YPOS(I+1)
        End If
        If (YPOS(I) .eq. 0 .and. XPOS(I) .eq. 0) Then
          PhiC = PhiMX(I)
        Else
          PhiC = ATAN2(YPOS(I),XPOS(I))
        End If
        If (PhiC .lt. 0) PhiC = PhiC + TwoPi
        BETA(I) = PhiC
        If (L_EC) Then
          ZPOS(I) = DEPTH(I)
        Else
          RPOS(I) = DEPTH(I)
        End If
      End Do
C
      Do I = 2,4
        If (RPOS(I) .eq. 0.0) Then
Cc          Type *,'Error in EMVTX: RPOS, I',I,RPOS,ZZ,L_EC
          RPOS(I) = RPOS(I-1)
        End If
      End Do
      If (L_EC) Then
        If (ZZ .lt. 1.4) ZZ = 1.4
        If (ZZ .gt. 2.5) ZZ = 2.5
        BETA(1) = BETA(1) - (0.5378-0.3666*ZZ+0.5897E-1*ZZ**2)/RPOS(1)
        BETA(2) = BETA(2) - (0.5126-0.3077*ZZ+0.3972E-1*ZZ**2)/RPOS(2)
        BETA(3) = BETA(3) - (0.2491-0.1597*ZZ+0.2378E-1*ZZ**2)/RPOS(3)
        BETA(4) = BETA(4) - (0.3276-0.2421*ZZ+0.4547E-1*ZZ**2)/RPOS(4)
C
        If (ZZ .lt. 1.6) Then
          RPOS(1)=RPOS(1)-(670.53-1479.2*ZZ+1055.0*ZZ**2-245.06*ZZ**3)
        Else
          RPOS(1)=RPOS(1)-(1.765-0.58768*ZZ)
        End If
        If (ZZ .lt. 1.7) Then
          RPOS(2)=RPOS(2)-(-1610.0+2978.0*ZZ-1834.3*ZZ**2+376.37*ZZ**3)
        Else
          RPOS(2)=RPOS(2)-(5.1318-4.1415*ZZ+0.87922*ZZ**2)
        End If
        If (ZZ .lt. 1.7) Then
          RPOS(3)=RPOS(3)-(-112.23+212.28*ZZ-133.10*ZZ**2+27.744*ZZ**3)
        Else If (ZZ .lt. 2.5) Then
          RPOS(3)=RPOS(3)-(0.69239-0.23839*ZZ)
        Else If (ZZ .le. 2.65) Then
          RPOS(3)=RPOS(3)-(375.22-469.68*ZZ+195.98*ZZ**2-27.251*ZZ**3)
        End If
        If (ZZ .lt. 1.6) Then
          RPOS(4)=RPOS(4)-(-601.65+1124.2*ZZ-699.74*ZZ**2+145.07*ZZ**3)
        Else
          RPOS(4)=RPOS(4)-(-1.6540+1.3845*ZZ-0.27280*ZZ**2)
        End If
C
        Do I = 1,4
          XPOS(I) = RPOS(I)*COS(BETA(I))
          YPOS(I) = RPOS(I)*SIN(BETA(I))
        End Do
C
        W(1) = 4.0630-2.7017*ZZ+0.49937*ZZ**2
        W(2) = 5.6336-3.8975*ZZ+0.69383*ZZ**2
        W(3) = 71.727-165.23*ZZ+151.23*ZZ**2-68.659*ZZ**3+15.465*ZZ**4-
     &         1.3833*ZZ**5
        W(4) = 1.8056-1.1494*ZZ+0.19703*ZZ**2
C
        PrbRZ = 1.E10
        Do I = 1,NVER
          RPOS(5) = 0.
          ZPOS(5) = ZVER(I)
          W(5)    = DZVR(I)
          Call SLFIT(ZPOS,RPOS,W,5,A,B,CHIS,SA,SZ,COR,IERR)
          If (Chis .lt. PrbRZ) Then
            PrbRZ = CHIS
            NVBST = I
          End If
        End Do
C
        Call SLFIT(ZPOS,RPOS,W,4,A,ZV,CHIS,SA,SZ,COR,IERR)
        If (IERR .eq. 0 .and. A .ne. 0.) Then
          ZV = -ZV/A
        Else
          ZV = ZVER(1)            ! Set it ot the primary vtx if fit fails
        End If
C
        W(1) = 20.051-26.124*ZZ+11.698*ZZ**2-1.7564*ZZ**3
        W(2) = 6.4475-4.6787*ZZ+0.87584*ZZ**2
        W(3) = 15.003-26.939*ZZ+18.134*ZZ**2-5.3944*ZZ**3+0.59778*ZZ**4
        W(4) = 1.1434-0.68625*ZZ+0.11401*ZZ**2
        W(5) = 0.05
C
        Call SLFIT(XPOS,YPOS,W,5,A,B,CHIS,SA,SZ,COR,IERR)
        If (IERR .eq. 0) Then
          CHIS = CHIS/(1.+A**2)
        Else
          CHIS = 0.
        End If
        PrbXY = CHIS
C
        Call SLFIT(XPOS,YPOS,W,4,A,RV,CHIS,SA,SZ,COR,IERR)
        If (IERR .eq. 0) Then
          RV = RV/SQRT(1.+A**2)
        Else
          RV = 0.
        End If
      Else
        ZZ = Q(LPELC+25)
        If (ZZ .gt.  120.) ZZ =  120.
        If (ZZ .lt. -120.) ZZ = -120.
        ZPOS(1) = ZPOS(1)-(0.4357E-1+0.1502E-1*ZZ)
        ZPOS(2) = ZPOS(2)-(0.1383E-1+0.9459E-2*ZZ)
        ZPOS(3) = ZZ-(-0.5103E-1+0.1842E-2*ZZ-0.2484E-4*ZZ**2-
     &    0.7804E-6*ZZ**3)
        ZPOS(4) = ZPOS(4)-(-0.3257-0.2072E-1*ZZ-0.4864E-4*ZZ**2-
     &    0.2277E-5*ZZ**3)
C
        W(1) = 1.743-0.9892E-3*ZZ+0.4545E-4*ZZ**2
        W(2) = 1.704-0.1148E-2*ZZ+0.6191E-4*ZZ**2
        W(3) = 0.4502+0.2498E-3*ZZ+0.7220E-4*ZZ**2
        W(4) = 1.077+0.2463E-2*ZZ+0.1583E-3*ZZ**2
C
        PrbRZ = 1.E10
        Do I = 1,NVER
          RPOS(5) = 0.
          ZPOS(5) = ZVER(I)
          W(5)    = DZVR(I)
          Call SLFIT(RPOS,ZPOS,W,5,A,B,CHIS,SA,SZ,COR,IERR)
          If (Chis .lt. PrbRZ) Then
            PrbRZ = CHIS
            NVBST = I
          End If
        End Do
C
        Call SLFIT(RPOS,ZPOS,W,4,A,ZV,CHIS,SA,SZ,COR,IERR)
C
        BETA(1) = BETA(1) + 0.1535/RPOS(1)
        BETA(2) = BETA(2) + 0.1742/RPOS(2)
        BETA(3) = BETA(3) + 0.2022/RPOS(3)
        BETA(4) = BETA(4) + 0.2461/RPOS(4)
C
        Do I = 1,4
          XPOS(I) = RPOS(I)*COS(BETA(I))
          YPOS(I) = RPOS(I)*SIN(BETA(I))
        End Do
C
        W(1) = 1.783-0.2116E-3*ZZ-0.1216E-4*ZZ**2
        W(2) = 1.866-0.1105E-3*ZZ-0.1445E-4*ZZ**2
        W(3) = 0.4468-0.3128E-3*ZZ-0.6698E-5*ZZ**2
        W(4) = 0.8056-0.4559E-3*ZZ-0.2027E-4*ZZ**2+0.4108E-7*ZZ**3+
     &    0.2169E-8*ZZ**4
        W(5) = 0.05
C
        Call SLFIT(XPOS,YPOS,W,5,A,B,CHIS,SA,SZ,COR,IERR)
        If (IERR .eq. 0) Then
          CHIS = CHIS/(1.+A**2)
        Else
          CHIS = 0.
        End If
        PrbXY = CHIS
C
        Call SLFIT(XPOS,YPOS,W,4,A,RV,CHIS,SA,SZ,COR,IERR)
        If (IERR .eq. 0) Then
          RV = RV/SQRT(1.+A**2)
        Else
          RV = 0.
        End If
      End If
      Prb = PROB(PrbXY+PrbRZ,6)
      PrbXY = PROB(PrbXY,3)
      PrbRZ = PROB(PrbRZ,3)
C
      Do I = 1,4
        XBAR(I,1) = XPOS(I)
        XBAR(I,2) = YPOS(I)
        XBAR(I,3) = ZPOS(I)
      End Do
C
  999 Return
      End
