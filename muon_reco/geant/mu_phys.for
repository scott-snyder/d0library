      SUBROUTINE MU_PHYS(P,X,NN,SD,LMED,MSANG)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns average total muon dedx and error.
C-                         Includes all effects including ionization,
C-                         radiation, ... . Also returns the RMS multiple
C-                         scattering angle. Fills MUPHYS.INC
C-
C-   Inputs  :
C-              P         : Particle momentum (GeV)
C-              X(1:3)    : Starting point coordinates.
C-              X(4:6)    : Direction cosines of the particle.
C-              NN        : Array SD and MSANG dimension (Max of 5)
C-              SD(NN)    : Number of standard deviations in calculating MSA
C-              LMED      : Last medium to search (0 = stop outside detector)
C-
C-   Outputs:
C-              MSANG(NN) : Array of RMS multiple scattering angles
C-
C-   Controls:
C-
C-   Created  10-MAR-1990   SHAHRIAR ABACHI
C-   UPDATED  18-MAR-1990   SHAHRIAR ABACHI    Multiple scattering added.
C-   UPDATED  13-FEB-1992   SHAHRIAR ABACHI    Kludge for geant311 taken out
C-   UPDATED  13-DEC-1993   SHAHRIAR ABACHI    Made compatible with IBM
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER NN, LMED
      REAL P,X(6),SD(5),MSANG(5)
      INCLUDE 'D0$INC:MUPHYS.INC'
      INCLUDE 'D0$INC:D0LOG.INC'
      INCLUDE 'D0$INC:GCONST.INC'
      INCLUDE 'D0$INC:GCUNIT.INC'
      INCLUDE 'D0$INC:GCLINK.INC'
      INCLUDE 'D0$INC:GCBANK.INC'
      INCLUDE 'D0$INC:GCMATE.INC'
      INCLUDE 'D0$INC:GCTMED.INC'
      INCLUDE 'D0$INC:GCKINE.INC'
      INCLUDE 'D0$INC:GCTRAK.INC'
      INCLUDE 'D0$INC:GCVOLU.INC'
      INCLUDE 'D0$INC:GCSETS.INC'
C
      REAL MINSTEP,DE,DES,XX(6),UBUF(100)
      INTEGER I, NWBUF, IXST
      REAL TKIN,VALUE,VALUEL,VALUEP,VALUEB,VALUEN
      REAL PCUT(5),PP,P2,MSA0
      INTEGER JMA,JEL2,IP,IREP,IFLG
      REAL TDIS1,TDIS2,MSA,MSA1,MSA2
      REAL TRADL,TRADL1,TRADL2
      REAL DEJ, DPROD, THETA,MDEDX
      REAL TV,TVP,TVB,TVN
      REAL VLEN1, VLEN2, EPS, EPS2, EPS3, CRADL1, CRADL2, RSD
      INTEGER ISVOL2, ICCEC, ITN, ILYR, ISUBL, II
      CHARACTER*4 NAMS(20), MVNAM
      LOGICAL LPR,KLUGE
      CHARACTER*20 DUM
      DATA LPR, KLUGE /.FALSE., .FALSE./
      DATA MINSTEP,EPS,EPS2,EPS3 /0.1,0.1,1.0E-6,1.0E-9/
C
      AMASS = 0.105      !Muon mass
      MFLG = 2
      DO I=1,6
        XX(I) = X(I)
      ENDDO
      NUMED = 0
      IFLG = 0
      MDEDX = 0.0
      DECAL = 0.0
      DEEM = 0.0
      DEHD = 0.0
      DEMUO = 0.0
      DEDED = 0.0
      SIGDEC = 0.0
      TDIS = 0.0
      TDIS1 = 0.0
      TDIS2 = 0.0
      TRADL = 0.0
      TRADL1 = 0.0
      TRADL2 = 0.0
      CRADL = 1.0E12
      CRADL1 = 1.0E12
      CRADL2 = 1.0E12
      DR = 0.0
      MSA = 0.0
      MSA1 = 0.0
      MSA2 = 0.0
      ILYR = 0
      ISUBL = 0
      ICCEC = 0
      PP = P
      IPART = 5
      GETOT = SQRT(AMASS*AMASS + PP*PP)
      TKIN = SQRT(AMASS*AMASS + PP*PP) - AMASS
      IREP = 0
      CHARGE = -1.0
      IF(IPART .EQ. 6) CHARGE = 1.0
C
      IF(LPR) THEN
        PRINT *, ' NLEVEL     NUMED    IDTYPE     RADL     ISVOL   DES'
      ENDIF
C
  100 CONTINUE
      DE = 0.0
      DEJ = 0.0
      IF(IFLG .EQ. 0) THEN
        CALL GMEDIA(X,NUMED)
        IF(NLEVEL .GT. 0) THEN
          DO I=1,NLEVEL
            CALL UHTOC(NAMES(I),4,NAMS(I),4)
          ENDDO
        ENDIF
        IF(NUMED .EQ. 0 .OR. NUMED .EQ. LMED) IFLG = 1
        CALL GFTMED(NUMED,DUM,NMAT,ISVOL,IFIELD,FIELDM,
     &            TMAXFD,STEMAX,DEEMAX,EPSIL,STMIN,UBUF,NWBUF)
        CALL GFMATE(NMAT,DUM,A,Z,DENS,RADL,ABSL,UBUF,NWBUF)
C
C--- Calculate energy loss
C
        CALL GFTMAT(NMAT,IPART,'LOSS',1,TKIN,VALUEL,PCUT,IXST)
C        CALL GFTMAT(NMAT,IPART,'PAIR',1,TKIN,VALUEP,PCUT,IXST)
C        CALL GFTMAT(NMAT,IPART,'BREM',1,TKIN,VALUEB,PCUT,IXST)
C        CALL GFTMAT(NMAT,IPART,'MUNU',1,TKIN,VALUEN,PCUT,IXST)
C        IF(LPR .AND. IXST .EQ. 0) PRINT *, ' ENERGY LOSS NOT CALCULATE '
        VALUE = VALUEL
CC        VALUE = VALUEL + VALUEP + VALUEB + VALUEN
        TV = VALUE/DENS
C        TVP = VALUEP/DENS
C        TVB = VALUEB/DENS
C        TVN = VALUEN/DENS
        DE = DE + VALUE
C
C        JMA = LQ(JMATE - NMAT)
C        JEL2 = LQ(JMA - 2)
C
C        IF(NMAT .GT. 0) THEN
C          IF(PP .GE. 1.0 .AND. PP .LT. 10.0) THEN
C            IP = 50 + PP/1.
C            DEJ = Q(JEL2 + IP) / DENS
C          ENDIF
C          IF(PP .GE. 10.0 .AND. PP .LT. 100.0) THEN
C            IP = 60 + PP/10.
C            DEJ = Q(JEL2 + IP) / DENS
C          ENDIF
C          IF(PP .GE. 100.0 .AND. PP .LT. 1000.0) THEN
C            IP = 70 + PP/100.
C            DEJ = Q(JEL2 + IP) / DENS
C          ENDIF
C          IF(PP .GE. 1000.0 .AND. PP .LT. 10000.0) THEN
C            IP = 80 +  PP/1000.
C            DEJ = Q(JEL2 + IP) / DENS
C          ENDIF
C        ENDIF
C
        CALL GNEXT(X,SNEXT,SAFETY)
        IF(SNEXT .LT. MINSTEP) SNEXT = MINSTEP
        TDIS = TDIS + SNEXT
        SNEXT = SNEXT + MINSTEP
        DES = DE * SNEXT
C
C- To fill GCSETS block
C
        ISVOL2 = ISVOL
        ISVOL = 1
        CALL GFINDS                     ! To fill GCSETS.INC
        ISVOL = ISVOL2
C
        IF(LPR) THEN
          PRINT *, NLEVEL, NUMED, IDTYPE, RADL, ISVOL, DES
          DO I=1,NLEVEL
            PRINT *, NAMS(I)
          ENDDO
        ENDIF
C
        IF(IDTYPE .GT. 1000) THEN
          ICCEC = IDTYPE / 1000
          ITN = IDTYPE - ICCEC * 1000
          ILYR = ITN / 10
          ISUBL = MOD(ITN, 10)
        ENDIF
C
        MDEDX = MDEDX + DES
C
        MVNAM(1:4) = '    '
        DO I=1,NLEVEL
          IF(NAMS(I)(1:4) .EQ. 'MCAL' .OR.
     &        NAMS(I)(1:4) .EQ. 'MMUO') THEN
            MVNAM(1:4) = NAMS(I)(1:4)
          ENDIF
        ENDDO
C
        IF( (ILYR .GT. 17) .OR.
     &      (ILYR .EQ. 0 .AND. ISVOL .EQ. 0) ) THEN     ! dead material
          DEDED = DEDED + DES
        ENDIF
C
CC        IF(MVNAM(1:4) .EQ. 'MCAL' .AND. ILYR .LT. 18) THEN      ! CAL
        IF(MVNAM(1:4) .EQ. 'MCAL') THEN      ! CAL
          DECAL = DECAL + DES
          IF(ILYR .LT. 8) DEEM = DEEM + DES             ! EM
          IF(ILYR .GT. 10) DEHD = DEHD + DES            ! HAD
          TDIS1 = TDIS1 + SNEXT
          TRADL1 = TRADL1 + SNEXT / RADL
        ENDIF
C
        IF(MVNAM(1:4) .EQ. 'MMUO') THEN         ! muon mother volume
          DEMUO = DEMUO + DES
          TDIS2 = TDIS2 + SNEXT
          TRADL2 = TRADL2 + SNEXT / RADL
        ENDIF
C
        DO I=1,3
          X(I) = X(I) + X(I+3) * SNEXT
        ENDDO
C
        TRADL = TRADL + SNEXT / RADL
C
        P2 = PP - MDEDX / 1000.
        IF(P2 .LE. EPS .OR. PP .LT. 0.5) P2 = PP
        GETOT = SQRT(AMASS*AMASS + P2*P2)
        TKIN = SQRT(AMASS*AMASS + P2*P2) - AMASS
        GOTO 100
      ENDIF
C
      PP = P + MDEDX / 1000.
      GETOT = SQRT(AMASS*AMASS + PP*PP)
      TKIN = SQRT(AMASS*AMASS + PP*PP) - AMASS
C
C- Repeat if low energy to get correct energy-loss.
C
      IF( (P .LT. 10.0 .AND. IREP .LT. 1) .OR.
     &     (P .LT. 4.0 .AND. IREP .LT. 2) .OR.
     &     (P .LT. 1.0 .AND. IREP .LT. 3) ) THEN
        IREP = IREP + 1
        IFLG = 0
        MDEDX = 0.0
        DECAL = 0.0
        DEEM = 0.0
        DEHD = 0.0
        DEMUO = 0.0
        DEDED = 0.0
        SIGDEC = 0.0
        TDIS = 0.0
        TDIS1 = 0.0
        TDIS2 = 0.0
        TRADL = 0.0
        TRADL1 = 0.0
        TRADL2 = 0.0
        DO I=1,6
          X(I) = XX(I)
        ENDDO
C
        GOTO 100
C
      ENDIF
C
      MDEDX = MDEDX / 1000.
      DECAL = DECAL / 1000.
      DEEM = DEEM / 1000.
      DEHD = DEHD / 1000.
      DEMUO = DEMUO / 1000.
      DEDED = DEDED / 1000.
C
C - Cal part for various standard deviations
C
      IF(TRADL1 .GT. 0.0 .AND. TDIS1 .GT. 0.0) THEN
        CRADL1 = TDIS1 / TRADL1
        II = 1
   11   RSD = SD(II)
        CALL MULTSC(TDIS1, CRADL1, GETOT, PP, RSD, MSA1)
        MSANG(II) = MSA1
        IF(II .LT. NN) THEN
          II = II + 1
          GOTO 11
        ENDIF
      ENDIF
C
C - Cal part for 1 standard deviation only
C
      IF(TRADL1 .GT. 0.0 .AND. TDIS1 .GT. 0.0) THEN
        CRADL1 = TDIS1 / TRADL1
        RSD = 1.0       ! standard deviation
        CALL MULTSC(TDIS1, CRADL1, GETOT, PP, RSD, MSA1)
      ENDIF
C
C - Toroid part
C
      IF(TRADL2 .GT. 0.0 .AND. TDIS2 .GT. 0.0) THEN
        CRADL2 = TDIS2 / TRADL2
        RSD = 1.0       ! standard deviation
        CALL MULTSC(TDIS2, CRADL2, GETOT, PP, RSD, MSA2)
      ENDIF
C
C - All detector combined
C
      IF(TRADL .GT. 0.0 .AND. TDIS .GT. 0.0) THEN
        CRADL = TDIS / TRADL
        RSD = 1.0       ! standard deviation
        CALL MULTSC(TDIS, CRADL, GETOT, PP, RSD, MSA0)
        MSA = MSA0
      ENDIF
C
      TRADLCA = TRADL1
      MSACA = MSA1
      DR = MSACA * TDIS1
C
      TRADLMU = TRADL2
      MSAMU = MSA2
C
      IF(KLUGE .AND. X(6) .LT. 0.0) THEN       ! Kludge for 311
        IF(DEMUO .LT. EPS2 .AND. ABS(X(6)) .LT. 0.98) THEN
          DEMUO = 2.0 / ABS(SIN(ACOS(X(6))))
          IF(ABS(X(6)) .GT. 0.75) DEMUO = 2.5
        ENDIF
        IF(TRADLMU .LT. EPS2 .AND. ABS(X(6)) .LT. 0.98) THEN
          TRADLMU = 80.0 / ABS(SIN(ACOS(X(6))))
          IF(ABS(X(6)) .GT. 0.75) TRADLMU = 87.0
        ENDIF
      ENDIF
C
      IF(DECAL .LT. EPS2) DECAL = EPS2
      IF(DEMUO .LT. EPS2) DEMUO = EPS2
      IF(MSACA .LT. EPS3) MSACA = EPS3
      IF(MSAMU .LT. EPS3) MSAMU = EPS3
      IF(TRADLCA .LT. EPS2) TRADLCA = EPS2
      IF(TRADLMU .LT. EPS2) TRADLMU = EPS2
C
      DO I=1,6
        X(I) = XX(I)
      ENDDO
C
  999 RETURN
      END
