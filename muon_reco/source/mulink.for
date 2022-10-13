      SUBROUTINE MULINK(IERR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-
C-   Inputs  :
C-   Outputs :  IERR   I   error code  (always 0 for now)
C-   Controls:
C-
C-   Created  13-FEB-1990   Shuichi Kunori
C-   Updated  29-MAR-1990   Shahriar Abachi
C-          To connect muon track with central detector, road for muon
C-          track was calculated.
C-   Updated  18-MAY-1990   Shahriar Abachi
C-           MUON and MUCD banks filled
C-   Updated  21-MAY-1990   Shahriar Abachi
C-          MUON bank partially filled.
C-   Updated  15-June-1990 Shuichi Kunori
C-      (1)  change to subroutine from logical function.
C-   Updated  24-JUN-1990   Shahriar Abachi
C-          THETA & PHI calculation corrected.
C-   Updated  25-JUN-1990   Shahriar Abachi
C-          If momentum funny declared error and set IERR = 1
C-   Updated  01-DEC-1990   Shahriar Abachi
C-          Only good muons processed (iflg=0), Muon bank was drastically
C-          Changed and filled. MUCD bank was filled with new information.
C     D. HEDIN 1-91 change MUOT contents; use MUOT momentum error
C-   Updated  31-JAN-1991   Shahriar Abachi      MUOT dedx taken out
C-   Updated  06-APR-1991   Shahriar Abachi   Angle between MU&CD was introdued
C-   Updated  03-JUL-1991   Shahriar Abachi   Link to ZTRK added,Angle debugged
C-   Updated  10-OCT-1991   Shahriar Abachi   All MUOT muons accepted now
C-   Updated  07-NOV-1991   Shahriar Abachi   Based on new structure and MFIT
C     AK 1/92 TEMP KLUGE
C-   Updated  07-JAN-1992   Shahriar Abachi   Arguments of MUPLXY corerected
C-   Updated  31-JAN-1992   Shahriar Abachi   starting point corrected for
C-                                            cosmic events
C-   Updated  08-OCT-1992   Shahriar Abachi   Smaller cone for CD search
C-   Updated  06-JAN-1993   Shahriar Abachi   Vertex number chosen by help of
C-                                            present calorimeter hits.
C    DH 4/12/93 comment out looping over vertices with calorimeter
C-   Updated  14-APR-1993   Shahriar Abachi   DIRMOD=2 case modified
C-   Updated  19-APR-1993   Shahriar Abachi   MUCALVTX introduced
C-   Updated  19-APR-1993   Shahriar Abachi   Limit eta for ztraks call twice
C-   Updated  27-APR-1993   Shahriar Abachi   NC was set to 1
C-   Updated  12-OCT-1993   Daria Zieminska   define IFW4PRIME; use it
C-   to skip the call to ZTRAKS for bad tracks; use ref link to VERT
C-   Updated   1-MAR-1994   Acharya fill MTOF
C-   Updated  24-JAN-1995   I.Mandrichenko    SAMUS changes
C-   Updated  11-FEB-1995   M. Fortner        Remove unused variables
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IERR
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZTRLNK.INC'
      INCLUDE 'D0$INC:ZLINKA.INC'
      INCLUDE 'D0$INC:PI.DEF'
      INCLUDE 'D0$INC:MUPHYS.INC'
      INCLUDE 'D0$LINKS:IZMUCD.LINK'
C  -- local variables...
      INTEGER IVER,NVERT,GFIT
      INTEGER MXZTRK,MAXV
      PARAMETER (MXZTRK=99,MAXV=9)
      INTEGER IMFIT,IMUCA,IMUON,IZTRK(MXZTRK),IFW4PRIME,IFMAX
      INTEGER ZLINKR(250)
      INTEGER NZTR,LMFIT,LMUON,LMUCA,LMUCD,LDTRK,LFTRK,LVTRK,LVERT,LMTOF
      REAL PMOM,MOM,MOMER,T0MAX,ECALMIN
      REAL PHI,THETA
      REAL PHIMIN,PHIMAX,THEMIN,THEMAX,PT,XC(3),CIMP(3),RIMP
      INTEGER GZMFIT,NC,IVT
      INTEGER I,LMED,IFLG1,IFLG2,NTRK,NTRK1,NTRK2,NN
      LOGICAL FIRST,CALLCD
      INTEGER DIRMOD,IER,IS,IS2,IJ,IK,NV,NS,MUVERT,IP,IHIT,IFM
      REAL ETA_ZTR,SD(5),MSA(5),DEMFIT,XM(3)
      REAL X(6), VERTEX(3), VERT(3,MAXV), ZVTX, DIRC(3),XA(3),XD(6)
      REAL THE_DTRK,PHI_DTRK,X_DTRK,Y_DTRK,Z_DTRK,COSMD
      REAL EPS, XX, YY, ZZ, RR,AMT,AMTMIN,RC,DZ,CZ
      REAL XP(3), XR, PMIN, DELT_R, DP, DT, PC(3),TANT,TEMP(14)
      INTEGER IVTRY, NVT,MTC_NVT
      CHARACTER*40 MSG
      DATA EPS,FIRST /1.0E-5,.TRUE./
      DATA RC, CZ, PMIN /150.0, 250.0, 0.3/
C----------------------------------------------------------------------
C
C     -- return error code...
      IERR = 0
      NTRK = 0
      NTRK1 = 0
      NTRK2 = 0
      IK = 0
C
      CALL EZPICK('MURECO_RCP')
      IF(FIRST) THEN
        CALL EZGET('ETA_ZTR',ETA_ZTR,IER)
        CALL EZGET_i('DIRMOD',DIRMOD,IER)
        CALL EZGET_i('MUVERT',MUVERT,IER)
        CALL EZGET_i('IFMAX',IFMAX,IER)
        CALL EZGET('T0MAX',T0MAX,IER)
        CALL EZGET('ECALMIN',ECALMIN,IER)
        CALL EZGET_i('GFIT',GFIT,IER)
        FIRST = .FALSE.
      ENDIF
C
C    -- Get primary vertex...
C
      DO I=1,3
        VERTEX(I) = 0.0
      ENDDO
      NVT = 0
      IVER = -1
      CALL MVERXYZ(IVER,MAXV,VERT,NV)
      IF(NV .EQ. 1) THEN
        DO I=1,3
          VERTEX(I) = VERT(I,1)
        ENDDO
        NVT = 1
      ENDIF
C
C  LOOP OVER MFIT TRACKS.
C  ======================
      NTRK = 1
      LMFIT = GZMFIT(NTRK)
C
 1000 CONTINUE
      IF(LMFIT .LE. 0) GO TO 999
      IJ = 0
      AGMUCD = 270.0
      NMTR = 0
      DP = 270.0
      DT = 270.0
      XR = -999.0
      DELT_R = 0.0
      CALL GRLINK('MULINK', IMFIT)
      LRLINK(IMFIT) = LMFIT
      NTRK2 = 0
C
C       -- Momentum and error...
      PMOM = ABS(Q(LMFIT + 23))
      MOMER = Q(LMFIT + 28)
      IFM = IQ(LMFIT + 7)
      IF( IQ(LMFIT+4).EQ.13 .OR. IQ(LMFIT+4).EQ.14 ) THEN
	DEMFIT = 0	    ! SAMUS momentum is at the vertex
      ELSE IF(IFM .EQ. 3) THEN
        DEMFIT = ABS(Q(LMFIT + 32) + Q(LMFIT + 33) / 2.0)
      ELSE
        DEMFIT = ABS(Q(LMFIT + 32) + Q(LMFIT + 33))
      ENDIF
      MOM = PMOM - DEMFIT
      DO I=1,3
        PC(I) = Q(LMFIT + 19 + I)     ! Momentum components
      ENDDO
      IF(MOM .LT. PMIN) THEN             ! Temp.
        WRITE(MSG,'(''P = '',F12.3)') MOM
        CALL ERRMSG(' Very low P, was reset to PMIN','MULINK',MSG,'W')
        DO I=1,3
          PC(I) = (PMIN + DEMFIT) * Q(LMFIT + 19 + I) / (PMOM + EPS)
        ENDDO
        MOM = PMIN
        PMOM = PMIN + DEMFIT
      ENDIF
C
C     -- Check quality
C
      IFLG1 = IQ(LMFIT+6)
      IFLG2 = IQ(LMFIT+5)
C
C     -- Get MFIT track information ...
C
      DO I=1,3
        XA(I) = Q(LMFIT + 10 + I)       ! Track origin
        XM(I) = Q(LMFIT + 13 + I)       ! center of magnet
      ENDDO
C
      DO I=1,3
        DIRC(I) = PC(I) / PMOM
      ENDDO
C
      IVTRY = 0
 1001 CONTINUE
      PHI = ATAN2(DIRC(2), DIRC(1))
      IF(DIRC(2) .LT. 0.0) PHI = PHI + 2.0 * PI
      THETA = ACOS(DIRC(3))
C
C---  CALCULATE AND FILL MUON AND MUCD BANK.
C
      DO I=1,3
        X(I) = VERTEX(I)
      ENDDO
      ZVTX = VERTEX(3)
C
      IF(MUVERT .NE. 1) THEN      ! Central (no cosmic)
        IF(IFLG1 .EQ. 2) THEN     ! A layer used
          IVRT = 0
          IP = -1               ! Point outside cal
          IF(THETA .GT. PI/4.0 .AND. THETA .LT. 3.0*PI/4.0) THEN
            CALL MUCYL(RC,DIRC,XA,IP,XC,IHIT)
          ELSE
            IF(THETA .LE. PI/4.0) THEN
              DZ = CZ
            ELSE
              DZ = - CZ
            ENDIF
            CALL MUPLNXY(DZ,XA,DIRC,XC,IHIT)
          ENDIF
          DO I=1,3
            XD(I) = XA(I)
            XD(I + 3) = DIRC(I)
          ENDDO
          CALL MUIMPP(VERTEX,XD,XP,XR)
          IF(DIRMOD .EQ. 2) THEN
            XX = XC(1) - X(1)
            YY = XC(2) - X(2)
            ZZ = XC(3) - X(3)
            RR = SQRT (XX**2 + YY**2 + ZZ**2)
            PHI = ATAN2(YY, XX)
            IF(YY .LT. 0.0) PHI = PHI + 2.0 * PI
            THETA = ACOS(ZZ / RR)
          ELSEIF(DIRMOD .EQ. 1) THEN
            XX = XA(1) - X(1)
            YY = XA(2) - X(2)
            ZZ = XA(3) - X(3)
            RR = SQRT (XX**2 + YY**2 + ZZ**2)
            PHI = ATAN2(YY, XX)
            IF(YY .LT. 0.0) PHI = PHI + 2.0 * PI
            THETA = ACOS(ZZ / RR)
          ENDIF
            NC = 1
          IF(NV .GT. 1) THEN    !more than one vertex
C            CALL MUCALVTX(XM,NV,VERT,NC,NVT)
            CALL MUCALVTX(XA,NV,VERT,NC,NVT)
          ENDIF
          IF (GFIT.EQ.3) THEN
            CALL MTC_MUCALVTX(NTRK,XA,NV,VERT,NC,MTC_NVT)
          END IF
C
        ELSEIF(IFLG1 .EQ. 1) THEN    ! Vertex constrained
          IVRT = 1
          XR = 0.0
          IF(NV .GT. 1) THEN    !more than one vertex
            NC = 1
            CALL MUCALVTX(XM,NV,VERT,NC,NVT)
          ENDIF
          IF (GFIT.EQ.3) THEN
            CALL MTC_MUCALVTX(NTRK,XM,NV,VERT,NC,MTC_NVT)
          END IF
        ENDIF
      ELSE              ! Cosmic
        IVRT = 2
        DO I=1,3
          XA(I) = X(I)
        ENDDO
        CALL MUPLNZX(0.,XA,DIRC,XC,IHIT)
        IF(IHIT .EQ. 0) THEN
          CALL MUPLNXY(0.,XA,DIRC,XC,IHIT)
          IF(IHIT .EQ. 0) GOTO 1101
        ENDIF
        DO I=1,3
          X(I) = XC(I)
        ENDDO
      ENDIF
C
      MUTHE = THETA
      IF(THETA .LT. EPS) THETA = THETA + EPS
      IF(THETA .GT. 3.1) THETA = THETA - EPS
      TANT = TAN(THETA/2.0)
      IF(TANT .GT. 0.0) THEN
        MUETA = -ALOG(TANT)
      ELSE
        MUETA = 99.0
      ENDIF
      MUPHI = PHI
C
      X(4) = SIN(THETA) * COS(PHI)
      X(5) = SIN(THETA) * SIN(PHI)
      X(6) = COS(THETA)
C
C ------- Calculate muon physical parameters.
C
      LMED = 0
      SD(1) = 1.0
      SD(2) = 2.0
      SD(3) = 3.0
      SD(4) = 4.0
      SD(5) = 5.0
      NN = 5
      CALL MU_PHYS(MOM,X,NN,SD,LMED,MSA)
C      IVT = - NVT
      IVT = NVT
      IF (GFIT.EQ.3) THEN
        IVT = MTC_NVT
      END IF
C
C-------- FILL MUON BANK
C
      CALL MUONFL(NTRK, MOM, X, IVT, LMUON)
      NTRK1 = NTRK1 + 1
      CALL GRLINK('MULINK', IMUON)
      LRLINK(IMUON) = LMUON
      LMFIT = LRLINK(IMFIT)
C
C  Fill MTOF bank
C
      LMTOF=0
      CALL MTOFFL(NTRK,LMTOF)
C
C-------- FILL MUCA BANK
C
      CALL MUCAFL(NTRK1, LMUCA)
      CALL GRLINK('MULINK', IMUCA)
      LRLINK(IMUCA) = LMUCA
      LMUON = LRLINK(IMUON)
      LMFIT = LRLINK(IMFIT)
C
C-------- FILL MUCD BANK
C
      PT = MOM * SQRT(X(4)**2 + X(5)**2)
C
      NZTR = 0
      NMTR = -1
C
C  Define IFW4PRIME
C
      IFW4PRIME=IQ(LMUON+6)
      IF (Q(LMUON+46).GT.T0MAX) THEN
        IFW4PRIME=IFW4PRIME+1
      END IF
      IF (Q(LMUON+43).LT.ECALMIN) THEN
        IFW4PRIME=IFW4PRIME+1
      END IF
C
C  Use ref link in MUON
C
      LVERT=LQ(LMUON-12)
      IF (LVERT.GT.0) THEN
        NVERT=IQ(LVERT-5)
        CALL GTVERT(NVERT,TEMP)
        VERTEX(1)=TEMP(3)
        VERTEX(2)=TEMP(4)
        VERTEX(3)=TEMP(5)
        NVT=NVERT
        ZVTX = VERTEX(3)
      END IF
      CALLCD=.TRUE.
      IF (ABS(MUETA) .GT. ETA_ZTR) CALLCD=.FALSE.
      IF (IFW4PRIME.GT.IFMAX) THEN
        CALLCD=.FALSE.
      END IF
      IF (LVERT.EQ.0) CALLCD=.FALSE.
      IF (CALLCD) THEN
        IF(ABS(MUETA) .LT. 1.5) THEN
          IS2 = 2
        ELSE
          IS2 = 1
        ENDIF
        IS = IS2
   11   CALL MURD(IS,SD,MSA,MOM,X,MOMER,PHI,THETA,PHIMIN,PHIMAX,
     &             THEMIN,THEMAX,DELT_R)
        CALL ZTRAKS(ZVTX,PHIMIN,PHIMAX,THEMIN,THEMAX,PT,NZTR,ZLINKR)
        IF(NZTR .LT. 1) THEN
          IS = IS + IS2
          IF (IS .LE. 2*IS2 .AND. ABS(MUETA) .LT. 1.8) GOTO 11
          IS = IS - IS2
        ELSE
          AMTMIN = 999.0
          DO I=1,NZTR
            LDTRK = LQ(ZLINKR(I) - 7)
            LFTRK = LQ(ZLINKR(I) - 8)
            LVTRK = LQ(ZLINKR(I) - 6)
            IF(LDTRK .GT. 0 .OR. LFTRK .GT. 0 .OR. LVTRK .GT. 0) THEN
              IF(LFTRK .GT. 0) THEN
                PHI_DTRK = Q(LFTRK + 6)
                THE_DTRK = Q(LFTRK + 22)
              ENDIF
              IF(LDTRK .GT. 0) THEN
                PHI_DTRK = Q(LDTRK + 6)
                THE_DTRK = Q(LDTRK + 9)
              ENDIF
              IF(LVTRK .GT. 0) THEN
                PHI_DTRK = Q(LVTRK + 6)
                IF(LFTRK .LE. 0 .AND. LDTRK .LE. 0) THEN
                  THE_DTRK = Q(LVTRK + 9)
                ENDIF
              ENDIF
              X_DTRK = SIN(THE_DTRK) * COS(PHI_DTRK)
              Y_DTRK = SIN(THE_DTRK) * SIN(PHI_DTRK)
              Z_DTRK = COS(THE_DTRK)
              COSMD = X_DTRK * X(4) + Y_DTRK * X(5) + Z_DTRK * X(6)
              IF(COSMD .GT. 1.0) COSMD = 1.0
              IF(COSMD .LT. -1.0) COSMD = -1.0
              AMT = ACOS(COSMD)
              IF(AMT .LT. AMTMIN) THEN
                AMTMIN = AMT
                DP = ABS(PHI - PHI_DTRK)
                DT = ABS(THETA - THE_DTRK)
                CALL MUIMPP(VERTEX,X,CIMP,RIMP)
                IJ = I
              ENDIF
            ENDIF
          ENDDO
          AGMUCD = AMTMIN / RADIAN
          IF(DP .GT. PI) DP = TWOPI - DP
          DP = DP / RADIAN
          DT = DT / RADIAN
        ENDIF
        NMTR = NZTR
      ENDIF
C
C - Fill some leftover MUON entries
C
      LMUON = LRLINK(IMUON)
      IQ(LMUON + 5) = NMTR
      Q(LMUON + 40) = AGMUCD
      Q(LMUON + 41) = DP
      Q(LMUON + 42) = DT
      Q(LMUON + 53) = XR        ! Muon track impact parameter
      Q(LMUON + 54) = -999.0        ! should be filled in MUFITG
      Q(LMUON + 23) = DELT_R
C
C  --  save ZTRAKs...
C
      DO I=1,NZTR
        IF(NZTR.GT.0 .AND. I.LE.MXZTRK) THEN
          NTRK2 = NTRK2 + 1
          CALL GSLINK('MULINK',IZTRK(I))
          LSLINK(IZTRK(I)) = ZLINKR(I)
          IF(I .EQ. IJ) THEN
            LMUON = LRLINK(IMUON)
            NS = IQ(LMUON - 2)
            LQ(LMUON - NS - 3) = LSLINK(IZTRK(IJ))   ! Make a link in MUON
          ENDIF
        ENDIF
      ENDDO
C
C - Fit with CAl cells
C
C      IF(.FALSE.) THEN
C        IF(Q(LMUON+33) .GT. -2.0) THEN
C          MCFLG = 0
C        ELSEIF(Q(LMUON+35) .LT. 4.0) THEN
C          MCFLG = 1
C        ELSE
C          MCFLG = 2
C        ENDIF
C        CALL MUCAFIT(MCFLG,IMUCA,IMUON)
C      ENDIF
C
      DO I=1,NTRK2
        CALL MUCDFL(IMUON, IZTRK(I), LMUCD)
        CALL ZTFLAG(LSLINK(IZTRK(I)), 'MUO')
      ENDDO
C
 1100 CONTINUE
C
      DO I=1,NTRK2
        CALL RSLINK('MULINK', IZTRK(I))
      ENDDO
      CALL RRLINK('MULINK', IMUON)
      CALL RRLINK('MULINK', IMUCA)
C
 1101 LMFIT = LRLINK(IMFIT)
      NTRK = NTRK + 1
      LMFIT = GZMFIT(NTRK)
      CALL RRLINK('MULINK', IMFIT)
      GOTO 1000
C
  999 CONTINUE
      CALL EZRSET
      RETURN
      END
