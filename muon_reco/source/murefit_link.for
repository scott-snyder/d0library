      SUBROUTINE MUREFIT_LINK(IERR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : update MUON bank for refit muons
C-                         (MUREFIT_MATCH and MUREFIT_MUFIT should
C-                          be called before)
C-
C-   Inputs  :
C-   Outputs :  IERR   I   error code  (always 0 for now)
C-   Controls:
C-
C-   Created  14-MAR-1993   D.Wood, based on MULINK
C-   Modified 15-MAY-1995   D.Wood, added call to MTOFFL, plus other minor
C-                          changes to shadow MULINK
C-   Modified 03-OCT-1995   D.Wood, added calorimeter vertex stuff (MUCALVTX,
C-                          MTC_MUCALVTX) controled by MTCREFIT switch
C-   Modified 16-OCT-1995   D.Wood, calculate & store muon-CD matching angles
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IERR
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:PI.DEF'
      INCLUDE 'D0$INC:MUPHYS.INC'
      INCLUDE 'D0$LINKS:IZMUCD.LINK'
      INCLUDE 'D0$LINKS:IZMFIT.LINK'
C  -- local variables...
      INTEGER MAXV
      PARAMETER(MAXV=9)
      INTEGER IVER,NN
      INTEGER LMTOF,LMFIT,LMUON
      REAL PMOM,MOM,MOMER
      REAL PHI,THETA
      REAL XC(3)
      INTEGER I,LMED,IFLG1,NTRK
      LOGICAL FIRST
      INTEGER DIRMOD,IER,NV,MUVERT,IP,IHIT,IFM
      REAL SD(5),MSA(5),DEMFIT,XM(3)
      REAL X(6), VERTEX(3), VERT(3,MAXV), ZVTX, DIRC(3),XA(3),XD(6)
      REAL EPS, XX, YY, ZZ, RR,RC,DZ,CZ
      REAL XP(3), XR, PMIN, PC(3),TANT
      INTEGER NVT,IVT,GFIT,MTCREFIT,NC,MTC_NVT
      INTEGER NS,LZTRK,LDTRK,LFTRK,LVTRK
      REAL PHI_DTRK,THE_DTRK,X_DTRK,Y_DTRK,Z_DTRK,COSMD,AMTMIN,DP,DT
      CHARACTER*40 MSG
C
      INTEGER  GZMUON
C
      EXTERNAL GZMUON
C
      DATA EPS,FIRST /1.0E-6,.TRUE./
      DATA RC, CZ, PMIN /150.0, 250.0, 0.3/
C----------------------------------------------------------------------
C
C     -- return error code...
      IERR = 0
C
      CALL EZPICK('MURECO_RCP')
      IF(FIRST) THEN
        CALL EZGET('DIRMOD',DIRMOD,IER)
        CALL EZGET('MUVERT',MUVERT,IER)
        CALL EZGET('GFIT',GFIT,IER)
        CALL EZGET('MTCREFIT',MTCREFIT,IER)
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
      IF(NV .GT. 0) THEN
        DO I=1,3
          VERTEX(I) = VERT(I,1)
        ENDDO
        NVT = 1
      ENDIF
C
C  LOOP OVER MUON TRACKS.
C  ======================
      LMUON = GZMUON(0)
C
      DO WHILE(LMUON.GT.0)
        NTRK = IQ(LMUON-5)
        LMFIT = LQ(LMUON-IZMFIT)
        IF(LMFIT.GT.0) THEN
C flags
          IFLG1 = IQ(LMFIT+6)
C
C       -- Momentum and error...
          PMOM = ABS(Q(LMFIT + 23))
          MOMER = Q(LMFIT + 28)
C IFM =  fit method, should always be 3 = copy of MUOT
          IFM = IQ(LMFIT + 7)
C energy loss used
          IF( IQ(LMFIT+4).EQ.13 .OR. IQ(LMFIT+4).EQ.14 ) THEN
            DEMFIT = 0          ! SAMUS momentum is at the vertex
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
            CALL ERRMSG(' Very low P, was reset to PMIN',
     &        'MUREST_LINK',MSG,'W')
            DO I=1,3
              PC(I) = (PMIN + DEMFIT) *
     &          Q(LMFIT + 19 + I) / (PMOM + EPS)
            ENDDO
            MOM = PMIN
            PMOM = PMIN + DEMFIT
          ENDIF
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
          PHI = ATAN2(DIRC(2), DIRC(1))
          IF(DIRC(2) .LT. 0.0) PHI = PHI + 2.0 * PI
          THETA = ACOS(DIRC(3))
C
C---  CALCULATE AND FILL MUON BANK.
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
              IF(MTCREFIT.GT.0) THEN
                NC = 1
                IF(NV .GT. 1) THEN    !more than one vertex
                  CALL MUCALVTX(XA,NV,VERT,NC,NVT)
                ENDIF
                IF (GFIT.EQ.3) THEN
                  CALL MTC_MUCALVTX(NTRK,XA,NV,VERT,NC,MTC_NVT)
                END IF
              ENDIF
C
            ELSEIF(IFLG1 .EQ. 1) THEN    ! Vertex constrained
              IVRT = 1
              XR = 0.0
              IF(MTCREFIT.GT.0) THEN
                NC = 1
                IF(NV .GT. 1) THEN    !more than one vertex
                  CALL MUCALVTX(XM,NV,VERT,NC,NVT)
                ENDIF
                IF (GFIT.EQ.3) THEN
                  CALL MTC_MUCALVTX(NTRK,XM,NV,VERT,NC,MTC_NVT)
                END IF
              ENDIF
            ENDIF
          ELSE              ! Cosmic
            IVRT = 2
            DO I=1,3
              XA(I) = X(I)
            ENDDO
            CALL MUPLNZX(0.,XA,DIRC,XC,IHIT)
            IF(IHIT .EQ. 0) THEN
              CALL MUPLNXY(0.,XA,DIRC,XC,IHIT)
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
C
C-------- FILL MUON BANK- careful...this might have to MZPUSH
C
          IVT = NVT
          IF(MTCREFIT.GT.0) THEN
            IF (GFIT.EQ.3) THEN
              IVT = MTC_NVT
            END IF
          ENDIF
          CALL MUREFIT_MUON(NTRK, MOM, X, IVT, LMUON)
C
C  Fill MTOF bank
C
          LMTOF=0
          CALL MTOFFL(NTRK,LMTOF)
C
C recalculate CD matching angles
          NS = IQ(LMUON-2)
          LZTRK = LQ(LMUON-NS-3)
          IF(LZTRK.GT.0) THEN
            LDTRK = LQ(LZTRK - 7)
            LFTRK = LQ(LZTRK - 8)
            LVTRK = LQ(LZTRK - 6)
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
              AMTMIN = ACOS(COSMD)
              DP = ABS(PHI - PHI_DTRK)
              DT = ABS(THETA - THE_DTRK)
              AGMUCD = AMTMIN / RADIAN
              IF(DP .GT. PI) DP = TWOPI - DP
              DP = DP / RADIAN
              DT = DT / RADIAN
              Q(LMUON + 40) = AGMUCD
              Q(LMUON + 41) = DP
              Q(LMUON + 42) = DT
            ENDIF
          ENDIF
C
C - Fill some leftover MUON entries
          Q(LMUON + 53) = XR        ! Muon track impact parameter
          Q(LMUON + 54) = -999.0        ! should be filled in MUFITG
C
C 1100 CONTINUE
C
        ENDIF
        LMUON = LQ(LMUON)
      ENDDO                           ! end of loop over muon banks
C
  999 CONTINUE
      CALL EZRSET
      RETURN
      END
