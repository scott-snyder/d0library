C+
      SUBROUTINE SSWTCP (DIR, NSAMIN, NPTOT, NTRK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Find tracks in space using SAMUS hit definition
C-                         as a cilynder in space and WAMUS drift and pads
C-
C-
C-   Inputs  : DIR - direction (N=1,S=2)
C-             NSAMIN - minimum number of SAMUS hits on track
C-             NTRK - track number.
C-   Outputs : none.
C-   Controls: none.
C-
C-   Based on Efimov's SATC2L
C-
C   1-JUL-1994 created      Joao de Mello, Neto
C  14-JUL-1994 updated       
C- 24-FEB-1995 updated      Andre Sznajder ( bug fixing and clean up) 
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INTEGER DIR, NSAMIN, NPTOT, NTRK
      INTEGER NPL
      PARAMETER (NPL=12)
      INTEGER LMTRH, LSAMT, LSAHS
      INTEGER GZMTRH, GZSAMT, GZSAHS, GZSATW, GZSTNA, GZSTSA
      INTEGER  HITS, NHIT, LINK
      REAL    ZMIN, ZMAX, ROAD 
      INTEGER MONITOR, HADR
C>>
      REAL X(12,2),Y(12,2),Z(12)
      INTEGER IH_BASE_BEST(12), IH_TRACK_BEST(12), WMON, WMON_BEST
      INTEGER WMON_TRACK_BEST
      REAL WROAD2
      COMMON /CSSWFCN1/ ZMIN, ZMAX, ROAD,
     &        MONITOR, HADR(3,NPL), IH_TRACK_BEST,X,Y,Z, WMON,
     &        WROAD2
C>> 
      INTEGER N_PAR
      PARAMETER (N_PAR=4)
      REAL    PAR(N_PAR), STEP_PAR(N_PAR), FUN(100)
      INTEGER PL1, PL2, LD1, LD2
      INTEGER JC1, JC2, N1, N2
      REAL    DS1, DS2, VX1, VX2, VY1, VY2, CX, CY
      REAL    LINE1(6), LINE2(6)
      INTEGER LD, JC, JTR, LT, PL, I, J, M
      REAL    LINE(6), DIST, ZZ, W, W1, W2, XI2, XI2_BEST
      INTEGER MON, MON_OLD, OK, NEXT
      INTEGER SW_MON_BEST, MON_BEST, HADR_BEST(3,NPL)
      REAL    LINE_BASE_BEST(6), LINE_TRACK_BEST(6)
      EXTERNAL SSWFCN1
      LOGICAL FIRST
      SAVE    FIRST
      DATA    FIRST /.TRUE./
C>>>
      REAL   BIGNUM 
      PARAMETER (BIGNUM = 1.0E+13)
      REAL   VERN
      PARAMETER (VERN = 60.96)
      INTEGER LPWAREA,LPWTRK,LP,IW1,IHIT(12)
      INTEGER LPWOTRK,LPF
      INTEGER IWMON,IWMON1,IPOS,IPOS1
      INTEGER LMUOH, LPMUOH, GZMUOH
      INTEGER ICELLHIT, IDRIFT, IWAVE, IPSOL, NWPOINTS
      INTEGER NWA, IH_BEST(12), IPOS1_BEST
      INTEGER NHTMX, NMAXROADS, NMAXHIT, NWTRY, NWAMIN, NMWPL
      INTEGER IWHICHSOL(12,2,4), ORENT
C>>
      REAL DRIFT, TDIV, PAD1, PAD2, WLEN, POINT(3), XC, YC
      REAL PS(2), WCHIS_BEST, WCHIS, DISTPL2, DISTPL2_BEST
      REAL DIST1, DIST2,SIGN, LINEWR(6)
      COMMON /LINEWR/ LINEWR
C----------------------------------------------------------------------
C
C ****  Initialyzing
C
      IF (FIRST) THEN
        NMAXROADS = 24
        NMAXHIT   = 24
        NWTRY = 1
        NMWPL = 12
        ROAD = 3.0            ! samus road after magnet
        WROAD2 = 10.0*10.0    ! wamus road**2 after magnet
        FIRST = .FALSE.
      END IF
C
      LMTRH = GZMTRH()
      LSAMT = GZSAMT()
      LSAHS = GZSAHS()
      HITS = 1 + GZSATW()
      IF (DIR .EQ. 1) THEN
        JTR = 1 + GZSTNA()
      ELSE
        JTR = 1 + GZSTSA()
      END IF
      LT = JTR + (NTRK - 1) * 64
      IQ(LT+33) = 0
      LINK = HITS + NPL
      NHIT = LINK + NPL
      NHTMX = 100                    !   set up WAMUS work area pointers 
      LPWAREA = 1 + GZSATW() + 2*NHTMX*NPL + NPL + 33
      LPWTRK = LPWAREA + 1 + 4*NMAXROADS + 7*NMAXROADS*NMAXHIT
      NHTMX = 24
      IF (NSAMIN .LT. 2) GO TO 999
      MON_BEST = 0            ! initialize variables with info about best track
      SW_MON_BEST = 0
      XI2_BEST = BIGNUM
      DO IW1 = 1, NMWPL
         IH_TRACK_BEST(IW1)=0
      END DO
      LP = LPWTRK + 1 + (NTRK-1)*(NMWPL+1) ! get points assoc. with this track
      WMON = IQ(LP)               ! get # hits in WAMUS there are in this track
      IF (WMON.LT.2) RETURN
      IWMON = 0
      DO IW1 = 1,NMWPL            ! loop over all wamus points on this track 
        LP = LP + 1               
        IF (IQ(LP).GT.0) THEN     ! check if this plane had a hit
          IWMON = IWMON + 1       ! update the hit counter
          IHIT(IWMON) = IQ(LP)    ! address in MUOH
          LMUOH=GZMUOH(0)         ! setup MUOH variables
          LPMUOH = LMUOH + 28*(IHIT(IWMON)-1)
          ICELLHIT = 1            ! ICELLHIT can be 1 or 2
          DRIFT=Q(LPMUOH+14+ICELLHIT)
          TDIV=Q(LPMUOH+16+ICELLHIT)
          PAD1=Q(LPMUOH+19)
          PAD2=Q(LPMUOH+20)
          ORENT=IABS(IQ(LPMUOH+5))
          WLEN=Q(LPMUOH+24)
          YC = Q(LPMUOH+22)
          XC = Q(LPMUOH+21)
          POINT(3) = Q(LPMUOH+23) ! Z of the chamber is the same for all X,Y    
          NWA = NINT((TDIV+WLEN/2)/VERN) ! # wavelenghts ( from bottom to TDIV) 
          PS(1) = PAD1
          PS(2) = PAD2
          DIST1 = BIGNUM
          DIST2 = 2.0*BIGNUM
          DO IDRIFT = 1, 2             ! loop over all possible drift solutions
            DRIFT= -DRIFT              !  first solution is negative
            DO IWAVE = -NWTRY,+NWTRY   !  loop over all possible pad solutions
              DO IPSOL = 1, 2
                IF(ORENT.EQ.3) THEN
                  POINT(2)=FLOAT(NWA+IWAVE)*VERN + YC + PS(IPSOL)
                  POINT(1)=XC + DRIFT
                ELSE
                  POINT(1)=FLOAT(NWA+IWAVE)*VERN + XC + PS(IPSOL)
                  POINT(2)=YC + DRIFT
                ENDIF
                CALL SADSPL(POINT,LINEWR,DISTPL2,W) 
                DIST = SQRT(DISTPL2) 
                IF (DIST.LE.DIST1) THEN   ! check the distance point to line
                   X(IWMON,1) = POINT(1) 
                   Y(IWMON,1) = POINT(2) 
                   Z(IWMON) = POINT(3)
                   DIST1 = DIST
                   IPOS = 1
                ENDIF
                IF (DIST.LT.DIST2.AND.DIST.GT.DIST1) THEN           
                   X(IWMON,2) = POINT(1) 
                   Y(IWMON,2) = POINT(2) 
                   Z(IWMON) = POINT(3)
                   DIST2 = DIST
                   IPOS = 2
                ENDIF                   
                IWHICHSOL(IWMON,IPOS,1) = IDRIFT    !  save solutions 
                IWHICHSOL(IWMON,IPOS,2) = NWA-IWAVE
                IWHICHSOL(IWMON,IPOS,3) = IPSOL
              END DO
            END DO
          END DO
        END IF
      END DO                           !     end of WAMUS hits preparation
C
C ****  Loop on base planes
C
      DO 101 PL1 = 1, 8
      IF (IQ(NHIT+PL1) .EQ. 0) GO TO 101
      LD1 = IQ(HITS+PL1)
      JC1 = IQ(LD1+3)
      HADR(1,1) = PL1
      HADR(2,1) = JC1
      HADR(3,1) = LD1
      DS1 = Q(LD1+4)
      CX = C(JC1+4)
      CY = C(JC1+5)
      W = DS1 / SQRT (CX * CX + CY * CY)
      VX1 = - CY * W
      VY1 = + CX * W
C
      DO 102 PL2 = 5, 12
      IF (PL1 .GE. 5 .AND. PL2 .LE. 8) GO TO 102
      IF (IQ(NHIT+PL2) .EQ. 0) GO TO 102
      LD2 = IQ(HITS+PL2)
      JC2 = IQ(LD2+3)
      HADR(1,2) = PL2
      HADR(2,2) = JC2
      HADR(3,2) = LD2
      DS2 = Q(LD2+4)
      CX = C(JC2+4)
      CY = C(JC2+5)
      W = DS2 / SQRT (CX * CX + CY * CY)
      VX2 = - CY * W
      VY2 = + CX * W
C
C ****  Loop on points in base planes
C
      DO 201 N1 = 1, 2
      DO I = 1, 6
        LINE1(I) = C(JC1+I)    ! tube axis parameters
      END DO
      IF (N1 .EQ. 1) THEN
        LINE1(1) = LINE1(1) + VX1
        LINE1(2) = LINE1(2) + VY1
      ELSE
        LINE1(1) = LINE1(1) - VX1
        LINE1(2) = LINE1(2) - VY1
      END IF
C
      DO 202 N2 = 1, 2
      DO I = 1, 6
        LINE2(I) = C(JC2+I)    ! tube axis parameters
      END DO
      IF (N2 .EQ. 1) THEN
        LINE2(1) = LINE2(1) + VX2
        LINE2(2) = LINE2(2) + VY2
      ELSE
        LINE2(1) = LINE2(1) - VX2
        LINE2(2) = LINE2(2) - VY2
      END IF
C
C *** Find parameters of the track passing through 2 base points in SAMUS
C     and one base point in WAMUS
C
      WMON_BEST = 0     ! initialize flags for best base point in WAMUS
      WCHIS_BEST = BIGNUM
      DO IW1 = 1, NMWPL
         IH_BASE_BEST(IW1)=0
      END DO
      DO 400  IWMON = 1, WMON   ! loop over WAMUS planes and their points 
      DO 401 IPOS = 1, 2
      POINT(1) = X(IWMON,IPOS)
      POINT(2) = Y(IWMON,IPOS)
      POINT(3) = Z(IWMON)
C
      CALL SACRPL (POINT, LINE1, LINE2, LINE, OK)
      IF (OK .LE. 0) GO TO 401
      CALL SADS2L (LINE, LINE1, DIST, W1, W2, OK)
      IF (OK .LE. 0 .OR. ABS(W2) .GT. C(JC1+7)) GO TO 401
      CALL SADS2L (LINE, LINE2, DIST, W1, W2, OK)
      IF (OK .LE. 0 .OR. ABS(W2) .GT. C(JC2+7)) GO TO 401
C
C ****  Find all SAMUS hits on this track for those base points
C
      MON = 2
      XI2 = 0.0
      DO 301 PL = 1, NPL
        IF (MON+NPL-PL+1 .LT. NSAMIN) GO TO 401
        IF (PL .EQ. PL1 .OR. PL .EQ. PL2) GO TO 301
        IF (IQ(NHIT+PL) .EQ. 0) GO TO 301
        LD = IQ(HITS+PL)
        JC = IQ(LD+3)
        CALL SADS2L (LINE, C(JC+1), DIST, W1, W2, OK)
        IF (OK .GT. 0 .AND. ABS(W2) .LE. C(JC+7)) THEN
          W = ABS (SQRT(DIST) - Q(LD+4))
          IF (W .LT. ROAD) THEN
            MON = MON + 1
            HADR(1,MON) = PL
            HADR(2,MON) = JC
            HADR(3,MON) = LD
            XI2 = XI2 + W * W
          END IF
        END IF
  301 CONTINUE
      IF (MON .LT. NSAMIN) GO TO 401   ! check SAMUS # hits
      IF (MON .GT. 2) XI2 = XI2 / (REAL(MON-2)*road**2) ! get SAMUS chisq
C
C *** Finds WAMUS hits on this track candidate
C
      NWAMIN = 2           ! min # WAMUS hit on track ( including base points )
      DO IWMON1=1,WMON     ! initialize pointer for best solution in each plane
        IH_BEST(IWMON1)= 0
      END DO
      NWPOINTS = 1
      WCHIS = 0
      DO IWMON1=1,WMON              ! loop over all WAMUS points
       IF (IWMON1.NE.IWMON) THEN    ! exclude the base point 
         DISTPL2_BEST = BIGNUM
         IPOS1_BEST = 0
         DO IPOS1=1, 2
           POINT(1) = X(IWMON1,IPOS1)
           POINT(2) = Y(IWMON1,IPOS1)
           POINT(3) = Z(IWMON1)
           CALL SADSPL(POINT,LINE,DISTPL2,W)
           IF (DISTPL2.LT.WROAD2) THEN
              IF (DISTPL2.LT.DISTPL2_BEST) THEN
                DISTPL2_BEST = DISTPL2
                IPOS1_BEST  = IPOS1
              END IF
           END IF
         END DO   ! end loop over points in one plane
         IF (IPOS1_BEST.GT.0) THEN  ! selects the closest points to the track 
           WCHIS = WCHIS + DISTPL2_BEST  ! calculate WAMUS chisq
           NWPOINTS = NWPOINTS + 1       ! count # WAMUS hits on this track
           IH_BEST(IWMON1)= IPOS1_BEST   ! save this track pointer
         END IF
       ELSE
         IH_BEST(IWMON) = IPOS      ! save the base point as a hit on track
       ENDIF
      END DO
      IF (NWPOINTS.LE.1) GOTO 401  ! selects tracks with at least 2 WAMUS hits
      IF (NWPOINTS.LT.WMON_BEST) GOTO 401 ! save solution with max. # hits 
      WCHIS  = WCHIS/(FLOAT(NWPOINTS-1)*WROAD2) ! save solution with min. chisq
      IF (NWPOINTS.EQ.WMON_BEST.AND.WCHIS.GE.WCHIS_BEST) GO TO 401
      WMON_BEST = NWPOINTS
      WCHIS_BEST = WCHIS
      DO IWMON1 = 1, WMON !  save information of the new best base point
        IH_BASE_BEST(IWMON1)=IH_BEST(IWMON1)
      END DO
      DO I=1,6
       LINE_BASE_BEST(I) = LINE(I)
      END DO
  401 CONTINUE
  400 CONTINUE
      IF (WMON_BEST.LT.NWAMIN) GO TO 202
C
C ****  search for the best track (SAMUS+WAMUS)
C
      IF ((MON+WMON_BEST).LT.SW_MON_BEST) GO TO 202
      IF ((MON+WMON_BEST).EQ.SW_MON_BEST .AND.
     &    (WCHIS_BEST+XI2) .GE. XI2_BEST) GO TO 202
      SW_MON_BEST = MON+WMON_BEST
      XI2_BEST = WCHIS_BEST+XI2
      WMON_TRACK_BEST = WMON_BEST  ! save WAMUS info
      DO IWMON1 = 1, WMON
         IH_TRACK_BEST(IWMON1)=IH_BASE_BEST(IWMON1)
      END DO
      MON_BEST=MON                 ! save SAMUS info
      DO I = 1, 6
        LINE_TRACK_BEST(I) = LINE_BASE_BEST(I)
      END DO
      DO J = 1, MON
        DO I = 1, 3
          HADR_BEST(I,J) = HADR(I,J)
        END DO
      END DO
      IF (SW_MON_BEST .EQ. WMON+NPTOT) GO TO 500
C
C ****  End of loops
C
  202 CONTINUE
  201 CONTINUE
  102 CONTINUE
  101 CONTINUE
C
C ****  Fit all hits
C
  500 CONTINUE
      IF (WMON_TRACK_BEST.LT.NWAMIN) GO TO 999
      IF (SW_MON_BEST .LT. (NSAMIN+NWAMIN)) GO TO 999
      MON = MON_BEST
      DO I = 1, 6
        LINE(I) = LINE_TRACK_BEST(I)
      END DO
      DO J = 1, MON
        DO I = 1, 3
          HADR(I,J) = HADR_BEST(I,J)
        END DO
      END DO
      NEXT = 0
  502 CONTINUE
      ZMIN = BIGNUM    !     establish ZMIN and ZMAX
      ZMAX = -BIGNUM
      DO J = 1, MON
        JC = HADR(2,J)
        ZZ = C(JC+3)
        IF (ZZ .LT. ZMIN) ZMIN = ZZ
        IF (ZZ .GT. ZMAX) ZMAX = ZZ
      END DO
      DO IWMON1 = 1, WMON
        IF (IH_TRACK_BEST(IWMON1).GT.0) THEN
          ZZ = Z(IWMON1)
          IF (ZZ .LT. ZMIN) ZMIN = ZZ
          IF (ZZ .GT. ZMAX) ZMAX = ZZ
        ENDIF
      END DO
C
      W = (ZMIN - LINE(3)) / LINE(6)
      PAR(1) = LINE(1) + W * LINE(4)
      PAR(2) = LINE(2) + W * LINE(5)
      W = (ZMAX - LINE(3)) / LINE(6)
      PAR(3) = LINE(1) + W * LINE(4)
      PAR(4) = LINE(2) + W * LINE(5)
      MONITOR = MON
      IF (NEXT .EQ. 0) THEN
        DO J = 1, N_PAR
          STEP_PAR(J) = ROAD
        END DO
        CALL SAMNSQ (SSWFCN1, MONITOR+WMON_TRACK_BEST, N_PAR, FUN, PAR,
     &    STEP_PAR)
      END IF
      DO J = 1, N_PAR
        STEP_PAR(J) = 0.1 * ROAD
      END DO
      CALL SAMNSQ (SSWFCN1, MONITOR+WMON_TRACK_BEST, N_PAR, FUN, PAR,
     &  STEP_PAR)
      LINE(1) = PAR(1)
      LINE(2) = PAR(2)
      LINE(3) = ZMIN
      LINE(4) = PAR(3) - PAR(1)
      LINE(5) = PAR(4) - PAR(2)
      LINE(6) = ZMAX - ZMIN
      W = 1.0 / SQRT (LINE(4)**2 + LINE(5)**2 + LINE(6)**2)
      LINE(4) = LINE(4) * W
      LINE(5) = LINE(5) * W
      LINE(6) = LINE(6) * W
C
C ****  Search new SAMUS points on track before fit procedure
C
      MON_OLD = MON
      MON = 0
      M = 0
      DO 501 PL = 1, NPL
        IF (IQ(NHIT+PL) .EQ. 0) GO TO 501
        LD = IQ(HITS+PL)
        JC = IQ(LD+3)
        CALL SADS2L (LINE, C(JC+1), DIST, W1, W2, OK)
        IF (OK .GT. 0 .AND. ABS(W2) .LE. C(JC+7)) THEN
          W = ABS (SQRT(DIST) - Q(LD+4))
          IF (W .LT. ROAD) THEN
            MON = MON + 1
            IF (HADR(1,MON) .NE. PL) M = 1
            HADR(1,MON) = PL
            HADR(2,MON) = JC
            HADR(3,MON) = LD
          END IF
        END IF
  501 CONTINUE
      IF (MON .LT. NSAMIN) GO TO 999
      IF (MON .NE. MON_OLD .OR. M .EQ. 1) THEN
        NEXT = NEXT + 1
        IF (NEXT .LT. 25) GO TO 502
      END IF
      MONITOR = MON
C
C ****  Put SAMUS information to the output banks
C
      IF (MONITOR+WMON_TRACK_BEST.LT. NSAMIN+NWAMIN) GO TO 999
      CALL SSWFCN1 (MONITOR+WMON_TRACK_BEST, N_PAR, FUN, PAR, 1)
      XI2 = 0.0
      DO J = 1, MONITOR+WMON_TRACK_BEST
        XI2 = XI2 + FUN(J)**2
      END DO
      XI2 = XI2 / REAL(MONITOR+WMON_TRACK_BEST-N_PAR) ! join(SAMUS+WAMUS) chisq
      IQ(LT+33) = MONITOR
      DO J = 1, 6
        Q(LT+J+33) = LINE(J)
      END DO
      Q(LT+40) = XI2
      I = LT + 40
      DO J = 1, MONITOR
        I = I + 1
        IQ(I) = HADR(3,J)
      END DO
C
C*** Fills Wamus output banks
C
      LPWOTRK=LPWTRK+1+NMAXROADS*(NMWPL+1)     ! end of wamus work bank
      LP=LPWOTRK+(NTRK-1)*12*5                 ! hits on track info bank pointer
      IQ(LP+1)=NTRK                            ! track number
      IQ(LP+2)=WMON_TRACK_BEST                 ! # wamus hits on this track
      I=0                                      
      DO IWMON=1,WMON                          ! now loop over hits to fill
        IPOS=IH_TRACK_BEST(IWMON)              ! the floating part of the bank
        IF (IPOS.GT.0) THEN                    ! check if this hit is on track
          I=I+1                                ! increment hits on track counter
          LPF=LP+2+(I-1)*3                     ! hits on track info bank pointer
          IQ(LPF+1)=IHIT(IWMON)                ! IHIT in MUOH bank
          IF (IWHICHSOL(IWMON,IPOS,1).EQ.1)THEN  ! drift time solution (1 or 2)
            DRIFT=-1
          ELSE
            DRIFT=1
          ENDIF
          IQ(LPF+2)=DRIFT
          IF (IWHICHSOL(IWMON,IPOS,3).EQ.1) THEN ! pad solution (1,2)
            SIGN=-1
          ELSE
            SIGN=+1
          ENDIF
          IQ(LPF+3)=(IWHICHSOL(IWMON,IPOS,2)-1)*SIGN  ! signed # waves -1
        ENDIF                                   
      END DO
C
  999 CONTINUE
      RETURN
      END
