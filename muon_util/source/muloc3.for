      SUBROUTINE MULOC3(ITEMP,XHTRAK,YHTRAK,DT,PD,WLEN,NSPD,ISDT,
     &  ISPD,QP)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-
C-   Inputs  :
C-      ITEMP  - Hit ID
C-      XHTRAK - View along the wire
C-      YHTRAK - Interplane
C-      DT     - Position along the wire
C-      PD     - Vernier Distance (2 soln's)
C-
C-   Outputs :
C-      NSPD   - Number of non-bend view solutions
C-      ISDT   - Drift Time solution 1 or 2
C-      ISPD   - Pad solution 1 or 2
C-      QP     - Quality of segment (chisq)
C-
C-   Created  16-AUG-1994   Elizabeth Brillhart 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INTEGER   NHP,NSM,NFIT
      PARAMETER (NHP=10)    ! Maximum number of hits per plane
      PARAMETER (NSM=10)    ! Maximum number of segments
      PARAMETER (NFIT=20)   ! Maximum number of points in fit (from MUFIT3)
C
      INTEGER   I,J,J1,J2,J3,J4,NT,NPD,P,DUMMY(NSM,4)
      INTEGER   NDTIME,NLAYER,NHITS,VS1,VS2,ISOL(NFIT),PL(4)
      INTEGER   ITEMP(4),NSDT,NSPD,ISDT(NSM,4),ISPD(NSM,4)
      INTEGER   IV(NFIT),K1(NFIT),K2(NFIT)
C
      REAL      XHTRAK(4,NHP),YHTRAK(4,NHP),DT(2,4,NHP),PD(2,4,NHP)
      REAL      QP(4,NSM),DX(4),DY(4),PNT(NSM,4,3)
      REAL      POINT,SLOPE 
      REAL      DTX(NHP),DTY(NHP),DTW(NHP),WLEN(NHP)
      REAL      DTSOL(4,2)  ! plane(1:4),soln 1 or 2
      REAL      PDSOL(4,2)  !     "          "
      REAL      WVTX,VTXX,VTXY,WDT1
      REAL      SUMW,SUMX,SUMX2,SUMY,SUMY2,SUMXY,XCOG,YCOG
      REAL      TSLOPE,TCHISQ,ERXCOG,ERSLP
      REAL      TIMES,DVS1,DVS2,SIGP
      REAL      YP(NFIT),XP(2,NFIT),X1,Z1,SL1,C1,CHISQMX
C
      INTEGER   IVER
      DATA      IVER/0/     ! =0 use vertex as point in fit
C                            ! in nb, central, vtxy,vtxx =0.
      REAL      SIGT
      DATA      SIGT/10./   ! relative weight for delta time (DH)
      REAL      SIGV
      DATA      SIGV/30./   ! relative weight for vertex if used in fit (DH)
      REAL      LVER
      DATA      LVER/60.96/ ! length of vernier pad
      DATA CHISQMX/300./
C----------------------------------------------------------------------
C
      CALL VZERO_i(ISDT,NSM*4)
      CALL VZERO_i(ISPD,NSM*4)
      CALL VZERO(PNT,NSM*4*2)
      CALL VZERO(QP,4*NSM)
      NSPD   = 0
      NSDT   = 0
      NHITS  = 0
      NLAYER = 0
      NDTIME = 0
      DO J=1,4
        IF ( ITEMP(J) .NE. 0) NLAYER = NLAYER + 1
      END DO
      IF (NLAYER .LT. 4) GO TO 999
C
C Time Solutions
      DO J=1,4                                         ! # of planes
        IF (ITEMP(J) .NE. 0) THEN
          DTSOL(J,1) = XHTRAK(J,ITEMP(J)) + DT(1,J,ITEMP(J))   ! time soln 1
          PDSOL(J,1) = XHTRAK(J,ITEMP(J)) + PD(1,J,ITEMP(J))   ! pad soln 1
          IF (DT(2,J,ITEMP(J)) .NE. 9999.) THEN
            DTSOL(J,2) = XHTRAK(J,ITEMP(J)) + DT(2,J,ITEMP(J)) ! time soln 2
          ELSE
            DTSOL(J,2) = 9999.                         ! only 1 time soln
          END IF
          IF (PD(2,J,ITEMP(J)) .NE. 9999.) THEN
            PDSOL(J,2) = XHTRAK(J,ITEMP(J)) + PD(2,J,ITEMP(J)) ! pad soln 2
          ELSE
            PDSOL(J,2) = 9999.
          END IF
        ELSE
          DTSOL(J,1) = 9998.
          DTSOL(J,2) = 9999.
          PDSOL(J,1) = 9998.
          PDSOL(J,2) = 9999.
        END IF
      END DO
      DO J=1,4
        IF ( DTSOL(J,1) .LT. 9998. ) NDTIME = NDTIME + 1
      END DO
      IF (NDTIME .LT.3) RETURN
C Include vertex in delta time fit if IVER=0
      IF(IVER.EQ.0) THEN              ! use vertex in fit
        WVTX = 1./SIGV**2             ! vertex weight
        VTXX = 0.                     ! in nbv, vtxx=0
        VTXY = 0.                     ! in nbv, central, vtxy=0
      ENDIF
C Weight for delta times (first time through)
      WDT1 = 1./SIGT**2
      DO 10 J1=1,2             ! loop over time soln's for each plane
        IF (DTSOL(1,J1) .GE. 9999.) GOTO 10
        DO 20 J2 = 1,2
          IF (DTSOL(2,J2) .GE. 9999.) GOTO 20
          DO 30 J3 = 1,2
            IF (DTSOL(3,J3) .GE. 9999.) GOTO 30
            DO 40 J4 = 1,2
              IF (DTSOL(4,J4) .GE. 9999.) GOTO 40
              DX(1) = DTSOL(1,J1)        ! direction along wire
              DX(2) = DTSOL(2,J2)
              DX(3) = DTSOL(3,J3)
              DX(4) = DTSOL(4,J4)
              DY(1) = YHTRAK(1,J1)       ! interplane
              DY(2) = YHTRAK(2,J2)
              DY(3) = YHTRAK(3,J3)
              DY(4) = YHTRAK(4,J4)
C
              NT = 0
              CALL VZERO(DTX,NHP)
              CALL VZERO(DTY,NHP)
              CALL VZERO(DTW,NHP)
              CALL VZERO_i(PL,4)
              DO J = 1,4
                IF (DX(J) .LT. 9998.) THEN
                  NT = NT+1
                  DTX(NT) = DX(J)
                  DTY(NT) = DY(J)
                  DTW(NT) = WDT1
                  PL(NT) = J
                ELSE
                  DY(J) = 9998.
                END IF
              END DO
              IF (NT .LT. 3) GOTO 40
              IF (IVER .EQ. 0) THEN
                NT      = NT + 1
                DTX(NT) = VTXX
                DTY(NT) = VTXY
                DTW(NT) = WVTX
              END IF
C
              CALL MUFSUM(NT,DTX,DTY,DTW,SUMW,SUMY,SUMY2,SUMX,SUMX2,
     &          SUMXY)
              CALL MUSLIN(SUMW,SUMX,SUMY,SUMX2,SUMY2,SUMXY,XCOG,YCOG,
     &          TSLOPE,ERXCOG,ERSLP,TCHISQ)
C
              IF (TCHISQ .LT. -1000) GOTO 40
              IF (IVER .EQ. 0) THEN
                P = NT-1
              ELSE
                P = NT
              END IF
              DO J = 1,P
                SIGP = ABS( XCOG-DTX(J) + TSLOPE*(DTY(J)-YCOG) )
                IF (SIGP .LT. 60.) THEN
                  DTW(J) = 1./SIGT**2
                ELSE
                  DTW(J) = 60.**2/SIGT**2/SIGP**2
                END IF
              END DO
C
              CALL MUFSUM(NT,DTX,DTY,DTW,SUMW,SUMY,SUMY2,SUMX,SUMX2,
     &          SUMXY)
              CALL MUSLIN(SUMW,SUMX,SUMY,SUMX2,SUMY2,SUMXY,XCOG,YCOG,
     &          TSLOPE,ERXCOG,ERSLP,TCHISQ)
C
              IF (TCHISQ .LT. -1000) GOTO 40
C
C Find closest vernier pad solution
              CALL VZERO(XP,2*NFIT)
              CALL VZERO(YP,NFIT)
              NPD = 0
              DO I = 1,4
                TIMES = XCOG + TSLOPE*(DY(I) - YCOG)
                IF (ABS(PDSOL(I,1)) .LT. 9998.AND. 
     1             ABS(PDSOL(I,2)) .LT. 9998) THEN
                  VS1   = (TIMES - PDSOL(I,1))/LVER + 0.5
                  DVS1  = LVER * VS1 + PDSOL(I,1) - TIMES
                  VS2   = (TIMES - PDSOL(I,2))/LVER + 0.5
                  DVS2  = LVER * VS2 + PDSOL(I,2) - TIMES
                  NPD = NPD + 1
                  IF (ABS(DVS1) .LT. ABS(DVS2) ) THEN
                    IV(NPD)=VS1+1          ! sol'n 1 is closest
                  ELSE
                    IV(NPD)=VS2+1          ! sol'n 2 is closest
                  ENDIF
                  IF(IV(NPD).LT.1) IV(NPD)=1
                  K1(NPD)=1
                  K2(NPD)=6
                  IF(IV(NPD).EQ.1) K1(NPD)=3     ! edge of chamber
                  IF( IV(NPD)*LVER+PDSOL(I,1).GT.WLEN(I) .AND.
     &                IV(NPD)*LVER+PDSOL(I,2).GT.WLEN(I) ) K2(NPD)=4
                  YP(NPD)=DY(I)
                  XP(1,NPD)=PDSOL(I,1)
                  XP(2,NPD)=PDSOL(I,2)
                ELSE IF (ABS(PDSOL(I,1)) .LT. 9998) THEN
                  VS1   = (TIMES - PDSOL(I,1))/LVER + 0.5
                  DVS1  = LVER * VS1 + PDSOL(I,1) - TIMES
                  NPD = NPD + 1
                  IV(NPD)=VS1+1          ! sol'n 2 unphysical
                  IF(IV(NPD).LT.1) IV(NPD)=1
                  K1(NPD)=1
                  K2(NPD)=6
                  IF(IV(NPD).EQ.1) K1(NPD)=3     ! edge of chamber
                  IF( IV(NPD)*LVER+PDSOL(I,1).GT.WLEN(I)) K2(NPD)=4
                  YP(NPD)=DY(I)
                  XP(1,NPD)=PDSOL(I,1)
                ELSE IF (ABS(PDSOL(I,2)) .LT. 9998) THEN
                  VS1   = (TIMES - PDSOL(I,1))/LVER + 0.5
                  DVS1  = LVER * VS1 + PDSOL(I,1) - TIMES
                  VS2   = (TIMES - PDSOL(I,2))/LVER + 0.5
                  DVS2  = LVER * VS2 + PDSOL(I,2) - TIMES
                  NPD = NPD + 1
                  IV(NPD)=VS2+1          ! sol'n 1 is unphysical
                  IF(IV(NPD).LT.1) IV(NPD)=1
                  K1(NPD)=1
                  K2(NPD)=6
                  IF(IV(NPD).EQ.1) K1(NPD)=3     ! edge of chamber
                  IF( IV(NPD)*LVER+PDSOL(I,2).GT.WLEN(I)) K2(NPD)=4
                  YP(NPD)=DY(I)
                  XP(2,NPD)=PDSOL(I,2)
                ELSE
                  NPD = NPD + 1
                  K1(NPD)=1
                  K2(NPD)=1
                END IF
              ENDDO
C SET REMAINDER TO 1
              DO I=NPD+1,NFIT
                K1(I)=1
                K2(I)=1
              ENDDO
C
              CALL MUFIT3(LVER,SUMW,SUMY,SUMY2,SUMX,SUMX2,SUMXY,NPD,IV,
     &          K1,K2,YP,XP,X1,Z1,SL1,C1,ISOL)
C
              IF (C1.GT.CHISQMX) GO TO 40
C
              J = 1
              DO I=1,4
                IF (PL(J) .EQ. I) THEN
                  IF (ISOL(J) .LT. 0) THEN
                    DX(I) = LVER*(ABS(ISOL(J))-1)+XP(1,J)
                  ELSEIF (ISOL(J) .GT. 0) THEN
                    DX(I) = LVER*(ABS(ISOL(J))-1)+XP(2,J)
                  END IF
                  J = J+1
                ELSE
                  DX(I) = 9998.
                END IF
              END DO
              NSPD = NSPD + 1
              DO J=1,4
                PNT(NSPD,J,1) = DTX(J)     ! delta T point
                PNT(NSPD,J,2) = DX(J)      ! pad point
                PNT(NSPD,J,3) = DY(J)      ! interplane
                DUMMY(NSPD,J) = ITEMP(J)   ! hit on plane
              END DO
C
              DO J = 1,4
                IF (DX(J) .NE. -1.) THEN
                  ISPD(NSPD,J) = ISOL(J)   ! pad solution
                END IF
              END DO
C
              QP(1,NSPD) = C1
              QP(2,NSPD) = POINT
              QP(3,NSPD) = SLOPE
              QP(4,NSPD) = FLOAT(NHITS)      ! # points in segment fit
C
              IF (J1 .EQ. 1) THEN     ! time soln 1 or 2, plane 0
                ISDT(NSPD,1) = 1
              ELSEIF (J1 .EQ. 2) THEN
                ISDT(NSPD,1) = -1
              END IF
C
              IF (J2 .EQ. 1) THEN     ! time soln 1 or 2, plane 1
                ISDT(NSPD,2) = 1
              ELSEIF (J2 .EQ. 2) THEN
                ISDT(NSPD,2) = -1
              END IF
C
              IF (J3 .EQ. 1) THEN     ! time soln 1 or 2, plane 2
                ISDT(NSPD,3) = 1
              ELSEIF (J3 .EQ. 2) THEN
                ISDT(NSPD,3) = -1
              END IF
C
              IF (J4 .EQ. 1) THEN     ! time soln 1 or 2, plane 3
                ISDT(NSPD,4) = 1
              ELSEIF (J4 .EQ. 2) THEN
                ISDT(NSPD,4) = -1
              END IF
C
              DO J = 1,4                                ! delta time soln not
                IF (DTX(J) .EQ. -1.) ISDT(NSPD,J) = 0   ! used in fit
              END DO
C
C
   40       CONTINUE
   30     CONTINUE
   20   CONTINUE
   10 CONTINUE
  999 RETURN
      END
