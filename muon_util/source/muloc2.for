           SUBROUTINE MULOC2(NP,ZHTRAK,YHTRAK,DR,NSDR,JHDR,
     &  ISOLDR,QD)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Find bend view solutions for A-stubs
C-
C-   Inputs:
C-      NP     - Number of hits/plane
C-      ZHTRAK - Drift direction
C-      YHTRAK - Interplane
C-      DR     - Drift distance
C-
C-   Outputs:
C-      NSDR   - Number of bend view solutions
C-      JHDR   - Hit ID of points used in segment
C-      ISOLDR - ITSIGN, left or right, drift sol'n 1 or 2
C-      QD     - Quality of segment (chisq)
C-
C-   Created  11-JUL-1994   Elizabeth Brillhart
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INTEGER NSM,NHP
      PARAMETER(NSM=10)   ! maximum number of segments
      PARAMETER(NHP=10)   ! maximum number of hits/plane

      INTEGER I,II1,II2,II3,II4,J,J1,J2,J3,J4
      INTEGER NSEG,NHITS,NLAYER,NHT,NPNTS
      INTEGER NP(4),NSDR,JHDR(NSM,4),ISOLDR(NSM,4) 

      REAL    DZ(4),DY(4),CHISQ,SLOPE,POINT 
      REAL    ZHTRAK(4,NHP),YHTRAK(4,NHP),DR(2,4,NHP),QD(4,NSM)
      REAL    D(4,4,NHP)  !plane(1:4),sol'n(1:4),# hits/plane(1:10)
      REAL    PNT(NSM,4,2) 
      REAL    DZOLD(4),DYOLD(4),CHISQMX
      DATA CHISQMX/10./
C----------------------------------------------------------------------

      CALL VZERO(JHDR,NSM*4)
      CALL VZERO(ISOLDR,NSM*4)
      CALL VZERO(QD,4*NSM)
      CALL VZERO(D,4*4*NHP)
      CALL VZERO(PNT,NSM*4*2)
      NSDR  = 0
      NSEG  = 0
      NHITS = 0

      NLAYER = 0
      DO J=1,4
        IF ( NP(J) .NE. 0) NLAYER = NLAYER + 1
        NHITS = NHITS + NP(J)
      END DO

C If less than 3 planes in A-layer hit, return
      IF (NLAYER .LT. 4) RETURN

C Drift solutions
      DO 1 J=1,4                                ! # of planes in A-layer
        DO 2 I=1,NP(J)                          ! # of hits/plane J
          D(J,1,I) = ZHTRAK(J,I) + DR(1,J,I)    ! drift sol'n 1, right
          D(J,2,I) = ZHTRAK(J,I) - DR(1,J,I)    ! drift sol'n 1, left
          IF (DR(1,J,I) .EQ. 9999.) THEN
            D(J,1,I) = 9998.
            D(J,2,I) = 9999.
          END IF
          IF (DR(2,J,I) .NE. 9999.) THEN
            D(J,3,I) = ZHTRAK(J,I) + DR(2,J,I)  ! drift sol'n 2, right
            D(J,4,I) = ZHTRAK(J,I) - DR(2,J,I)  ! drift sol'n 2, left
          ELSE
            D(J,3,I) = 9999.                    ! only one hit on wire
            D(J,4,I) = 9999.
          END IF
    2   CONTINUE
    1 CONTINUE

C Form segments using one hit in each layer
      DO 10 II1=1,NP(1)                ! loop over hits in plane 0
        DO 11 J1=1,4                   ! 4 drift solutions
          IF (D(1,J1,II1) .GE. 9999.) GOTO 11

          DO 20 II2=1,NP(2)            ! loop over hits in plane 1
            DO 21 J2=1,4               ! 4 drift solutions
              IF (D(2,J2,II2) .GE. 9999.) GOTO 21

              DO 30 II3=1,NP(3)        ! loop over hits in plane 2
                DO 31 J3=1,4           ! 4 drift solutions
                  IF (D(3,J3,II3) .GE. 9999.) GOTO 31

                  DO 40 II4=1,NP(4)    ! loop over hits in plane 3
                    DO 41 J4=1,4       ! 4 drift solutions
                      IF (D(4,J4,II4) .GE. 9999.) GOTO 41

                      DZ(1) = D(1,J1,II1)        ! drift
                      DZ(2) = D(2,J2,II2)
                      DZ(3) = D(3,J3,II3)
                      DZ(4) = D(4,J4,II4)
                      DY(1) = YHTRAK(1,II1)      ! interplane
                      DY(2) = YHTRAK(2,II2)
                      DY(3) = YHTRAK(3,II3)
                      DY(4) = YHTRAK(4,II4)

                      NHT = 0

                      CALL VZERO(DZOLD,4)
                      CALL VZERO(DYOLD,4)

                      DO I=1,4
                        DZOLD(I) = DZ(I)
                        DYOLD(I) = DY(I)
                        IF ( (DZ(I) .GE. 9998.)   .OR.
     &                       (DY(I) .GE. 9998.) ) THEN
                          NHT = NHT + 1
                        END IF
                      END DO
                      IF (NHT .GT. 1) GOTO 41

C Use least-squares to find best fit to the hits
                      CALL MULOCLS(DY,DZ,POINT,SLOPE,CHISQ)
                      IF (CHISQ.GT.CHISQMX) GO TO 41

C Count number of segments found
                      NSEG = NSEG + 1
                      NSDR = NSEG
C
                      IF (NSDR.GT.0.AND.NSDR.LE.10) THEN
                        QD(1,NSDR)   = CHISQ         ! quality of fit
                        QD(2,NSDR)   = POINT
                        QD(3,NSDR)   = SLOPE
                        QD(4,NSDR)   = FLOAT(NPNTS)  ! # points in segment fit
                        JHDR(NSDR,1) = II1           ! hit id
                        JHDR(NSDR,2) = II2
                        JHDR(NSDR,3) = II3
                        JHDR(NSDR,4) = II4
                        IF (J1 .EQ. 1) THEN          ! drift sol'n 1,2; L,R
                          ISOLDR(NSDR,1) = 1         ! for each plane
                        ELSE IF (J1 .EQ. 2) THEN
                          ISOLDR(NSDR,1) = -1
                        ELSE IF (J1 .EQ. 3) THEN
                          ISOLDR(NSDR,1) = 2
                        ELSE IF (J1 .EQ. 4) THEN
                          ISOLDR(NSDR,1) = -2
                        ELSE
                          ISOLDR(NSDR,1) = 0
                        END IF
                        IF (J2 .EQ. 1) THEN
                          ISOLDR(NSDR,2) = 1
                        ELSE IF (J2 .EQ. 2) THEN
                          ISOLDR(NSDR,2) = -1
                        ELSE IF (J2 .EQ. 3) THEN
                          ISOLDR(NSDR,2) = 2
                        ELSE IF (J2 .EQ. 4) THEN
                          ISOLDR(NSDR,2) = -2
                        ELSE
                          ISOLDR(NSDR,2) = 0
                        END IF
                        IF (J3 .EQ. 1) THEN
                          ISOLDR(NSDR,3) = 1
                        ELSE IF (J3 .EQ. 2) THEN
                          ISOLDR(NSDR,3) = -1
                        ELSE IF (J3 .EQ. 3) THEN
                          ISOLDR(NSDR,3) = 2
                        ELSE IF (J3 .EQ. 4) THEN
                          ISOLDR(NSDR,3) = -2
                        ELSE
                          ISOLDR(NSDR,3) = 0
                        END IF
                        IF (J4 .EQ. 1) THEN
                          ISOLDR(NSDR,4) = 1
                        ELSE IF (J4 .EQ. 2) THEN
                          ISOLDR(NSDR,4) = -1
                        ELSE IF (J4 .EQ. 3) THEN
                          ISOLDR(NSDR,4) = 2
                        ELSE IF (J4 .EQ. 4) THEN
                          ISOLDR(NSDR,4) = -2
                        ELSE
                          ISOLDR(NSDR,4) = 0
                        END IF
                        DO I=1,4
                          IF (DZ(I) .GE. 9998.) THEN
                            JHDR(NSDR,I)   = 0
                            ISOLDR(NSDR,I) = 0
                          END IF
                          PNT(NSDR,I,1) = DZ(I)      ! points used in fit
                          PNT(NSDR,I,2) = DY(I)
                          DZ(I) = DZOLD(I)
                          DY(I) = DYOLD(I)
                        END DO
                      END IF
   41               CONTINUE
   40             CONTINUE               ! plane 3
   31           CONTINUE
   30         CONTINUE                   ! plane 2
   21       CONTINUE
   20     CONTINUE                       ! plane 1
   11   CONTINUE
   10 CONTINUE                           ! plane 0
  111 IF (NSEG .LE. 10) THEN
        NSDR = NSEG
      ELSE
        NSDR = 10
      END IF
  999 RETURN
      END
