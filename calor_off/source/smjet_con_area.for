      SUBROUTINE SMJET_CON_AREA(CONE_USED, NUM_CONES,
     &  OLDJET, CONE_ARRY, SMAREA, SMAREA_STHW,
     &  SMAREA_ICR, SMAREA_ICR_STHW )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return area calculated for split and
C-    merged jets for later use in underlying event zero suppression noise
C-    calculations.
C-
C-   Inputs  : CONE_USED     [R]  : Conesize of jet
C-             NUM_CONES  [I]  : NUMBER OF JET CONES ABOVE THRESHOLD
C-             OLDJET     [I]  : Previously found jet shaing Et w/ NEWJET
C-             CONE_ARRY(200,3) [R] : ARRAY OF FOUND JET CONES
C-                   CONE_ARRY(N,1) : ETA FOR CONE N
C-                   CONE_ARRY(N,2) : PHI FOR CONE N
C-                   CONE_ARRY(N,3) : JET CONTAINING CONE N
C-   Outputs : SMAREA(3)           [R]  : eta x phi area of jet
C-             SMAREA_STHW(3)      [R]  :    ''  sin(theta) weighted
C-             SMAREA_ICR(3)       [R]  :    ''  w/in ICR
C-             SMAREA_ICR_STHW(3)  [R]  :    ''  w/in ICR sin(theta) weighted
C-                    1 : OVERLAP AREA CORRECTION NEAREST NEWJET
C-                    2 : OVERLAP AREA CORRECTION NEAREST OLDJET
C-                    3 : NON OVERLAP AREA CORRECTION FOR NEWJET
C-
C-   Created  17-OCT-1995   Bob Hirosky
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:PI.DEF'
C
      REAL CONE_USED, CONE_ARRY(200,3)
      INTEGER NUM_CONES,OLDJET
      REAL SMAREA(3), SMAREA_STHW(3)
      REAL SMAREA_ICR(3), SMAREA_ICR_STHW(3)
C
      REAL ETA_NEW, PHI_NEW
      REAL ETA_SLICE, PHI_SLICE, PHI_PIECE
      REAL DISTONW, DISTOLD, TMPDIST
      LOGICAL IN_NEW, IN_OLD, IN_OTHER
      INTEGER ICONE, OTHER_JET
      REAL ETA_OTHER, PHI_OTHER
      INTEGER IAREA
C
      REAL SINTHETA, DELTA_R
      REAL RETA1, RETA2, RPHI1, RPHI2
C
      REAL FACTOR
      PARAMETER( FACTOR = .0098 )
      REAL SMALL
      PARAMETER( SMALL = .00001 )
      REAL PIE
      PARAMETER( PIE = PI )
C---------------------------------------------------------------------
C: Statement function to determine delta r in eta-phi(X-Y)
      DELTA_R(RETA1,RETA2,RPHI1,RPHI2) = SQRT( ((RETA1)-(RETA2))**2 +
     &  MIN( MOD(ABS((RPHI1)-(RPHI2)),2*PIE) ,
     &  2*PIE-ABS(MOD(ABS((RPHI1)-(RPHI2)),2*PIE)) )**2 )
C----------------------------------------------------------------------
      DO IAREA = 1,3
        SMAREA(IAREA) = 0.0
        SMAREA_STHW(IAREA) = 0.0
        SMAREA_ICR(IAREA) = 0.0
        SMAREA_ICR_STHW(IAREA) = 0.0
      ENDDO
C
      ETA_NEW = CONE_ARRY(NUM_CONES,1)       !NEW JET COORD
      PHI_NEW = CONE_ARRY(NUM_CONES,2)
C
C: Break the calculation up into eta,phi squares and assign each square
C: to a region 1-4 depending on which jet(s) contain the square.
C
C- then do calc. for overlap region
C: Use .099 instead of .1 to avoid round off problems that would
C: stop the loop prematurely
C
      DO ETA_SLICE = ETA_NEW - CONE_USED, ETA_NEW + CONE_USED -.05, .099
C
C
        DO PHI_SLICE = PHI_NEW - CONE_USED, PHI_NEW + CONE_USED-.05,.099
C
          PHI_PIECE = PHI_SLICE
          IF (PHI_PIECE .LT. 0.0) THEN
            PHI_PIECE = 2.0*PIE + PHI_PIECE
          ELSEIF (PHI_PIECE .GT. 2.0*PIE) THEN
            PHI_PIECE = PHI_PIECE - 2.0*PIE
          ENDIF
C
          DISTONW = DELTA_R(ETA_SLICE,ETA_NEW,PHI_PIECE,PHI_NEW)
          IN_NEW = ( DISTONW .LE.  CONE_USED )
C
          IF (IN_NEW) THEN                    ! IS POINT INSIDE NEW JET?
            IN_OLD = .FALSE.
            IN_OTHER = .FALSE.
C
C ****  LOOK THROUGH ALL CONES TO FIND OVERLAPPING AREAS W/ NEWJET
C
            DISTOLD = 999.9
            DO ICONE = 1,NUM_CONES-1          ! DONT LOOK FOR OVERLAP W/ ITSELF
              ETA_OTHER = CONE_ARRY(ICONE,1)        !LAST CONE IS NEWJET
              PHI_OTHER = CONE_ARRY(ICONE,2)
              TMPDIST = DELTA_R(ETA_OTHER,ETA_SLICE,PHI_OTHER,PHI_PIECE)
C
              IF (TMPDIST.LE.CONE_USED) THEN
                OTHER_JET = NINT(CONE_ARRY(ICONE,3))
                IF (OTHER_JET .EQ. OLDJET) THEN
                  IN_OLD = .TRUE.
                  DISTOLD = MIN(TMPDIST,DISTOLD)
                ELSE
                  IN_OTHER = .TRUE.
                  GOTO 666
C                  ICONE = NUM_CONES-1       ! GET OUT OF LOOP, POINT IN 3RD JET
                ENDIF
              ENDIF
C
            ENDDO
  666       CONTINUE
C
            IAREA = 0
            IF ( (.NOT. IN_OTHER) .AND. (.NOT. IN_OLD) )THEN  ! AREA in NEWJET
              IAREA = 3
            ELSEIF ((IN_OLD) .AND. (.NOT. IN_OTHER)) THEN     ! OVERLAP AREA
              IAREA = 1                                        ! AREA ->NEWJET
              IF ( DISTOLD .LT. DISTONW ) IAREA = 2            ! AREA ->OLDJET
            ELSE
              IAREA = 4                                       ! Overlap w/ 3rd
            ENDIF                                             ! jet, ignore
C
            IF ((IAREA.GT.0).AND.(IAREA.LT.4)) THEN      !AREA NOT IN A 3RD JET
              SINTHETA = 1./COSH(ETA_SLICE)
              IF ( ABS(ETA_SLICE + SMALL) .GE. 1.2  .AND.
     &                          ABS(ETA_SLICE - SMALL) .LE. 1.5 ) THEN
                SMAREA_ICR(IAREA) = SMAREA_ICR(IAREA) + FACTOR
                SMAREA_ICR_STHW(IAREA) = SMAREA_ICR_STHW(IAREA) + 
     &            FACTOR * SINTHETA
              ENDIF
C
              SMAREA(IAREA) = SMAREA(IAREA) + FACTOR
              SMAREA_STHW(IAREA) = SMAREA_STHW(IAREA) + 
     &          FACTOR * SINTHETA
            ENDIF
C
          ENDIF   !in newjet
        ENDDO     !phi_slice
C
      ENDDO       !eta_slice
C
  999 RETURN
      END
