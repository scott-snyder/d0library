      SUBROUTINE FSINGLE_DL(COMB,NCOMB,USED_0,USED_M,USED_P,
     &            AHIT0,AHITM,AHITP,OK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :  Try to assign single, left over, delay 
C-    line hits to SW0 hits.  
C-
C-   Inputs  : COMB = Particular combination of interest
C-             NCOMB = Number of combinations
C-             USED_0,USED_M,USED_P = Arrays telling which segments
C-                                    have been used already
C-             AHIT0,AHITM,AHITP = Lists of hit combinations
C-             
C-   Outputs : OK = TRUE if delay line hit is acceptable
C-   Controls: 
C-
C-   Created  20-FEB-1992   Susan K. Blessing
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$PARAMS:FDPARA.PARAMS'
C
      INTEGER C,COMB,NCOMB
      INTEGER HIT0,HITM,HITP
      INTEGER AHITM(1000),AHITP(1000),AHIT0(1000)
C
      LOGICAL USED_M(0:MX_HIT_WIRE),USED_P(0:MX_HIT_WIRE)
      LOGICAL USED_0(MX_HIT_WIRE)
      LOGICAL OK
C
C----------------------------------------------------------------------
C
      OK = .FALSE.
C
      HIT0 = AHIT0(COMB)
      HITM = AHITM(COMB)
      HITP = AHITP(COMB)
C
      DO C = 1, NCOMB
C
        IF (C.NE.COMB) THEN
C
C Only check single hits
          IF ((AHITM(C).EQ.0.OR.AHITP(C).EQ.0).AND.
     &      .NOT.USED_0(AHIT0(C))) THEN
C
C If HIT0 appears again in the singles, the solution is ambiguous
            IF (HIT0.EQ.AHIT0(C).AND.
     &          .NOT.USED_M(AHITM(C)).AND.
     &          .NOT.USED_P(AHITP(C)) ) THEN
C Turn off so don't check again
              USED_0(HIT0) = .TRUE.
              GO TO 999
            END IF
C
C If HITM appears again in the singles, the solution is ambiguous
            IF (HITM.EQ.AHITM(C).AND.HITM.GT.0) THEN
C Turn off so don't check again
              USED_M(HITM) = .TRUE.
              GO TO 999
            END IF
C
C If HITP appears again in the singles, the solution is ambiguous
            IF (HITP.EQ.AHITP(C).AND.HITP.GT.0) THEN
C Turn off so don't check again
              USED_P(HITP) = .TRUE.
              GO TO 999
            END IF
C
          END IF
C
        END IF
C
      END DO
C
      OK = .TRUE.
C
  999 RETURN
      END
