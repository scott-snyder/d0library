      SUBROUTINE QCD_GDSP
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Simulation of Global DSP
C-                         Basically remove identical candidates
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  30-JUN-1993   Richard V. Astur
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:PSL1.INC'
      INTEGER I,J, ITARGET, IETA, IPHI
      REAL MASK1(8+1,8+1), MASK2(8+1,8+1)
      INTEGER ID1(9,9), ID2(9,9)
      INTEGER IMASK1_ETA, IMASK2_ETA, IMASK1_PHI, IMASK2_PHI
      REAL ET
C----------------------------------------------------------------------
C
      CALL VZERO( MASK1, 9*9 )
      CALL VZERO( MASK2, 9*9 )
      CALL VZERO( ID1, 9*9 )
      CALL VZERO( ID2, 9*9 )
C
C: Identify candidates that are close to others. Keep the highest ET object
C
      DO I = 1, NENTRY(3)
        IMASK1_ETA = ( PSLDSP_ETA(I) - 1 )/5 + 1
        IMASK1_PHI = ( PSLDSP_PHI(I) - 1 )/4 + 1
        IMASK2_ETA = ( PSLDSP_ETA(I) + 1 )/5 + 1
        IMASK2_PHI = ( PSLDSP_PHI(I) + 1 )/4 + 1
        IF ( MASK1( IMASK1_ETA, IMASK1_PHI ) .LT. PSLDSP_ET(I) .AND.
     &    MASK2( IMASK2_ETA, IMASK2_PHI ) .LT. PSLDSP_ET(I) ) THEN
          MASK1( IMASK1_ETA, IMASK1_PHI ) = PSLDSP_ET(I)
          MASK2( IMASK2_ETA, IMASK2_PHI ) = PSLDSP_ET(I)
          IF ( ID1( IMASK1_ETA, IMASK1_PHI ) .GT. 0 ) PSLDSP_ET( ID1(
     &      IMASK1_ETA, IMASK1_PHI ) ) = 0.
          IF ( ID2( IMASK2_ETA, IMASK2_PHI ) .GT. 0 ) PSLDSP_ET( ID2(
     &      IMASK2_ETA, IMASK2_PHI ) ) = 0.
          ID1(IMASK1_ETA, IMASK1_PHI ) = I
          ID2(IMASK2_ETA, IMASK2_PHI ) = I
        ELSE
          PSLDSP_ET(I) = -1.
        ENDIF
      ENDDO
C
C: Remake list
C
      NENTRY(4) = 0
      DO I = 1, NENTRY(3)
        IF ( PSLDSP_ET(I) .GT. 0. ) THEN
          NENTRY(4) = NENTRY(4) + 1
          IF ( NENTRY(4) .NE. I ) THEN
            PSLDSP_ET( NENTRY(4) ) = PSLDSP_ET( I )
            PSLDSP_ETA( NENTRY(4) ) = PSLDSP_ETA( I )
            PSLDSP_PHI( NENTRY(4) ) = PSLDSP_PHI( I )
          ENDIF
        ENDIF
      ENDDO
C
      CALL HFILL( 907, FLOAT( NENTRY(4) ), 0., 1. )
  999 RETURN
      END
