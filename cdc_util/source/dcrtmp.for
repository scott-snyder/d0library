      SUBROUTINE DCRTMP(LAYER,SECT,WIRE,UBIT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Mapping between Electronic & Logical Channels
C-
C-   Inputs  : None
C-   Outputs : LAYER = CDC Layer (0-3) 0 => inner, 3 => outer
C-             SECT  = CDC Sector (0-31)
C-             WIRE  = CDC Wire # (0-10) [(0-6)=>SW's,(7-10)=>DL's]
C-             UBIT  = 0 for used channels, = 1 for unused channels
C-   Controls: CRATE = CDC Crate Number (0-5) for CDC (id's 4,14,24,...54)
C-             CARD  = CDC Card Number (0-15)
C-             CHNL  = CDC Channel Number (0-15)
C-
C-   Created  31-JUL-1990   Jim Cochran
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INTEGER I,J,K
      INTEGER LAYER(0:5,0:15,0:15),SECT(0:5,0:15,0:15)
      INTEGER WIRE(0:5,0:15,0:15),UBIT(0:5,0:15,0:15)
      INTEGER SW(0:3,0:7),DLA(0:15),DLB,DLC,DLD
C
      DATA SW/1,8,15,24,3,6,13,22,5,4,11,20,7,2,9,18,
     &  25,0,23,16,27,30,21,14,29,28,19,12,31,26,17,10/
      DATA DLA/1,1,0,0,1,1,0,0,2,2,1,1,2,2,1,1/
C----------------------------------------------------------------------
C     --- zero output variables - vzero and uzero don't work
      DO I = 0,5                         
        DO J = 0,15                     
          DO K = 0,15
            UBIT(I,J,K) = 0
            LAYER(I,J,K) = 0
            SECT(I,J,K)= 0
            WIRE(I,J,K) = 0
          END DO
        END DO
      END DO
C
C                               --- determine mapping for SW's
      DO I = 0,3                    ! I=> crate, J=> card, K=> channel
        DO J = 0,15
          DO K = 0,15
            IF (MOD(J,2) .EQ. 0) THEN         ! --- if J is even
              SECT(I,J,K) = SW(I,J/2)
              IF (K .LT. 9) LAYER (I,J,K)=2
              IF (K .GT. 8) LAYER (I,J,K)=3
                IF (I .EQ. 1) THEN            ! --- fix for crate 1
                  IF (K .LT. 9) LAYER (I,J,K)=0
                  IF (K .GT. 8) LAYER (I,J,K)=1
                ENDIF
            ENDIF
            IF (MOD(J,2) .EQ. 1) THEN         ! --- if J is odd
              SECT(I,J,K) = SW(I,(J-1)/2)
              IF (K .LT. 9) LAYER (I,J,K)=0
              IF (K .GT. 8) LAYER (I,J,K)=1
                IF (I .EQ. 1) THEN            ! --- fix for crate 1
                  IF (K .LT. 9) LAYER (I,J,K)=2
                  IF (K .GT. 8) LAYER (I,J,K)=3
                ENDIF
            ENDIF
            IF (K .EQ. 0 .OR. K .EQ. 1) UBIT(I,J,K) = 1
            IF (K .EQ. 0 .OR. K .EQ. 1) WIRE(I,J,K) = K+11
            IF (K .GT. 1 .AND. K .LT. 9) WIRE(I,J,K) = K-2
            IF (K .GT. 8) WIRE(I,J,K) = K-9
          END DO
        END DO
      END DO
C                               --- determine mapping for DL's
      DLB = 7
      DLC = 5
      DLD = 0
      DO I = 4,5
        DO J = 0,15
          DO K = 0,15
            UBIT(I,J,K) = 0
            LAYER(I,J,K) = DLD
            IF (MOD(K,2) .EQ. 1) DLD = DLD + 1
            IF (DLD .EQ. 4) DLD = 0
            IF (J .LT. 8) THEN
              IF (I .EQ. 4) SECT(I,J,K) = DLA(K) + DLB
              IF (I .EQ. 5) SECT(I,J,K) = DLA(K) + DLC
              IF (SECT(I,J,K) .GE. 32) SECT(I,J,K)=SECT(I,J,K) - 32
              IF (MOD(K,2) .EQ. 0) WIRE(I,J,K) = 8
              IF (MOD(K,2) .EQ. 1) WIRE(I,J,K) = 10
            ENDIF
            IF (J .GT. 7) THEN
              IF (I .EQ. 4) SECT(I,J,K) = DLA(K) + DLB
              IF (I .EQ. 5) SECT(I,J,K) = DLA(K) + DLC
              IF (SECT(I,J,K) .GE. 32) SECT(I,J,K)=SECT(I,J,K) - 32
              IF (MOD(K,2) .EQ. 0) WIRE(I,J,K) = 7
              IF (MOD(K,2) .EQ. 1) WIRE(I,J,K) = 9
            ENDIF
          END DO
          DLB = DLB + 2
          DLC = DLC - 2
          IF (DLC+1 .EQ. 0) DLC = 31
          IF (I .EQ. 4 .AND. J .EQ. 7) DLB = 7
          IF (I .EQ. 5 .AND. J .EQ. 7) DLC = 5
        END DO
      END DO
 999  RETURN
      END
