      SUBROUTINE FCODER_MCFIX(ADDR,HALF,UNIT,QDRT,SCTR,WIRE)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Encode components into address.
C-                         Special version to fix bad MC data generated
C-                         ~Nov 1992.
C-
C-   Inputs  : half = N/S FDC   : bit 11
C-             unit = 1 for phi, 0 for theta : bit 10
C-             wire = 0 - 9 for theta, 0 - 15 for phi  : bits 0-3
C-             sctr = 0 - 5 for theta : (bits 4-6), 0 - 35 for phi (bits 4-9)
C-             qdrt = 0 - 7 for theta : (bits 7-9) , meaningless for phi
C-   Outputs : id  : coded address of channel
C-
C-   Created   2-DEC-1988   Srini Rajagopalan
C-   Updated  28-JAN-1993   Robert E. Avery   from standard fcoder.for
C
C----------------------------------------------------------------------
C
      IMPLICIT NONE
C
      INCLUDE 'D0$PARAMS:FDPARA.PARAMS'
C
      INTEGER ADDR
      INTEGER HALF,UNIT,DETT,WIRE,SCTR,QDRT
      INTEGER POWER2(0:15),ADDR_TEMP
      INTEGER RUN,RUNTYPE
      INTEGER SCTR_TEMP,QDRT_TEMP,W_TEMP 
      INTEGER MC_SCTR_FLIP(0:35)
      INTEGER MC_QDRT_FLIP(0:7)
C
      INTEGER THETA_ADDRESS(0:9,0:7,0:7,0:1)
      INTEGER PHI_ADDRESS(0:15,0:35,0:1)
      INTEGER H,U,Q,S,W
      INTEGER NQDRT,NSCTR,NWIRE
C
      LOGICAL FIRST
C
      DATA FIRST/.TRUE./
      DATA POWER2/ 1,2,4,8,16,32,64,128,256,512,1024,
     &             2048,4096,8192,16384,32768/
      DATA MC_QDRT_FLIP /    1, 0, 3, 2, 6, 5, 4, 7/
      DATA MC_SCTR_FLIP /   17,16,15,14,13,12,11,10, 9,
     &                       8, 7, 6, 5, 4, 3, 2, 1, 0,
     &                      35,34,33,32,31,30,29,28,27,
     &                      26,25,24,23,22,21,20,19,18 /
C
C----------------------------------------------------------------------
C
      IF (FIRST) THEN
        FIRST = .FALSE.
        CALL VZERO(THETA_ADDRESS,960)
        CALL VZERO(PHI_ADDRESS,1152)
C
        DETT = 2                        ! FDC
C
        DO H = 0, 1
          DO U = 0, 1
C
            IF (U.EQ.0) THEN
              NQDRT = MXQUAD
C
C We encode unused delay lines as Sector 6 & 7
              NSCTR = 7
              NWIRE = MXWIRT + 2
            ELSE
              NQDRT = 0
              NSCTR = MXSECP
              NWIRE = MXWIRP
            END IF
C
            DO Q = 0, NQDRT
              DO S = 0, NSCTR
                DO W = 0, NWIRE
C
                  ADDR_TEMP = 0
                  ADDR_TEMP = ADDR_TEMP + U*POWER2(10)
                  ADDR_TEMP = ADDR_TEMP + H*POWER2(11)
                  ADDR_TEMP = ADDR_TEMP + DETT*POWER2(13)
                  IF (U.EQ.0) THEN             ! Theta chamber
                    QDRT_TEMP = Q
                    SCTR_TEMP= S
                    W_TEMP = W
C MC fix:
                    IF ( H.EQ.1 ) THEN
                      QDRT_TEMP = MC_QDRT_FLIP( Q )
                      IF ( W.EQ.8 ) THEN
                        W_TEMP = 9
                      ELSEIF ( W.EQ.9 ) THEN
                        W_TEMP = 8
                      ENDIF
                    ENDIF
C End MC fix:
                    ADDR_TEMP = ADDR_TEMP + W_TEMP *POWER2(0)
                    ADDR_TEMP = ADDR_TEMP + SCTR_TEMP *POWER2(4)
                    ADDR_TEMP = ADDR_TEMP + QDRT_TEMP *POWER2(7)
                  ELSE                            ! Phi chamber
                    SCTR_TEMP= S
C MC fix:
                    IF ( H.EQ.1 ) THEN
                      SCTR_TEMP= MC_SCTR_FLIP(S)
                    ENDIF
C End MC fix:
                    ADDR_TEMP = ADDR_TEMP + W*POWER2(0)
                    ADDR_TEMP = ADDR_TEMP + SCTR_TEMP*POWER2(4)
                  ENDIF
C
                  IF (U.EQ.0) THEN
                    THETA_ADDRESS(W,S,Q,H) = ADDR_TEMP
                  ELSE
                    PHI_ADDRESS(W,S,H) = ADDR_TEMP
                  END IF
C
                END DO
              END DO
            END DO
          END DO
        END DO
C
      END IF
C
      IF (UNIT.EQ.0) THEN
        ADDR = THETA_ADDRESS(WIRE,SCTR,QDRT,HALF)
      ELSE
        ADDR = PHI_ADDRESS(WIRE,SCTR,HALF)
      END IF
C
  999 RETURN
      END
