      SUBROUTINE FCODER(ADDR,HALF,UNIT,QDRT,SCTR,WIRE,UBIT,IFL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : decode address into detector components for ifl=1
C-                         Encode components into address for ifl=2
C-
C-   for ifl = 1
C-   Inputs  : id  : coded address of channel
C-   Outputs : ubit = 1 if unused channel : bit 15
C-             half = N/S FDC   : bit 11
C-             unit = 1 for phi, 0 for theta : bit 10
C-             wire = 0 - 9 for theta, 0 - 15 for phi  : bits 0-3
C-             sctr = 0 - 5 for theta : (bits 4-6), 0 - 35 for phi (bits 4-9)
C-             qdrt = 0 - 7 for theta : (bits 7-9) , meaningless for phi
C- Input/Output reversed for IFL = 2
C-
C-   Created   2-DEC-1988   Srini Rajagopalan
C-   Updated   5-MAR-1989   SR, Added IFL = 2 feature for reverse decoding
C-   Updated  18-APR-1989   Accomodated change in Logical bit pattern - SR
C-   Updated   7-MAY-1989   Change of bits ordering, use of function MVBITS
C-                          and IBITS instead of Jbit/Jbyt.
C-   Updated  10-APR-1991   Robert E. Avery   Accomodate mistakes in downloaded
C-                              ID's during Cosmic Commissioning run.
C-   Updated   3-MAY-1991   Robert E. Avery   Remove RCP call by adding
C-                              a new initialization entry point,
C-                              FCODER_INI, to be called in FTRPAR.
C-                              Also fixes problem in test beam runs.
C-   Updated   2-JUL-1991   Susan K. Blessing   Use a look up table
C-    to encode the address
C-   Updated   8-OCT-1991   Robert E. Avery   Fix bug in lookup table.
C-     (Also have increased max-sector for theta to 7, for fake DL channels).
C-   Updated  14-FEB-1992   Susan K. Blessing  Remove machine block.
C-   Updated   9-JUL-1993   Susan K. Blessing  Check for inappropriate
C-    input.
C
C----------------------------------------------------------------------
C
      IMPLICIT NONE
C
      INCLUDE 'D0$PARAMS:FDPARA.PARAMS'
C
      INTEGER IFL,ADDR,NEG
      INTEGER UBIT,HALF,UNIT,DETT,WIRE,SCTR,QDRT
      INTEGER POWER2(0:15),ADDR_TEMP
      INTEGER RUN,RUNTYPE
      INTEGER SCTR_TEMP
      INTEGER QDRT_TEMP
      INTEGER COS_SCTR_FLIP(0:35,0:1)
      INTEGER TB_SCTR_FLIP(0:35)
      INTEGER TB_QDRT_FLIP(0:7)
C
      INTEGER THETA_ADDRESS(0:9,0:7,0:7,0:1)
      INTEGER PHI_ADDRESS(0:15,0:35,0:1)
      INTEGER H,U,Q,S,W
      INTEGER NQDRT,NSCTR,NWIRE
C
      LOGICAL FIRST
      LOGICAL DO_COS_SECT_FLIP
      LOGICAL DO_TB_SECT_FLIP
      LOGICAL DO_TB_QUAD_FLIP
C
      DATA FIRST/.TRUE./
      DATA POWER2/ 1,2,4,8,16,32,64,128,256,512,1024,
     &             2048,4096,8192,16384,32768/
      DATA TB_QDRT_FLIP /    0, 1, 2, 3, 6, 5, 4, 7/
      DATA TB_SCTR_FLIP /    0, 1, 2, 3, 4, 5, 6, 7, 8,
     &                      13,12,11,10, 9,14,15,16,17,
     &                      18,19,20,21,22,23,24,25,26,
     &                      27,28,29,30,31,32,33,34,35/
      DATA COS_SCTR_FLIP/   0, 1, 2, 3, 4, 5, 6, 7, 8,
     &                      17,16,15,14,13,12,11,10, 9,
     &                      26,25,24,23,22,21,20,19,18,
     &                      27,28,29,30,31,32,33,34,35,
     &                      8, 7, 6, 5, 4, 3, 2, 1, 0,
     &                      9,10,11,12,13,14,15,16,17,
     &                      18,19,20,21,22,23,24,25,26,
     &                      35,34,33,32,31,30,29,28,27 /

C
      DATA DO_COS_SECT_FLIP / .FALSE. /
      DATA DO_TB_SECT_FLIP / .FALSE. /
      DATA DO_TB_QUAD_FLIP / .FALSE. /
C
C----------------------------------------------------------------------
C
      IF (FIRST) THEN
        FIRST = .FALSE.
        CALL VZERO(THETA_ADDRESS,960)
        CALL VZERO(PHI_ADDRESS,1152)
C
        DETT = 2                        ! FDC
        NEG = 0
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
                  ADDR_TEMP = ADDR_TEMP +  NEG*POWER2(12)
                  ADDR_TEMP = ADDR_TEMP + DETT*POWER2(13)
                  IF (U.EQ.0) THEN             ! Theta chamber
                    IF ( DO_TB_QUAD_FLIP ) THEN
                      QDRT_TEMP = TB_QDRT_FLIP( Q )
                    ELSE
                      QDRT_TEMP = Q
                    ENDIF
                    ADDR_TEMP = ADDR_TEMP + W*POWER2(0)
                    ADDR_TEMP = ADDR_TEMP + S*POWER2(4)
                    ADDR_TEMP = ADDR_TEMP + QDRT_TEMP *POWER2(7)
                  ELSE                            ! Phi chamber
                    SCTR_TEMP= S
                    IF ( DO_COS_SECT_FLIP ) THEN
                      IF     (( H .EQ. 1 ) .AND. (S.EQ.27)) THEN
                        SCTR_TEMP = 29
                      ELSEIF (( H .EQ. 1 ) .AND. (S.EQ.29)) THEN
                        SCTR_TEMP = 27
                      ENDIF
                      SCTR_TEMP= COS_SCTR_FLIP(SCTR_TEMP,H)
                    ELSEIF ( DO_TB_SECT_FLIP ) THEN
                      SCTR_TEMP= TB_SCTR_FLIP(S)
                    ELSE
                    ENDIF
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
      IF (IFL.EQ.2) GO TO 10
      UBIT = IBITS(ADDR,15,1)
      DETT = IBITS(ADDR,13,2)
      HALF = IBITS(ADDR,11,1)
      UNIT = IBITS(ADDR,10,1)
C
      IF (UNIT.EQ.0) THEN             ! Theta chamber
        WIRE = IBITS(ADDR,0,4)
        SCTR = IBITS(ADDR,4,3)
        QDRT = IBITS(ADDR,7,3)
        IF ( DO_TB_QUAD_FLIP ) THEN
          QDRT = TB_QDRT_FLIP( QDRT )
        ENDIF
      ELSE IF (UNIT.EQ.1) THEN        ! Phi chamber
        WIRE = IBITS(ADDR,0,4)
        SCTR = IBITS(ADDR,4,6)
        IF ( DO_COS_SECT_FLIP ) THEN
          SCTR = COS_SCTR_FLIP(SCTR,HALF)
          IF     (( HALF .EQ. 1 ) .AND. (SCTR.EQ.27)) THEN
            SCTR = 29
          ELSEIF (( HALF .EQ. 1 ) .AND. (SCTR.EQ.29)) THEN
            SCTR = 27
          ENDIF
        ELSEIF ( DO_TB_SECT_FLIP ) THEN
          SCTR = TB_SCTR_FLIP(SCTR)
        ENDIF
        QDRT = 0
      ENDIF
      GO TO 999
C
   10 CONTINUE
C
      ADDR = 0
C
      IF (HALF.EQ.0.OR.HALF.EQ.1) THEN
C
        IF (UNIT.EQ.0) THEN
C Check input.  Wires include delay lines.  Sectors 6 and 7 are unused delay
C lines.
          IF ((WIRE.GE.0.AND.WIRE.LE.9).AND.
     &      (SCTR.GE.0.AND.SCTR.LE.7).AND.
     &      (QDRT.GE.0.AND.QDRT.LE.7)) THEN
C
            ADDR = THETA_ADDRESS(WIRE,SCTR,QDRT,HALF)
          END IF
C
        ELSE IF (UNIT.EQ.1) THEN
C Check input.
          IF ((WIRE.GE.0.AND.WIRE.LE.15).AND.
     &        (SCTR.GE.0.AND.SCTR.LE.35)) THEN
C
            ADDR = PHI_ADDRESS(WIRE,SCTR,HALF)
          END IF
        END IF
      END IF
C
      IF (UBIT.NE.0) THEN
        ADDR = ADDR + UBIT*POWER2(15)
      END IF
C
      RETURN
C------------------------------------------------------------------------
      ENTRY FCODER_INI(RUNTYPE,RUN)
C
C  Called from FTRPAR to initialize fcoder
C----------------------------------------------------------------------
      IF (  (RUNTYPE .EQ. 3)
     &      .OR. (RUNTYPE .EQ. 4) ) THEN
        DO_COS_SECT_FLIP = .TRUE.
        DO_TB_SECT_FLIP = .FALSE.
        DO_TB_QUAD_FLIP = .FALSE.
      ELSE IF (RUNTYPE .EQ. 2) THEN
        DO_COS_SECT_FLIP = .FALSE.
        DO_TB_SECT_FLIP = (RUN.GE.1000689).AND.(RUN.LE.1000843)
        DO_TB_QUAD_FLIP = (RUN.GE.1003860).AND.(RUN.LE.1005283)
      ELSE
        DO_COS_SECT_FLIP = .FALSE.
        DO_TB_SECT_FLIP = .FALSE.
        DO_TB_QUAD_FLIP = .FALSE.
      ENDIF
C
  999 RETURN
      END
