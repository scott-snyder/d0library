      SUBROUTINE MOTMOD(IGO,IMOD,ITRIG)
C-----------------------------------------------------------------------
C-   Purpose and Methods : Store modules used in trigger
C-
C-   Inputs  :  IGO    - 1 = Initialize 
C-                       2 = Set module from CCT list
C-                       3 = Set module from OTC chains
C-                       4 = Set single module
C-                       5 = Read module
C-              IMOD   - Module number, trig region for IGO=2,3
C-              ITRIG  - IGO=2:latch, IGO=3:switch, IGO=4:Bit#
C- 
C-   Output  :  ITRIG  - Trigger tag word
C-                       Bit 0 : CCT octant set
C-                       Bit 1 : OTC octant set
C-                       Bit 2 : OTC low pt set
C-                       Bit 3 : OTC high pt set
C-
C-   Created:   19-Feb-1994  Mike Fortner
C-   Modified:  MF 4/95 Fix SAMUS u-plane set
C-
C-----------------------------------------------------------------------
C
      IMPLICIT NONE
      INTEGER IGO,IMOD,ITRIG
      INTEGER MOTSET(460)
      SAVE MOTSET
      INTEGER ILATCH,IREG,JMOD,JSTA
      INTEGER I,J,K,ILOOP,JLOOP
C
C                Initialize array
C
      IF (IGO.EQ.1) THEN
          DO I = 1,460
              MOTSET(I) = 0
          ENDDO
C
C                Set list of level 1 modules
C
      ELSE IF (IGO.EQ.2) THEN
        ILATCH = ITRIG
        IF (BTEST(ILATCH,0).OR.BTEST(ILATCH,1)) THEN
          IREG = -IMOD
          ILOOP = 8
          IF (IMOD.GT.1) ILOOP = 4
          DO I = 1,ILOOP
            IF (BTEST(ILATCH,I+11)) THEN
              CALL MOTDAT(IREG,I,0,0,JLOOP)
              DO J = 1,JLOOP
                CALL MOTDAT(IREG,I,0,J,JMOD)
                IF (JMOD.NE.0) MOTSET(JMOD) = IBSET(MOTSET(JMOD),0)
                IF (JMOD.GE.400) THEN
                  JSTA = 10*(JMOD/10)
                  DO K=0,6,2
                    MOTSET(JSTA+K) = IBSET(MOTSET(JSTA+K),0)
                  ENDDO
                ENDIF
              ENDDO
            ENDIF
          ENDDO
        ENDIF
C
C                Set list of level 1.5 modules
C
      ELSE IF (IGO.EQ.3) THEN
        IREG = IMOD
        DO I = 1,3
          DO J = 1,5
            CALL MOTDAT(IREG,ITRIG,I,J,JMOD)
            IF (JMOD.NE.0) MOTSET(JMOD) = IBSET(MOTSET(JMOD),1)
            IF (JMOD.GE.400) THEN
              JSTA = 10*(JMOD/10)
              DO K=0,6,2
                MOTSET(JSTA+K) = IBSET(MOTSET(JSTA+K),1)
              ENDDO
            ENDIF
          ENDDO
        ENDDO
C
C                Set single module bit
C
      ELSE IF (IGO.EQ.4) THEN
          IF (IMOD.LE.0.OR.IMOD.GT.460) RETURN
          MOTSET(IMOD) = IBSET(MOTSET(IMOD),ITRIG)
          IF (IMOD.GE.400) THEN
            JSTA = 10*(IMOD/10)
            DO K=0,6,2
              MOTSET(JSTA+K) = IBSET(MOTSET(JSTA+K),ITRIG)
            ENDDO
            IF (IMOD-JSTA.GE.4) THEN
              MOTSET(IMOD-4) = IBSET(MOTSET(IMOD-4),ITRIG)
            ELSE
              MOTSET(IMOD+4) = IBSET(MOTSET(IMOD+4),ITRIG)
            ENDIF
          ENDIF
C
C                Read trigger value
C
      ELSE IF (IGO.EQ.5) THEN
          IF (IMOD.LE.0.OR.IMOD.GT.460) RETURN
          ITRIG = MOTSET(IMOD)
C
C
      ENDIF
C
      RETURN
      END
