      SUBROUTINE L1DMP_REFSET(LUN, SET_TYPE, SET_NUMBER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Writes the definition of the given reference set.
C-
C-   Inputs  : LUN         The unit number to write output to.
C-   Outputs : File output
C-   Controls: SET_TYPE    What kind of reference set (EM ET, TOT ET...)
C-                      One of: EM_ET_REF_MIN, TOT_ET_REF_MIN, LT_REF_MIN
C-             SET_NUMBER  The reference set number [RS_SET_MIN..RS_SET_MAX]
C-
C-   Created   9-JAN-1991   Level 1 Simulator, Michigan State University,
C-                      Philippe Laurens, Steven Klocek,
C-                      L1C -> L1SIM, and major upgrade to use LSM, COOR_sim
C-   Updated  12-NOV-1991   Philippe Laurens, Steven Klocek   
C-                    - Prints header slightly differently.
C-                    - If the entire Reference Set is 0, this routine now
C-                        prints UNUSED.
C-                    - Prints '.GE.' and '.LT.' in regards to the thresholds.
C-   Updated  26-NOV-1991   Philippe Laurens, Steven Klocek   
C-                    - Removes offsets from Reference Set output.
C-                    - Identifies thresholds which were not defined in
C-                      programming file as unused.
C-                    - Corrects for scale of TOT Et threshold values.
C-   Updated   6-JUL-1993   Philippe Laurens - MSU L1 Trigger  
C-                    - add large tile option. 
C-                    - Use a simple approach. "Piggy back" on the work already
C-                      done. The loops looking for Trigger Tower eta, phi
C-                      blocks are not modified. Large Tile coordinates are
C-                      derived each time so that the same comparisons are
C-                      repeated until a LT boundary is reached. This causes
C-                      extra work, but no harm.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:LEVEL1_LOOKUP.PARAMS'
      INCLUDE 'D0$INC:LEVEL1_LOOKUP.INC'
      INCLUDE 'D0$PARAMS:L1_CALTRIG.PARAMS'
      INCLUDE 'D0$PARAMS:L1_FRAMEWORK.PARAMS'
      INCLUDE 'D0$INC:L1FW_ANDOR_AND_MISC.INC'
      INCLUDE 'D0$INC:L1C_GLOBAL_ENERGY_THRESHOLDS.INC'
      INCLUDE 'D0$INC:L1C_REFSET_AND_COUNT_THRESHOLDS.INC'
      INCLUDE 'D0$PARAMS:L1DBB_DATA_BLOCK.PARAMS'
      INCLUDE 'D0$INC:L1DBB_DATA_BLOCK.INC'
      INCLUDE 'D0$INC:L1_SPECIFIC_TRIGGER.INC'
      INTEGER TRULEN
      EXTERNAL TRULEN
C
      INTEGER THRESH
      INTEGER SET_TYPE, SET_NUMBER, LUN
      INTEGER ETA, PHI, SIGN
      INTEGER ETA_BEGIN, ETA_END, PHI_BEGIN, PHI_END
      LOGICAL SYMMETRIC_SIGN
      LOGICAL ALL_ZERO
      INTEGER ISTAT
C
      INTEGER HIGH_THRESH
      PARAMETER (HIGH_THRESH = 10000)
C
C     The following are statement functions
      INTEGER LT_PHI, LT_ETA
      LT_ETA(ETA) = (ETA-1)/TT_ETA_PER_LT + 1
      LT_PHI(PHI) = (PHI-1)/TT_PHI_PER_LT + 1
C
C       Reset margins and clear screen
C
C       Print Headers
C
  800 FORMAT(' ', A, I2)
      IF (SET_TYPE .EQ. EM_ET_REF_MIN) THEN
        WRITE (LUN,800,IOSTAT=ISTAT) 'EM Et  Ref Set # ', SET_NUMBER
      ELSE IF (SET_TYPE .EQ. TOT_ET_REF_MIN) THEN
        WRITE (LUN,800,IOSTAT=ISTAT) 'TOT Et Ref Set # ', SET_NUMBER
      ELSE 
        WRITE (LUN,800,IOSTAT=ISTAT) 'Large Tile Ref Set # ', SET_NUMBER
      ENDIF
      WRITE (LUN,*) '-------------------'
C
C       Check for the Ref Set completely zero
C
      ALL_ZERO = .TRUE.
C
      DO PHI = PHI_MIN, PHI_MAX
        DO ETA = ETA_MIN, ETA_MAX
          DO SIGN = POS_ETA, NEG_ETA
            IF (SET_TYPE .EQ. EM_ET_REF_MIN) THEN
              IF ( EMT_THRSHLD( SIGN, ETA, PHI, 
     &                          SET_NUMBER + EM_ET_REF_MIN) 
     &           .NE. 255) GOTO 90
              IF ( HDT_VETO( SIGN, ETA, PHI, 
     &                       SET_NUMBER + EM_ET_REF_MIN) 
     &           .NE. 0) GOTO 90
            ELSE IF (SET_TYPE .EQ. TOT_ET_REF_MIN) THEN
              IF ( TOT_THRSHLD( SIGN, ETA, PHI, 
     &                          SET_NUMBER + TOT_ET_REF_MIN) 
     &           .NE. 255) GOTO 90
            ELSE
              IF ( LT_THRSHLD( SIGN, LT_ETA(ETA), LT_PHI(PHI), 
     &                         SET_NUMBER + LT_REF_MIN) 
     &           .LE. 4 * 8 * 255 ) GOTO 90
            ENDIF
          END DO
        END DO
      END DO
C
C       If execution reaches here, then the Ref Set is completely ZERO
C
      WRITE (LUN,*) 'UNUSED'
      GOTO 999
C
C       Remove all offsets, and unify to 1/4 GeV per count, 
C       note that this modified the *_THRSH variables 
C       which will thus have to be restored later.
C
   90 CONTINUE
      IF (SET_TYPE .NE. LT_REF_MIN) THEN
        DO PHI = PHI_MIN, PHI_MAX
          DO ETA = ETA_MIN, ETA_MAX
            DO SIGN = POS_ETA, NEG_ETA
              IF (SET_TYPE .EQ. EM_ET_REF_MIN) THEN
                THRESH = EMT_THRSHLD( SIGN, ETA, PHI, 
     &                                SET_NUMBER + EM_ET_REF_MIN) + 1
                IF (THRESH .EQ. 256) THEN
                  THRESH = HIGH_THRESH
                ELSE
                  THRESH = THRESH  
     &                   - LOOKUP_ZERESP( SIGN, ETA, PHI, EM_ET_QUANT)
                ENDIF
                EMT_THRSHLD( SIGN, ETA, PHI, SET_NUMBER + EM_ET_REF_MIN)
     &                   = THRESH
C
                THRESH = HDT_VETO( SIGN, ETA, PHI, 
     &                             EM_ET_REF_MIN + SET_NUMBER) + 1
                IF (THRESH .EQ. 256) THEN
                  THRESH = HIGH_THRESH
                ELSE
                  THRESH = THRESH 
     &                   - LOOKUP_ZERESP( SIGN, ETA, PHI, HD_ET_QUANT)
                ENDIF
                HDT_VETO( SIGN, ETA, PHI, EM_ET_REF_MIN + SET_NUMBER) 
     &                   = THRESH
C
              ELSE IF ( SET_TYPE .EQ. TOT_ET_REF_MIN ) THEN 
                THRESH = TOT_THRSHLD( SIGN, ETA, PHI, 
     &                                TOT_ET_REF_MIN + SET_NUMBER) + 1
                IF (THRESH .EQ. 256) THEN
                  THRESH = HIGH_THRESH
                ELSE
                  THRESH = THRESH 
     &                   - ( LOOKUP_ZERESP(SIGN, ETA, PHI, HD_ET_QUANT)
     &                     + LOOKUP_ZERESP(SIGN, ETA, PHI, EM_ET_QUANT)
     &                     ) / 2
                ENDIF
                TOT_THRSHLD( SIGN, ETA, PHI, TOT_ET_REF_MIN+SET_NUMBER) 
     &                   = THRESH
              ENDIF
            END DO
          END DO
        END DO
      ELSE
        DO PHI = LT_PHI_MIN, LT_PHI_MAX
          DO ETA = LT_ETA_MIN, LT_ETA_MAX
            DO SIGN = POS_ETA, NEG_ETA
              THRESH = LT_THRSHLD( SIGN, ETA, PHI, 
     &                             SET_NUMBER + LT_REF_MIN) 
              IF (THRESH .GT. 4 * 8 * 255 ) THEN
                THRESH = HIGH_THRESH
              ELSE
                THRESH = THRESH 
     &                 - LT_ZERESP( SIGN, ETA, PHI )
              ENDIF
              LT_THRSHLD( SIGN, ETA, PHI, LT_REF_MIN + SET_NUMBER) 
     &                 = THRESH
            END DO
          END DO
        END DO
      ENDIF
C
C       Check for symmetry over SIGN_ETA
C
      SYMMETRIC_SIGN = .TRUE.
      DO PHI = PHI_MIN, PHI_MAX
        DO ETA = ETA_MIN, ETA_MAX
          IF (SET_TYPE .EQ. EM_ET_REF_MIN) THEN
            IF ((EMT_THRSHLD(POS_ETA, ETA, PHI, SET_NUMBER 
     &                                          + EM_ET_REF_MIN)
     &        .NE. EMT_THRSHLD(NEG_ETA, ETA, PHI, SET_NUMBER 
     &                                          + EM_ET_REF_MIN)) .OR.
     &        (HDT_VETO(POS_ETA, ETA, PHI, SET_NUMBER + EM_ET_REF_MIN)
     &        .NE. HDT_VETO(NEG_ETA, ETA, PHI, SET_NUMBER
     &                                          +EM_ET_REF_MIN))) THEN
              SYMMETRIC_SIGN = .FALSE.
              GOTO 100
            ENDIF
          ELSE IF ( SET_TYPE .EQ. TOT_ET_REF_MIN ) THEN 
            IF (TOT_THRSHLD(POS_ETA, ETA, PHI, SET_NUMBER 
     &                                      + TOT_ET_REF_MIN)
     &        .NE. TOT_THRSHLD(NEG_ETA, ETA, PHI, SET_NUMBER
     &                                      + TOT_ET_REF_MIN)) THEN
              SYMMETRIC_SIGN = .FALSE.
              GOTO 100
            ENDIF
          ELSE 
            IF ( LT_THRSHLD( POS_ETA, LT_ETA(ETA), LT_PHI(PHI), 
     &                       SET_NUMBER + LT_REF_MIN)
     &         .NE. LT_THRSHLD( NEG_ETA, LT_ETA(ETA), LT_PHI(PHI), 
     &                          SET_NUMBER + LT_REF_MIN) ) THEN
              SYMMETRIC_SIGN = .FALSE.
              GOTO 100
            ENDIF
          ENDIF
        END DO
      END DO
  100 CONTINUE 
C
C       Will later change SIGN to minus and repeat following block, if there is
C       no symmetry over sign_eta
C
      SIGN = POS_ETA
C
C       Check the symmetry over ETA, and find a range which is symmetric
C       
      DO WHILE (.TRUE.)
        WRITE (LUN,*)
        IF (SYMMETRIC_SIGN .EQV. .TRUE.) THEN
          WRITE (LUN,*) 'eta_sign positive and negative'
        ELSE IF (SIGN .EQ. POS_ETA) THEN
          WRITE (LUN,*) 'eta_sign positive'
        ELSE
          WRITE (LUN,*) 'eta_sign negative'
        ENDIF
C
        ETA_BEGIN = ETA_MIN
        ETA_END = ETA_MIN
C
        DO WHILE (.TRUE.)
          DO WHILE (.TRUE.)
            IF (ETA_END .EQ. ETA_MAX) GOTO 200
            DO PHI = PHI_MIN, PHI_MAX
              IF (SET_TYPE .EQ. EM_ET_REF_MIN) THEN
                IF ((EMT_THRSHLD(SIGN, ETA_END, PHI, SET_NUMBER 
     &                                          + EM_ET_REF_MIN)
     &            .NE. EMT_THRSHLD(SIGN, ETA_END+1, PHI, SET_NUMBER
     &                                          + EM_ET_REF_MIN))
     &          .OR. (HDT_VETO(SIGN, ETA_END, PHI, SET_NUMBER 
     &                                          + EM_ET_REF_MIN)
     &            .NE. HDT_VETO(SIGN, ETA_END+1, PHI, SET_NUMBER
     &                                          + EM_ET_REF_MIN))) THEN
                  GOTO 200
                ENDIF
              ELSE IF ( SET_TYPE .EQ. TOT_ET_REF_MIN ) THEN 
                IF (TOT_THRSHLD(SIGN, ETA_END, PHI, SET_NUMBER
     &                                          + TOT_ET_REF_MIN)
     &            .NE. TOT_THRSHLD(SIGN, ETA_END+1, PHI, SET_NUMBER
     &                                          + TOT_ET_REF_MIN)) THEN
                  GOTO 200
                ENDIF
              ELSE
                IF ( LT_THRSHLD( SIGN, LT_ETA(ETA_END), LT_PHI(PHI), 
     &                           SET_NUMBER + LT_REF_MIN )
     &            .NE. LT_THRSHLD( SIGN, LT_ETA(ETA_END+1), LT_PHI(PHI),
     &                             SET_NUMBER + LT_REF_MIN)) THEN
                  GOTO 200
                ENDIF
              ENDIF
            END DO
            ETA_END = ETA_END + 1
          END DO
C
C       We now have a symmetric range ETA_BEGIN to ETA_END
C
  200     CONTINUE
          IF (ETA_BEGIN .NE. ETA_MIN) THEN
            WRITE (LUN,*)
          ENDIF
C
  810     FORMAT(' ', A, I3)
  820     FORMAT(' ', A, I3, A, I3)
          IF (ETA_BEGIN .EQ. ETA_END) THEN
            WRITE (LUN,810,IOSTAT=ISTAT) '  eta_magnitude', ETA_BEGIN
          ELSE
            WRITE (LUN,820,IOSTAT=ISTAT) '  eta_magnitude', ETA_BEGIN,
     &        ' to', ETA_END
          ENDIF
C
          PHI_BEGIN = PHI_MIN
          PHI_END = PHI_MIN
C
C       Find a symmetric range of PHI
          DO WHILE (.TRUE.)
            DO WHILE (.TRUE.)
              IF (PHI_END .EQ. PHI_MAX) GOTO 300
              IF (SET_TYPE .EQ. EM_ET_REF_MIN) THEN
                IF ((EMT_THRSHLD(SIGN, ETA_END, PHI_END, SET_NUMBER
     &                                               + EM_ET_REF_MIN)
     &            .NE. EMT_THRSHLD(SIGN, ETA_END, PHI_END+1, SET_NUMBER
     &                                               + EM_ET_REF_MIN))
     &          .OR. (HDT_VETO(SIGN, ETA_END, PHI_END, SET_NUMBER
     &                                               + EM_ET_REF_MIN)
     &            .NE. HDT_VETO(SIGN, ETA_END, PHI_END+1, SET_NUMBER
     &                                       + EM_ET_REF_MIN))) THEN
                  GOTO 300
                ENDIF
              ELSE IF ( SET_TYPE .EQ. TOT_ET_REF_MIN ) THEN 
                IF (TOT_THRSHLD(SIGN, ETA_END, PHI_END, SET_NUMBER
     &                                               + TOT_ET_REF_MIN)
     &            .NE. TOT_THRSHLD(SIGN, ETA_END, PHI_END+1, SET_NUMBER
     &                                  + TOT_ET_REF_MIN)) THEN
                  GOTO 300
                ENDIF
              ELSE 
                IF (LT_THRSHLD( SIGN, LT_ETA(ETA_END), LT_PHI(PHI_END), 
     &                          SET_NUMBER + LT_REF_MIN)
     &            .NE. LT_THRSHLD( SIGN, LT_ETA(ETA_END), 
     &                             LT_PHI(PHI_END+1), 
     &                             SET_NUMBER + LT_REF_MIN)) THEN
                  GOTO 300
                ENDIF
              ENDIF
              PHI_END = PHI_END + 1
            END DO
C
C       Now have a range of phi from PHI_BEGIN to PHI_END
C
  300       CONTINUE
            IF (PHI_BEGIN .EQ. PHI_END) THEN
              WRITE (LUN,810,IOSTAT=ISTAT) '    phi', PHI_END
            ELSE
              WRITE (LUN,820,IOSTAT=ISTAT) '    phi',PHI_BEGIN,' to',
     &          PHI_END
            ENDIF
C
C       Write out the thresholds
C
  350       FORMAT('       EM Et Threshold =  .GE.', F8.2, 
     &        ' GeV     HD Et Veto =  .LT. ', F8.2, ' GeV')
  360       FORMAT('       TOT Threshold =  .GE.', F8.2, ' GeV')
  370       FORMAT('       EM Et Threshold =  .GE.', F8.2,
     &        ' GeV     HD Et Veto =  None')
  380       FORMAT('       No Contribution')
  390       FORMAT('       LT Threshold =  .GE.', F8.2, ' GeV')
C
C
            IF (SET_TYPE .EQ. EM_ET_REF_MIN) THEN
              IF (HDT_VETO(SIGN, ETA_END, PHI_END, SET_NUMBER 
     &          + EM_ET_REF_MIN) .NE. HIGH_THRESH) THEN
                IF (EMT_THRSHLD(SIGN, ETA_END, PHI_END, SET_NUMBER 
     &            + EM_ET_REF_MIN) .NE. HIGH_THRESH) THEN
                  WRITE (LUN,350,IOSTAT=ISTAT) 
     &                    ( EMT_THRSHLD( SIGN, ETA_END, PHI_END, 
     &                                   SET_NUMBER + EM_ET_REF_MIN)
     &                    * GLOBAL_ENERGY_SCALE(EM_ET_QUANT) ),
     &                    ( HDT_VETO(    SIGN, ETA_END, PHI_END, 
     &                                   SET_NUMBER + EM_ET_REF_MIN) 
     &                    * GLOBAL_ENERGY_SCALE(HD_ET_QUANT) )
                ELSE
                  WRITE (LUN,380,IOSTAT=ISTAT)
                ENDIF
              ELSE
                IF (EMT_THRSHLD(SIGN, ETA_END, PHI_END, SET_NUMBER
     &            + EM_ET_REF_MIN) .NE. HIGH_THRESH) THEN
                  WRITE (LUN,370,IOSTAT=ISTAT) 
     &                    ( EMT_THRSHLD( SIGN, ETA_END, PHI_END, 
     &                                   SET_NUMBER + EM_ET_REF_MIN)
     &                    * GLOBAL_ENERGY_SCALE(EM_ET_QUANT) )
                ELSE
                  WRITE (LUN,380,IOSTAT=ISTAT)
                ENDIF
              ENDIF
            ELSE IF ( SET_TYPE .EQ. TOT_ET_REF_MIN ) THEN 
              IF (TOT_THRSHLD(SIGN, ETA_END, PHI_END, SET_NUMBER +
     &          TOT_ET_REF_MIN) .NE. HIGH_THRESH) THEN
                WRITE (LUN,360,IOSTAT=ISTAT) 
     &                    ( TOT_THRSHLD( SIGN, ETA_END, PHI_END, 
     &                                   SET_NUMBER + TOT_ET_REF_MIN) 
     &                    * GLOBAL_ENERGY_SCALE(PX_QUANT) ) 
              ELSE
                WRITE (LUN, 380, IOSTAT=ISTAT)
              ENDIF
            ELSE 
              IF ( LT_THRSHLD( SIGN, LT_ETA(ETA_END), LT_PHI(PHI_END),
     &                         SET_NUMBER + LT_REF_MIN) 
     &           .NE. HIGH_THRESH) THEN
                WRITE (LUN,390,IOSTAT=ISTAT) 
     &                    ( LT_THRSHLD(  SIGN, LT_ETA(ETA_END), 
     &                                   LT_PHI(PHI_END), 
     &                                   SET_NUMBER + LT_REF_MIN) 
     &                    * GLOBAL_ENERGY_SCALE(PX_QUANT) )
              ELSE
                WRITE (LUN, 380, IOSTAT=ISTAT)
              ENDIF
            ENDIF
C
C       Advance the ranges and check for more symmetries
C
            IF (PHI_END .LT. PHI_MAX) THEN
              PHI_BEGIN = PHI_END + 1
              PHI_END = PHI_END + 1
            ELSE
              GOTO 400
            ENDIF
          END DO
C
C       Advance the ETA range
C
  400     CONTINUE
          IF (ETA_END .LT. ETA_MAX) THEN
            ETA_BEGIN = ETA_END + 1
            ETA_END = ETA_END + 1
          ELSE
            GOTO 450
          ENDIF
        END DO
  450   CONTINUE
        IF ((SYMMETRIC_SIGN .EQV. .TRUE.) .OR. (SIGN .EQ.
     &    NEG_ETA)) THEN
          GOTO 500
        ELSE
          SIGN = NEG_ETA
        ENDIF
      END DO
C
C
C       Restore the offsets
C
  500 CONTINUE
      IF (SET_TYPE .NE. LT_REF_MIN) THEN
        DO PHI = PHI_MIN, PHI_MAX
          DO ETA = ETA_MIN, ETA_MAX
            DO SIGN = POS_ETA, NEG_ETA
              IF (SET_TYPE .EQ. EM_ET_REF_MIN) THEN
                THRESH = EMT_THRSHLD( SIGN, ETA, PHI, 
     &                                SET_NUMBER + EM_ET_REF_MIN)
                IF (THRESH .EQ. HIGH_THRESH) THEN
                  THRESH = 255
                ELSE
                  THRESH = THRESH - 1
     &                   + LOOKUP_ZERESP( SIGN, ETA, PHI, EM_ET_QUANT) 
                ENDIF
                EMT_THRSHLD( SIGN, ETA, PHI, SET_NUMBER + EM_ET_REF_MIN)
     &                   = THRESH
C
                THRESH = HDT_VETO( SIGN, ETA, PHI, 
     &                             EM_ET_REF_MIN + SET_NUMBER) 
                IF (THRESH .EQ. HIGH_THRESH) THEN
                  THRESH = 255
                ELSE
                  THRESH = THRESH - 1
     &                   + LOOKUP_ZERESP( SIGN, ETA, PHI, HD_ET_QUANT) 
                ENDIF
                HDT_VETO( SIGN, ETA, PHI, EM_ET_REF_MIN + SET_NUMBER) 
     &                   = THRESH
C
              ELSE IF ( SET_TYPE .EQ. TOT_ET_REF_MIN ) THEN 
                THRESH = TOT_THRSHLD( SIGN, ETA, PHI, 
     &                                TOT_ET_REF_MIN + SET_NUMBER) 
                IF (THRESH .EQ. HIGH_THRESH) THEN
                  THRESH = 255
                ELSE
                  THRESH = THRESH - 1 
     &                   + ( LOOKUP_ZERESP(SIGN, ETA, PHI, HD_ET_QUANT)
     &                     + LOOKUP_ZERESP(SIGN, ETA, PHI, EM_ET_QUANT) 
     &                     ) / 2 
                ENDIF
                TOT_THRSHLD( SIGN, ETA, PHI, TOT_ET_REF_MIN+SET_NUMBER)
     &                   = THRESH
              END IF
            END DO
          END DO
        END DO
      ELSE
        DO PHI = LT_PHI_MIN, LT_PHI_MAX
          DO ETA = LT_ETA_MIN, LT_ETA_MAX
            DO SIGN = POS_ETA, NEG_ETA
              THRESH = LT_THRSHLD( SIGN, ETA, PHI, 
     &                             LT_REF_MIN + SET_NUMBER) 
              IF (THRESH .EQ. HIGH_THRESH) THEN
                THRESH = 4*8*255 + 1
              ELSE
                THRESH = THRESH 
     &                 + LT_ZERESP( SIGN, ETA, PHI)
              ENDIF
              LT_THRSHLD( SIGN, ETA, PHI, LT_REF_MIN + SET_NUMBER) 
     &                 = THRESH
            END DO
          END DO
        END DO
      ENDIF
C----------------------------------------------------------------------
  999 CONTINUE
      RETURN
      END
