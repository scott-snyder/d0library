      SUBROUTINE L1COOR_REFSET_INIT   (THRTYP)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Define a reference set from the parsed configuration
C-      file.
C-
C-   Inputs  : THRTYP   The type of threshold being defined, 
C-                      use COOR_RSEMET, COOR_RSHDVETO, COOR_RSEMET, or
C-                      COOR_RSLGTILE  cf. L1COOR_PARSER.INC
C-                      
C-   Outputs : set flag PARSE_STATUS in L1COOR_PARSER.INC
C-   Controls: none
C-
C- ENTRY L1COOR_REFSET_VERIFY (THRTYP)
C-                      skip threshold assignement while still checking syntax
C-                      
C-   Created  31-JUL-1991   Level 1 Simulator, Michigan State University,
C-                      Philippe Laurens, Steven Klocek,
C-                      L1C -> L1SIM, and major upgrade to use LSM, COOR_sim
C-   Updated  18-NOV-1991   Philippe Laurens, Steven Klocek   
C-                      Added the entry point L1COOR_REFSET_VERIFY to verify
C-                      an input list without performing any assignments.
C-   Updated  17-DEC-1991   Philippe Laurens, Steven Klocek   
C-                      Check to see if the last thing to occur was a threshold
C-                      assignment.
C-   Updated  16-JAN-1992   Philippe Laurens, Steven Klocek   
C-                      Replace call to ABORT with call to D0_ABORT
C-   Updated  28-JUN-1993   Philippe Laurens - MSU L1 Trigger   
C-                      Avoid duplicating code in entry point
C-                      L1COOR_REFSET_VERIFY by setting a special flag to skip
C-                      assignment, and require argument to know when it is for
C-                      Large Tiles.
C-   updated  6-Jan-1994    Zhengzhi Zhang, added parsing L1.5 thresholds
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:LEVEL1_LOOKUP.PARAMS'
      INCLUDE 'D0$PARAMS:L1_CALTRIG.PARAMS'
      INCLUDE 'D0$PARAMS:L1_FRAMEWORK.PARAMS'
      INCLUDE 'D0$PARAMS:L1COOR_PARSER.PARAMS'
      INCLUDE 'D0$INC:L1COOR_PARSER.INC'
      INCLUDE 'D0$INC:L1C_REFSET_AND_COUNT_THRESHOLDS.INC'
      INCLUDE 'D0$PARAMS:L15coor_parser.PARAMS'
      INCLUDE 'D0$INC:L15C_REFSET_THRESHOLDS.INC'

C
      INTEGER THRTYP
C
      LOGICAL MAGN_ETA_L(ETA_MIN:ETA_MAX)
      LOGICAL SIGN_ETA_L(POS_ETA:NEG_ETA)
      LOGICAL PHI_L(PHI_MIN:PHI_MAX)
      INTEGER MAGN_ETA, SIGN_ETA, PHI
      INTEGER COUNT, LAST_OBJECT
      INTEGER THRESHOLD
      LOGICAL ASSIGNMENT_OCCURRED
      LOGICAL SKIP_ASSIGNMENTS
      INTEGER L15_CRATE_NUM, L15_TERM_NUM, L15_TYPE
C----------------------------------------------------------------------
C
C   This is where the entry point L1COOR_REFSET_VERIFY catches up 
C   with the (same) code, after setting the SKIP_ASSIGNMENTS flag to TRUE 
C
      SKIP_ASSIGNMENTS = .FALSE. 
   10 CONTINUE

C       The default is to use the entire range
      DO SIGN_ETA = POS_ETA, NEG_ETA
        SIGN_ETA_L(SIGN_ETA) = .TRUE.
      END DO
C
      DO MAGN_ETA = ETA_MIN, ETA_MAX
        IF (THRTYP .EQ. COOR_RSLGTILE) THEN
C       For large tiles, only set mask for the lower magn_eta of each tile.
          IF ( (MAGN_ETA - 1 ) 
     &    .EQ. (TT_ETA_PER_LT * ( (MAGN_ETA-1) / TT_ETA_PER_LT) ) ) THEN
            MAGN_ETA_L(MAGN_ETA) = .TRUE.
          ELSE 
            MAGN_ETA_L(MAGN_ETA) = .FALSE.
          ENDIF
        ELSE 
          MAGN_ETA_L(MAGN_ETA) = .TRUE.
        ENDIF
      END DO
C
      DO PHI = PHI_MIN, PHI_MAX
        IF (THRTYP .EQ. COOR_RSLGTILE) THEN
C       For large tiles, only set mask for the lower phi of a tile.
          IF ( (PHI - 1 ) 
     &    .EQ. (TT_PHI_PER_LT * ((PHI-1) / TT_PHI_PER_LT) ) ) THEN
            PHI_L(PHI) = .TRUE.
          ELSE 
            PHI_L(PHI) = .FALSE.
          ENDIF
        ELSE 
          PHI_L(PHI) = .TRUE.
        ENDIF
      END DO
C
C       Go through list one entry at a time. Perform processing based on what
C       the current object is
      LAST_OBJECT = NO_OBJECT
      DO COUNT = 1, LIST_TOP
C
C       Reset that an assignment occurred
        ASSIGNMENT_OCCURRED = .FALSE.
C       Negated objects are an error
        IF (LIST(COUNT,OBJ_T_INDEX) .EQ. PARSE_NEGATED) THEN
          PARSE_STATUS = PARSE_BAD_PARAM
          GOTO 999
        ENDIF
C       Empty parenthesis are an error 
        IF ((LIST(COUNT,ITEM_T_INDEX) .EQ. PARSE_EMPTY_PAREN) .OR.
     &    (LIST(COUNT,ITEM_T_INDEX) .EQ. PARSE_NO_PAREN)) THEN
          PARSE_STATUS = PARSE_BAD_FORMAT
          GOTO 999
        ENDIF
C       Skip anything which is an END_RANGE
        IF (LIST(COUNT,ITEM_T_INDEX) .EQ. PARSE_UPPER_BOUNDARY) THEN
          LAST_OBJECT = LIST(COUNT,OBJ_INDEX)
C
C       Assign thresholds if the object is an integer
        ELSEIF (LIST(COUNT,OBJ_T_INDEX) .EQ. PARSE_ASSERTED)THEN
C
          IF (THRTYP .NE. COOR_RSLGTILE) THEN
            IF (  (LIST(COUNT,OBJ_INDEX) .LT. RS_SET_MIN) 
     &       .OR. (LIST(COUNT,OBJ_INDEX) .GT. RS_SET_MAX) ) THEN
              PARSE_STATUS = PARSE_BAD_PARAM
              GOTO 999
            ENDIF
          ELSE
            IF ( (LIST(COUNT,OBJ_INDEX) .LT. 0 ) 
     &      .OR. (LIST(COUNT,OBJ_INDEX) .GT. (LT_REF_MAX-LT_REF_MIN) ) )
     &      THEN
              PARSE_STATUS = PARSE_BAD_PARAM
              GOTO 999
            ENDIF
          END IF
C
          IF (LIST(COUNT,ITEM_T_INDEX) .NE. PARSE_ASSERTED) THEN
            PARSE_STATUS = PARSE_BAD_PARAM
            GOTO 999
          ENDIF
C
C   Do assignments, unless the entry point L1COOR_REFSET_VERIFY was used
C
          IF ( SKIP_ASSIGNMENTS .EQV. .FALSE. ) THEN
            DO PHI = PHI_MIN, PHI_MAX
              IF (PHI_L(PHI) .EQV. .FALSE.) GOTO 200
              DO MAGN_ETA = ETA_MIN, ETA_MAX
                IF (MAGN_ETA_L(MAGN_ETA) .EQV. .FALSE.) GOTO 210
                DO SIGN_ETA = POS_ETA, NEG_ETA
                  IF (SIGN_ETA_L(SIGN_ETA) .EQV. .FALSE.) GOTO 220
C
                  IF (THRTYP .EQ. COOR_RSEMET) THEN
                    CALL TOWER_THRESHOLD_TRANSLATION(
     &                SIGN_ETA, MAGN_ETA, PHI, TT_EMET_THRTYP, 
     &                FLOAT(LIST(COUNT,ITEM_INDEX)) / 1000.,
     &                EMT_THRSHLD(SIGN_ETA, MAGN_ETA, PHI, 
     &                   EM_ET_REF_MIN+ LIST(COUNT,OBJ_INDEX)))
C
                  ELSEIF (THRTYP .EQ. COOR_RSHDVETO) THEN
                    CALL TOWER_THRESHOLD_TRANSLATION(
     &                SIGN_ETA, MAGN_ETA, PHI, TT_HDVETO_THRTYP, 
     &                FLOAT(LIST(COUNT,ITEM_INDEX)) / 1000.,
     &                HDT_VETO(SIGN_ETA, MAGN_ETA, PHI, 
     &                   EM_ET_REF_MIN+ LIST(COUNT,OBJ_INDEX)))
C
                  ELSEIF (THRTYP .EQ. COOR_RSTOTET) THEN
                    CALL TOWER_THRESHOLD_TRANSLATION(
     &                SIGN_ETA, MAGN_ETA, PHI, TT_TOTET_THRTYP, 
     &                FLOAT(LIST(COUNT,ITEM_INDEX)) / 1000.,
     &                TOT_THRSHLD(SIGN_ETA, MAGN_ETA, PHI, 
     &                   TOT_ET_REF_MIN+ LIST(COUNT,OBJ_INDEX)))
C                  
C     for large tiles, parsing has set the flags for the lower boundary
C     in magn_eta and phi only.
                  ELSEIF (THRTYP .EQ. COOR_RSLGTILE) THEN
                    CALL LGTILE_THRESHOLD_TRANSLATION(
     &                SIGN_ETA, MAGN_ETA, PHI, 
     &                FLOAT(LIST(COUNT,ITEM_INDEX)) / 1000.,
     &                LT_THRSHLD(SIGN_ETA, 
     &                          (MAGN_ETA-1)/TT_ETA_PER_LT+1, 
     &                          (PHI-1)/TT_PHI_PER_LT+1, 
     &                          LT_REF_MIN+LIST(COUNT,OBJ_INDEX)))
C
                  ELSEIF (THRTYP. EQ. L15COOR_REFSET)THEN
                    IF(L15_TYPE .EQ. KEY_EM)
     &                L15_EMT_THRSHLD(L15_CRATE_NUM, L15_TERM_NUM,
     &                  SIGN_ETA, MAGN_ETA, PHI) =
     &                                      LIST(COUNT,ITEM_INDEX)
                    IF(L15_TYPE .EQ. KEY_TOT)
     &                L15_TOT_THRSHLD(L15_CRATE_NUM, L15_TERM_NUM,
     &                  SIGN_ETA, MAGN_ETA, PHI) =
     &                                      LIST(COUNT,ITEM_INDEX)
                    
                  ELSEIF (THRTYP .GE. 0) THEN
                  ELSE ! Should never get here
                    CALL D0_ABORT('Reached unreachable statement in' // 
     &                 'L1COOR_REFSET_INIT.FOR')
                  ENDIF
C
  220             CONTINUE
                END DO
  210           CONTINUE
              END DO
  200         CONTINUE
            END DO
          ENDIF
C
          ASSIGNMENT_OCCURRED = .TRUE.
C
          LAST_OBJECT = NO_OBJECT
C
C       Handle keywords
        ELSEIF (LIST(COUNT,OBJ_T_INDEX) .EQ. PARSE_KEYWORD) THEN
C
C       This item in the list will not cause a threshold assignment
          ASSIGNMENT_OCCURRED = .FALSE.
          IF (LIST(COUNT,OBJ_INDEX) .EQ. KEY_SIGN_ETA) THEN
C       Should have only POS or NEG
            IF ((LIST(COUNT,ITEM_T_INDEX) .NE. PARSE_KEYWORD) 
     &        .OR. ((LIST(COUNT,ITEM_INDEX) .NE. KEY_POS)
     &        .AND. (LIST(COUNT,ITEM_INDEX) .NE. KEY_NEG))) THEN
              PARSE_STATUS = PARSE_BAD_FORMAT
              GOTO 999
            ENDIF
C       Clear range if necessary
            IF (LAST_OBJECT .NE. LIST(COUNT,OBJ_INDEX)) THEN
              SIGN_ETA_L(POS_ETA) = .FALSE.
              SIGN_ETA_L(NEG_ETA) = .FALSE.
            ENDIF
C       Set the indicated entrys true
            IF (LIST(COUNT,ITEM_INDEX) .EQ. KEY_POS) THEN
              SIGN_ETA_L(POS_ETA) = .TRUE.
            ELSE
              SIGN_ETA_L(NEG_ETA) = .TRUE.
            ENDIF
C
            LAST_OBJECT = LIST(COUNT,OBJ_INDEX)
C
C       Handle MAGN_ETA
          ELSEIF (LIST(COUNT,OBJ_INDEX) .EQ. KEY_MAGN_ETA) THEN
C       Clear the variable if necessary
            IF (LAST_OBJECT .NE. LIST(COUNT,OBJ_INDEX)) THEN
              DO MAGN_ETA = ETA_MIN, ETA_MAX
                MAGN_ETA_L(MAGN_ETA) = .FALSE.
              END DO
            ENDIF
C       The item should not be negated, and should not be a keyword
C       The item should be in the range ETA_MIN, ETA_MAX
            IF ((LIST(COUNT,ITEM_T_INDEX) .EQ. PARSE_NEGATED) .OR.
     &          (LIST(COUNT,ITEM_T_INDEX) .EQ. PARSE_KEYWORD) .OR. 
     &          (LIST(COUNT,ITEM_INDEX) .LT. ETA_MIN) .OR.
     &          (LIST(COUNT,ITEM_INDEX) .GT. ETA_MAX)) THEN
              PARSE_STATUS = PARSE_BAD_PARAM
              GOTO 999
            ENDIF
C       The same goes for the range end if this is a range
            IF (LIST(COUNT,ITEM_T_INDEX) .EQ. PARSE_LOWER_BOUNDARY) THEN
              IF ((COUNT+1 .GT. LIST_TOP) .OR. 
     &            (LIST(COUNT+1,ITEM_T_INDEX) .NE. 
     &             PARSE_UPPER_BOUNDARY) .OR.
     &            (LIST(COUNT+1,ITEM_INDEX) .LT. ETA_MIN) .OR.
     &            (LIST(COUNT+1,ITEM_INDEX) .GT. ETA_MAX) .OR.
     &            (LIST(COUNT+1,ITEM_INDEX) .LE. 
     &             LIST(COUNT, ITEM_INDEX))) THEN
                PARSE_STATUS = PARSE_BAD_PARAM
                GOTO 999
              ENDIF
C       If this is for large tiles, the boundaries should follow tile layout
              IF (THRTYP .EQ. COOR_RSLGTILE) THEN
                MAGN_ETA = LIST(COUNT,ITEM_INDEX) 
                MAGN_ETA = TT_ETA_PER_LT * ( (MAGN_ETA-1) 
     &                                     / TT_ETA_PER_LT ) + 1
                IF ( MAGN_ETA .NE. LIST(COUNT,ITEM_INDEX) ) THEN 
                  PARSE_STATUS = PARSE_BAD_PARAM
                  GOTO 999
                ENDIF
                MAGN_ETA = LIST(COUNT+1,ITEM_INDEX) 
                MAGN_ETA = TT_ETA_PER_LT * ( MAGN_ETA / TT_ETA_PER_LT )
                IF ( MAGN_ETA .NE. LIST(COUNT+1,ITEM_INDEX) ) THEN 
                  PARSE_STATUS = PARSE_BAD_PARAM
                  GOTO 999
                ENDIF
              END IF
C       Assign the range
              DO MAGN_ETA = LIST(COUNT,ITEM_INDEX), 
     &                      LIST(COUNT+1,ITEM_INDEX)
                IF (THRTYP .EQ. COOR_RSLGTILE) THEN
C       For large tiles, only set mask for the lower magn_eta of a tile
                  IF ( ( MAGN_ETA - 1 ) 
     &            .EQ. ( TT_ETA_PER_LT * ( (MAGN_ETA-1) 
     &                                   / TT_ETA_PER_LT) ) ) 
     &            MAGN_ETA_L(MAGN_ETA) = .TRUE.
                ELSE 
                  MAGN_ETA_L(MAGN_ETA) = .TRUE.
                ENDIF
              END DO
C       if it is not a range
C       (END_RANGE have already been skipped due to action above)
            ELSE
C       This cannot be for a large tile, as it needs to cover more than one eta
              IF (THRTYP .EQ. COOR_RSLGTILE) THEN
                PARSE_STATUS = PARSE_BAD_PARAM
                GOTO 999
              ENDIF
              MAGN_ETA_L(LIST(COUNT,ITEM_INDEX)) = .TRUE.
            ENDIF
C
            LAST_OBJECT = LIST(COUNT,OBJ_INDEX)
C
C       Handle PHI range
          ELSEIF (LIST(COUNT,OBJ_INDEX) .EQ. KEY_PHI) THEN
C       Clear the variable if necessary
            IF (LAST_OBJECT .NE. LIST(COUNT,OBJ_INDEX)) THEN
              DO PHI = PHI_MIN, PHI_MAX
                PHI_L(PHI) = .FALSE.
              END DO
            ENDIF
C       The item should not be negated, and should not be a keyword
C       The item should be in the range PHI_MIN, PHI_MAX
            IF ((LIST(COUNT,ITEM_T_INDEX) .EQ. PARSE_NEGATED) .OR.
     &          (LIST(COUNT,ITEM_T_INDEX) .EQ. PARSE_KEYWORD) .OR. 
     &          (LIST(COUNT,ITEM_INDEX) .LT. PHI_MIN) .OR.
     &          (LIST(COUNT,ITEM_INDEX) .GT. PHI_MAX)) THEN
              PARSE_STATUS = PARSE_BAD_PARAM
              GOTO 999
            ENDIF
C       The same goes for the range end if this is a range
            IF (LIST(COUNT,ITEM_T_INDEX) .EQ. PARSE_LOWER_BOUNDARY) THEN
              IF ((COUNT+1 .GT. LIST_TOP) .OR. 
     &            (LIST(COUNT+1,ITEM_T_INDEX) .NE. 
     &             PARSE_UPPER_BOUNDARY) .OR.
     &            (LIST(COUNT+1,ITEM_INDEX) .LT. PHI_MIN) .OR.
     &            (LIST(COUNT+1,ITEM_INDEX) .GT. PHI_MAX) .OR.
     &            (LIST(COUNT+1,ITEM_INDEX) .LE. 
     &             LIST(COUNT, ITEM_INDEX))) THEN
                PARSE_STATUS = PARSE_BAD_PARAM
                GOTO 999
              ENDIF
C       If this is for large tiles, the boundaries should follow tile layout
              IF (THRTYP .EQ. COOR_RSLGTILE) THEN
                PHI = LIST(COUNT,ITEM_INDEX) 
                PHI = TT_PHI_PER_LT * ( (PHI-1) / TT_PHI_PER_LT ) + 1
                IF ( PHI .NE. LIST(COUNT,ITEM_INDEX) ) THEN 
                  PARSE_STATUS = PARSE_BAD_PARAM
                  GOTO 999
                ENDIF
                PHI = LIST(COUNT+1,ITEM_INDEX) 
                PHI = TT_PHI_PER_LT * ( PHI / TT_PHI_PER_LT )
                IF ( PHI .NE. LIST(COUNT+1,ITEM_INDEX) ) THEN 
                  PARSE_STATUS = PARSE_BAD_PARAM
                  GOTO 999
                ENDIF
              END IF
C       Assign the range
              DO PHI = LIST(COUNT,ITEM_INDEX), 
     &                 LIST(COUNT+1,ITEM_INDEX)
                IF (THRTYP .EQ. COOR_RSLGTILE) THEN
C       For large tiles, only set mask for the lower phi of a tile.
                  IF ( ( PHI - 1 ) 
     &            .EQ. ( TT_PHI_PER_LT * ((PHI-1) / TT_PHI_PER_LT) ) ) 
     &            PHI_L(PHI) = .TRUE.
                ELSE 
                  PHI_L(PHI) = .TRUE.
                ENDIF
              END DO
C       if it is not a range
C       (END_RANGE have already been skipped due to action above)
            ELSE
C       This cannot be for a large tile, as it needs to cover more than one phi
              IF (THRTYP .EQ. COOR_RSLGTILE) THEN
                PARSE_STATUS = PARSE_BAD_PARAM
                GOTO 999
              ENDIF
              PHI_L(LIST(COUNT,ITEM_INDEX)) = .TRUE.
            ENDIF
C
            LAST_OBJECT = LIST(COUNT,OBJ_INDEX)
C
C
C       Handle L1.5 key words
          ELSE IF (LIST(COUNT,OBJ_INDEX) .EQ. KEY_CRATE) THEN
            IF(LIST(COUNT,ITEM_T_INDEX) .NE. PARSE_ASSERTED.OR.
     &         LIST(COUNT,ITEM_INDEX).GT.L15CT_NUM_MAX.OR.
     &         LIST(COUNT,ITEM_INDEX).LT.L15CT_NUM_MIN)THEN
              PARSE_STATUS = PARSE_BAD_PARAM
              GOTO 999
            ENDIF
            L15_CRATE_NUM = LIST(COUNT,ITEM_INDEX)
            LAST_OBJECT = LIST(COUNT,OBJ_INDEX)

          ELSE IF (LIST(COUNT,OBJ_INDEX) .EQ. KEY_TERM) THEN
            IF(LIST(COUNT,ITEM_T_INDEX) .NE. PARSE_ASSERTED.OR.
     &         LIST(COUNT,ITEM_INDEX).GT.L15TM_NUM_MAX.OR.
     &         LIST(COUNT,ITEM_INDEX).LT.L15TM_NUM_MIN)THEN
              PARSE_STATUS = PARSE_BAD_PARAM
              GOTO 999
            ENDIF
            L15_TERM_NUM = LIST(COUNT,ITEM_INDEX)
            LAST_OBJECT = LIST(COUNT,OBJ_INDEX)
        
          ELSE IF (LIST(COUNT,OBJ_INDEX) .EQ. KEY_TYPE) THEN
            IF(LIST(COUNT,ITEM_T_INDEX) .NE. PARSE_KEYWORD.OR.
     &         LIST(COUNT,ITEM_INDEX).LT.KEY_EM.OR.
     &         LIST(COUNT,ITEM_INDEX).GT.KEY_TOT)THEN
              PARSE_STATUS = PARSE_BAD_PARAM
              GOTO 999
            ENDIF
            L15_TYPE = LIST(COUNT,ITEM_INDEX)
            LAST_OBJECT = LIST(COUNT,OBJ_INDEX)
        
C       Otherwise it is a keyword invalid in this context
          ELSE
            PARSE_STATUS = PARSE_BAD_PARAM
            GOTO 999
C
          ENDIF
        ENDIF
      END DO
C
C       It is an error if the last item on the list does not cause an
C       assignment
C
      IF (ASSIGNMENT_OCCURRED .EQV. .FALSE.) THEN
        PARSE_STATUS = PARSE_BAD_FORMAT
      ENDIF
C
  999 CONTINUE 
      IF ( SKIP_ASSIGNMENTS .EQV. .TRUE. ) GOTO 20

c      IF(THRTYP. EQ. L15COOR_REFSET.AND.L15_TYPE.EQ.KEY_TOT)THEN
c        WRITE(*,*)'**********L15_TOT_THRSHLD**********'
c        WRITE(*,*)L15_TOT_THRSHLD(0,0,POS_ETA,ETA_MIN,PHI_MIN),
c     &            L15_TOT_THRSHLD(0,0,NEG_ETA,16,PHI_MAX)
c        WRITE(*,*)L15_TOT_THRSHLD(0,0,NEG_ETA,17,PHI_MIN),
c     &            L15_TOT_THRSHLD(0,0,POS_ETA,20,PHI_MAX)
c      ENDIF
c      IF(THRTYP. EQ. L15COOR_REFSET.AND.L15_TYPE.EQ.KEY_EM)THEN
c        WRITE(*,*)'**********L15_EM_THRSHLD**********'
c        WRITE(*,*)L15_EMT_THRSHLD(0,0,POS_ETA,ETA_MIN,PHI_MIN),
c     &            L15_EMT_THRSHLD(0,0,NEG_ETA,16,PHI_MAX)
c        WRITE(*,*)L15_EMT_THRSHLD(0,0,NEG_ETA,17,PHI_MIN),
c     &            L15_EMT_THRSHLD(0,0,POS_ETA,20,PHI_MAX)
c      ENDIF
      RETURN
C#######################################################################
      ENTRY      L1COOR_REFSET_VERIFY (THRTYP)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Perform the same syntax checking as
C-                         L1COOR_REFSET_INIT without the assignments
C-
C-   Inputs  : THRTYP   The type of threshold being defined, 
C-                      use COOR_RSEMET, or COOR_RSLGTILE cf. L1COOR_PARSER.INC
C-   Outputs : set flag PARSE_STATUS in L1COOR_PARSER.INC
C-   Controls: 
C-
C-   Created   6-JUL-1993   Philippe Laurens - MSU L1 Trigger
C-
C----------------------------------------------------------------------
      SKIP_ASSIGNMENTS = .TRUE. 
C     Now catch up with the main code body 
      GOTO 10
C     Returning from the main code body      
  20  CONTINUE
      RETURN
      END
