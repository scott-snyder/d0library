      SUBROUTINE L15C_FILL_DEBUG_T0(INDEX,EVENT_TYPE,WDS_T_FLW)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fill L15CAL_DEBUG_BLOCK array of common block
C-
C-   Inputs  : Seperate entry points for each block type
C-    type0 inputs: INDEX = index of first word of block in array
C-                           L15CAL_DEBUG_BLOCK
C_                  EVENT_TYPE = 0 for normal events, = 1 for mark&pass
C_    type1 inputs: INDEX (see type0), LDSP = local DSP number (1 - 11)
C-    type2 inputs: INDEX, TERM = local term #, LDSP
C-    type3 inputs: INDEX, TERM = local term #, LDSP
C-   Outputs : returns WDS_T_FLW = number of words following first word.
C-             also fills that many words in common array L15CAL_DEBUG_BLOCK
C-   Controls:
C-
C-   Created   5-MAY-1994   Dan Owen
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:LEVEL1_LOOKUP.PARAMS'
      INCLUDE 'D0$PARAMS:L15CALDBB_DATA_BLOCK.PARAMS'
      INCLUDE 'D0$PARAMS:L15_FRAMEWORK.PARAMS'
      INCLUDE 'D0$PARAMS:L15_LOCAL_DSP.PARAMS'
      INCLUDE 'D0$PARAMS:L15COOR_PARSER.PARAMS'
      INCLUDE 'D0$INC:LEVEL1_LOOKUP.INC'
      INCLUDE 'D0$INC:L15CALDBB_DATA_BLOCK.INC'
      INCLUDE 'D0$INC:L15_LOCAL_DSP.INC'
      INCLUDE 'D0$INC:L15C_REFSET_THRESHOLDS.INC'
C
      INTEGER NEXT, DUMMY_ET, ETA, AETA, PHI, ETA_LOW, TERM,
     &  ETA_HIGH,ENERGY, E_TYPE, BYTE, LDSP, RACK, INDEX,
     &  EVENT_TYPE, WDS_T_FLW, I68K, CRATE, REF_MEV, REF_RAW, DUMMY_REF
      INTEGER WORD_INDEX,ISIGN
      LOGICAL EM_SET
      PARAMETER (I68K = 104,DUMMY_REF=255)
C
C     Parameters to be used with the BITBYT CernLib package
C
      INTEGER    BYTE_LENGTH,     WORD_LENGTH,
     +           LONG_WORD_LENGTH
      PARAMETER (BYTE_LENGTH      = 8,
     +           WORD_LENGTH      = 2 * BYTE_LENGTH,
     +           LONG_WORD_LENGTH = 4 * BYTE_LENGTH)
C
      INTEGER    FIRST_BYTE,   SECOND_BYTE,
     +           THIRD_BYTE,   FOURTH_BYTE,
     +           FIRST_WORD,   SECOND_WORD
      PARAMETER (FIRST_BYTE  = 1,
     +           SECOND_BYTE = FIRST_BYTE  + BYTE_LENGTH,
     +           THIRD_BYTE  = SECOND_BYTE + BYTE_LENGTH,
     +           FOURTH_BYTE = THIRD_BYTE  + BYTE_LENGTH,
     +           FIRST_WORD  = 1,
     +           SECOND_WORD = FIRST_WORD  + WORD_LENGTH)

      DATA DUMMY_ET/16/
C---------------------------------------------------------------
C-  entry point for Type 0 block filling.
      NEXT=INDEX+1
      L15CAL_DEBUG_BLOCK(NEXT) = -EVENT_TYPE
      WDS_T_FLW = 1
      CALL L15C_DEBUG_TYPE_FW(INDEX,WDS_T_FLW,0,I68K)
      GO TO 999
C----------------------------------------------------------------
C-  entry point for Type 1 block filling.
      ENTRY L15C_FILL_DEBUG_T1(INDEX,LDSP,WDS_T_FLW)
C
C Get pointer to first word of Debug Section type 1
C For now rely on fact that type 0 is fixed length of 2
      NEXT=INDEX
C loop over racks
      DO RACK = 1 , 2
        IF ( RACK .EQ. 1 ) THEN
          ETA_LOW = DSP_ETA_LOW(LDSP)-2
          ETA_HIGH = ETA_LOW + 3
        ELSE
          ETA_HIGH = DSP_ETA_HIGH(LDSP)+2
          ETA_LOW = ETA_HIGH-3
        ENDIF
C loop over energy type (E_TYPE=1 for EM, =2 for TOT)
        DO E_TYPE = 1 , 2
C loop over phi
          DO PHI = PHI_MIN , PHI_MAX
            NEXT = NEXT+1
C get energies for 4 eta values & combine into one word
            BYTE = -BYTE_LENGTH + 1
            DO ETA = ETA_LOW , ETA_HIGH
              BYTE = BYTE + BYTE_LENGTH
              IF ((LDSP .EQ. 1 .AND. RACK .EQ. 1) .OR.
     &          (LDSP .EQ. 11 .AND. RACK .EQ. 2))
     &          THEN
                ENERGY = DUMMY_ET
              ELSE
                ENERGY = L15CT_TT_RAW(ETA,PHI,E_TYPE)
              ENDIF
C pack into data block word
              CALL SBYT(ENERGY,L15CAL_DEBUG_BLOCK(NEXT),BYTE,
     &            BYTE_LENGTH)
            ENDDO
          ENDDO
        ENDDO
      ENDDO
      WDS_T_FLW = NEXT - INDEX
C-  fill in first word of this Type 1 block
      CALL L15C_DEBUG_TYPE_FW(INDEX,WDS_T_FLW,1,LDSP_ID(LDSP))
      GO TO 999
C----------------------------------------------------------------------
C-  entry pt for type 2 block
      ENTRY L15C_FILL_DEBUG_T2(INDEX,CRATE,TERM,LDSP,WDS_T_FLW)
c... given the term #, find out whether to use EM or TOT refset
c...
      EM_SET = .TRUE.
C...
c... write the term number to the 2nd word in the type 2 block
c...
      L15CAL_DEBUG_BLOCK(INDEX+1) = TERM
c...
c... loop over the eta values, phi values, etc., filling debug block
c... if LDSP is either first or last ldsp must extend ETA range
c...
      IF ( LDSP .EQ. 1 ) THEN
        ETA_LOW = -22
      ELSE
        ETA_LOW = DSP_ETA_LOW(LDSP)
      ENDIF
      IF ( LDSP .EQ. NUM_DSPS ) THEN
        ETA_HIGH = 22
      ELSE
        ETA_HIGH = DSP_ETA_HIGH(LDSP)
      ENDIF
      IF(EM_SET) THEN
        DO PHI = PHI_MIN, PHI_MAX
          NEXT = 0
          DO 100 ETA = ETA_LOW,ETA_HIGH
c...
            IF(ETA.EQ.0) GOTO 100
            AETA = ABS(ETA)
            NEXT = NEXT + 1
            IF(SIGN(1,ETA).GT.0) THEN ! get the sign of the eta
              ISIGN = POS_ETA          ! since L15_EMT_THRSHLD
            ELSE                       ! will need it
              ISIGN = NEG_ETA          !
            ENDIF                      !
c...
            WORD_INDEX = INDEX+1+(PHI-1)*4+NEXT
            IF ( AETA .GT. ETA_MAX ) THEN
              L15CAL_DEBUG_BLOCK(WORD_INDEX) = DUMMY_REF
            ELSE
c... get REF energy from array (units MEV), convert to 1/4 GeV units and add in
c    pedestal
              REF_MEV = L15_EMT_THRSHLD(CRATE,TERM,ISIGN,AETA,PHI)
              REF_RAW = REF_MEV/250 +
     &                  LOOKUP_ZERESP(ISIGN, AETA, PHI, EM_ET_QUANT)
              L15CAL_DEBUG_BLOCK(WORD_INDEX) = REF_RAW
            ENDIF
  100     CONTINUE
        ENDDO
      ELSE
        DO PHI = PHI_MIN, PHI_MAX
          NEXT = 0
          DO 200 ETA = ETA_LOW,ETA_HIGH
c...
            IF(ETA.EQ.0) GOTO 200
            AETA = ABS(ETA)
            NEXT = NEXT + 1
            IF(SIGN(1,ETA).GT.0) THEN ! get the sign of the eta
              ISIGN = POS_ETA          ! since L15_EMT_THRSHLD
            ELSE                       ! will need it
              ISIGN = NEG_ETA          !
            ENDIF                      !
c...
            WORD_INDEX = INDEX+1+(PHI-1)*4+NEXT
            IF ( ETA .GT. ETA_MAX ) THEN
              L15CAL_DEBUG_BLOCK(WORD_INDEX) = DUMMY_REF
            ELSE
c... get REF energy from array (units MEV), convert to 1/4 GeV units and add in
c    pedestal
              REF_MEV = L15_TOT_THRSHLD(CRATE,TERM,ISIGN,AETA,PHI)
              REF_RAW = REF_MEV/250
     &                + LOOKUP_ZERESP(ISIGN, AETA, PHI, EM_ET_QUANT)
     &                + LOOKUP_ZERESP(ISIGN, AETA, PHI, HD_ET_QUANT)
              L15CAL_DEBUG_BLOCK(WORD_INDEX) = REF_RAW
            ENDIF
  200     CONTINUE
        ENDDO
      ENDIF
c...
c... use end of loop index to give back words to follow
c...
      WDS_T_FLW = WORD_INDEX - INDEX
      CALL L15C_DEBUG_TYPE_FW(INDEX,WDS_T_FLW,2,LDSP_ID(LDSP))
      GO TO 999
C----------------------------------------------------------------------
C-  entry pt of type 3 block
      ENTRY L15C_FILL_DEBUG_T3(INDEX,CRATE,TERM,LDSP,WDS_T_FLW)
      wds_t_flw=7
      CALL L15C_DEBUG_TYPE_FW(INDEX,WDS_T_FLW,3,LDSP_ID(LDSP))
      GO TO 999
C----------------------------------------------------------------------
C-  entry pt for type 4 block
      ENTRY L15C_FILL_DEBUG_T4(INDEX,WDS_T_FLW)
      WDS_T_FLW=1
      CALL L15C_DEBUG_TYPE_FW(INDEX,WDS_T_FLW,4,GDSP_ID)
      GO TO 999
C------------------------------------------------------------------
  999 RETURN
      END
