      SUBROUTINE L1_L15CAL_FILL ()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : fill the l1.5 calorimeter blocks in TRGR
C-
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created   15-NOV-1993 S. ENO U. MARYLAND
C-   Updated  28-APR-1994   sFahey  Replaced PARAM block with TOOL PARAM
C-                                  block, added FRAME PARAM block.
C-   Updated  11-MAY-1994   sFahey  Accommodated variable length blocks 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'

      INCLUDE 'D0$PARAMS:L15CALDBB_DATA_BLOCK.PARAMS'
      INCLUDE 'D0$INC:L15CALDBB_DATA_BLOCK.INC'
C
      INTEGER    FFFFFFFF,      FFFF
      PARAMETER (FFFFFFFF = -1, FFFF = 65535)
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
     +           THIRD_BYTE,   FOURTH_BYTE
      PARAMETER (FIRST_BYTE  = 1,
     +           SECOND_BYTE = FIRST_BYTE  + BYTE_LENGTH,
     +           THIRD_BYTE  = SECOND_BYTE + BYTE_LENGTH,
     +           FOURTH_BYTE = THIRD_BYTE  + BYTE_LENGTH )

C
C  local parameters
C
      INTEGER I              !counters for DO loops
C
      INTEGER LTRGR,GZTRGR   !pointers to TRGR bank
c
      INTEGER L15_OFFSET     !pointer to the L15 cal crate block in TRGR
      INTEGER LL15CAL
      INTEGER PNTR           !current pointer within L15 data
C
      INTEGER BLOCK_LENGTH,DATA_VALID,JBYT
C
C----------------------------------------------------------------------
C
C   If L15CAL_SIM not run, don't fill entire debug block...
C
      DATA_VALID = JBYT(L15CAL_CRATE_HEADER(6),1,8)
      IF (DATA_VALID.NE.0) THEN
        L15CAL_DEBUG_BLOCK(1) = TYPE0_NLW + TYPE4_NLW
      ENDIF
C
C   Find length of L15C block (by adding up lengths of sub-blocks)
C
      BLOCK_LENGTH = l15cal_crate_header(1) + 1      +
     &               l15cal_frame_block(1)  + 1      +
     &               l15cal_frame_param_block(1) + 1 +
     &               l15cal_tool_param_block(1) + 1  +
     &               l15cal_local_dsp_block(1) + 1   +
     &               l15cal_global_dsp_block(1) + 1  +
     &               l15cal_debug_block(1) + 1       +
     &               l15cal_trailer_length
C
Cfind where in the TRGR block the L15 calorimeter crate information goes
C
      LTRGR = GZTRGR()
      call l1util_trgr_extension(LTRGR,
     &                BLOCK_LENGTH,
     &                l15_offset)
      LTRGR = GZTRGR()
      LL15CAL = LTRGR + L15_OFFSET

C                                                                  Header Block
C                                                                  ------------
      PNTR = LL15CAL
      IF(L15CAL_CRATE_HEADER(1).GT.L15CAL_HEADER_LENGTH-1) THEN
        CALL ERRMSG('L1_L15CAL_FILL','L1SIM_EVENT',
     &    'L15 HEADER DATA SICK','W')
        RETURN
      ENDIF
      DO I = 1, (L15CAL_CRATE_HEADER(1) + 1)
        PNTR=PNTR+1
        IQ(PNTR) = L15CAL_CRATE_HEADER(I)
      ENDDO
C                                                                  Frame section
C                                                                    ----------
      IF(L15CAL_FRAME_BLOCK(1).GT.L15CAL_FRAME_LENGTH-1) THEN
        CALL ERRMSG('L1_L15CAL_FILL','L1SIM_EVENT',
     &     'L15 FRAME DATA SICK','W')
        RETURN
      ENDIF
      DO I = 1, (L15CAL_FRAME_BLOCK(1) + 1)
        PNTR = PNTR + 1
        IQ(PNTR) = L15CAL_FRAME_BLOCK (I)
      ENDDO
C                                                            FRAME PARAM section
C                                                                    ----------
      IF(L15CAL_FRAME_PARAM_BLOCK(1).GT.
     &       L15CAL_FRAME_PARAM_LENGTH-1) THEN
        CALL ERRMSG('L1_L15CAL_FILL','L1SIM_EVENT',
     &     'L15 FRAME PARAM DATA SICK','W')
        RETURN
      ENDIF
      DO I = 1, (L15CAL_FRAME_PARAM_BLOCK(1) + 1)
        PNTR = PNTR + 1
        IQ(PNTR) = L15CAL_FRAME_PARAM_BLOCK (I)
      ENDDO
C                                                             TOOL PARAM section
C                                                                    ----------
      IF(L15CAL_TOOL_PARAM_BLOCK(1).GT.L15CAL_TOOL_PARAM_LENGTH-1) THEN
        CALL ERRMSG('L1_L15CAL_FILL','L1SIM_EVENT',
     &     'L15 TOOL PARAM DATA SICK','W')
        RETURN
      ENDIF
      DO I = 1, (L15CAL_TOOL_PARAM_BLOCK(1) + 1)
        PNTR = PNTR + 1
        IQ(PNTR) = L15CAL_TOOL_PARAM_BLOCK (I)
      ENDDO
C                                                            LOCAL dsp section
C                                                                    ----------
      IF(L15CAL_LOCAL_DSP_BLOCK(1).GT.L15CAL_LOCAL_DSP_LENGTH-1) THEN
        CALL ERRMSG('L1_L15CAL_FILL','L1SIM_EVENT',
     &     'L15 LOCAL DSP DATA SICK','W')
        RETURN
      ENDIF
      DO I = 1, (L15CAL_LOCAL_DSP_BLOCK(1) + 1)
        PNTR = PNTR + 1
        IQ(PNTR) = L15CAL_LOCAL_DSP_BLOCK (I)
      ENDDO
C                                                            global dsp section
C                                                                    ----------
      IF(L15CAL_GLOBAL_DSP_BLOCK(1).GT.L15CAL_GLOBAL_DSP_LENGTH-1) THEN
        CALL ERRMSG('L1_L15CAL_FILL','L1SIM_EVENT',
     &     'L15 GLOBAL DSP DATA SICK','W')
        RETURN
      ENDIF
      DO I = 1, (L15CAL_GLOBAL_DSP_BLOCK(1) + 1)
        PNTR = PNTR + 1
        IQ(PNTR) = L15CAL_GLOBAL_DSP_BLOCK (I)
      ENDDO
C                                                                debug section
C                                                                    ----------
      IF(L15CAL_DEBUG_BLOCK(1).GT.L15CAL_DEBUG_LENGTH-1) THEN
        CALL ERRMSG('L1_L15CAL_FILL','L1SIM_EVENT',
     &     'L15 DEBUG DATA SICK','W')
        RETURN
      ENDIF
C
      DO I = 1, (L15CAL_DEBUG_BLOCK(1) + 1)
        PNTR = PNTR + 1
        IQ(PNTR) = L15CAL_DEBUG_BLOCK (I)
      ENDDO
C
C                                                                 Trailer Block
C                                                                 -------------
      PNTR = PNTR + 1
      IQ(PNTR) = BLOCK_LENGTH         ! FIRST WORD OF TRAILER IS BLOCK SIZE
      DO I = 2, L15CAL_TRAILER_LENGTH
        PNTR = PNTR + 1
        IQ(PNTR) = L15cal_CRATE_TRAILER(I)
      ENDDO
C



  999 RETURN
      END
