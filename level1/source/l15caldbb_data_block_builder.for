      SUBROUTINE L15CALDBB_DATA_BLOCK_BUILDER()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Builds the Level 1.5 calorimetry Trigger Data Block.
C-
C-   Inputs  :
C-
C-   Outputs : 
C-   Controls: None.
C-
C-  created 15nov93 S. Eno. U. Maryland
C-
C----------------------------------------------------------------------
C
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
C
      INCLUDE 'D0$PARAMS:L15CALDBB_DATA_BLOCK.PARAMS'
      INCLUDE 'D0$INC:L15CALDBB_DATA_BLOCK.INC'
C
      INTEGER    FFFFFFFF,      FFFF
      PARAMETER (FFFFFFFF = -1, FFFF = 65535)
      INTEGER LTRGR_LEVEL1
      INTEGER   GZFIND_CRATE, GZTRGR
      EXTERNAL  GZFIND_CRATE, GZTRGR
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
C
c first initialize whole array to zero so won't be left with
c any results from last event
      call vzero(l15cal_FRAME_BLOCK,
     +                     L15CAL_FRAME_LENGTH  )
      call vzero(l15cal_FRAME_PARAM_BLOCK,
     +                     L15CAL_FRAME_PARAM_LENGTH  )
      call vzero(l15cal_TOOL_PARAM_BLOCK,
     +                     L15CAL_TOOL_PARAM_LENGTH  )
      call vzero(l15cal_LOCAL_DSP_BLOCK,
     +                     L15CAL_LOCAL_DSP_LENGTH  )
      call vzero(l15cal_GLOBAL_DSP_BLOCK,
     +                     L15CAL_GLOBAL_DSP_LENGTH  )
      call vzero(l15cal_DEBUG_BLOCK,
     +                     L15CAL_DEBUG_LENGTH  )
C
C     Header and Trailer Block filling
C     ================================
C
C
      call vzero(l15cal_CRATE_HEADER,
     +                     L15CAL_HEADER_LENGTH  )
      call vzero(l15cal_CRATE_TRAILER,
     +                     L15CAL_TRAILER_LENGTH  )
        L15CAL_CRATE_HEADER(1) = L15CAL_HEADER_LENGTH - 1
        L15CAL_CRATE_HEADER(2) = FFFF
        L15CAL_CRATE_HEADER(3) = 0
        CALL SBYT (L15CAL_CRATE_ID, L15CAL_CRATE_HEADER(3), FOURTH_BYTE,
     &               BYTE_LENGTH)
        L15CAL_CRATE_HEADER(4) = L15CAL_VERSION_NUMBER
        L15CAL_CRATE_HEADER(5) = L15CAL_REVISION_NUMBER
        L15CAL_CRATE_HEADER(6) = 'F'X  !should be copied from hardware trgr?
        L15CAL_CRATE_HEADER(7) = 0     !should be replaced with m+pass flags
C        L15CAL_CRATE_TRAILER(1) = L15CAL_BANK_LENGTH 
        L15CAL_CRATE_TRAILER(2) = L15CAL_CRATE_ID
        L15CAL_CRATE_TRAILER(3) = FFFFFFFF
C   
C   copy header sync word from Crate 11
C   added by D Owen 5/13/94
        LTRGR_LEVEL1 = GZFIND_CRATE ( 'TRGR', GZTRGR(), 11 )
        IF ( LTRGR_LEVEL1 .LE. 0 ) 
     &  THEN 
C comment out this error message section until we sort out what to do for MC
C data where there is no TRGR bank to pick up the sync word from
C          WRITE ( ERR_MESSAGE, 1000 )  LTRGR_LEVEL1 
C 1000     FORMAT ( ' Cannot find L1 Crate to copy SYNC header word', 
C     &            I4 )
C          CALL ERRMSG( 'GZFIND_CRATE', 'L1_SPECIAL_TERMS_SIM',
C     &                ERR_MESSAGE, 'W')
          L15CAL_CRATE_HEADER(2)=FFFF
        ELSE
          L15CAL_CRATE_HEADER(2) = IQ(LTRGR_LEVEL1+1)
        ENDIF
C
C frame block
C 
      l15cal_frame_block(1)=l15cal_frame_length - 1
C
C frame param block
C 
      CALL L15C_FILL_FRAME_PARAM_BLOCK
C
C tool param block
C 
      CALL L15C_FILL_TOOL_PARAM_BLOCK
C
C local DSP block
C 
      l15cal_local_dsp_block(1)=l15cal_local_dsp_length - 1
C
C global block
C 
      l15cal_global_dsp_block(1)=l15cal_global_dsp_length - 1
C
C debug block
C 
      l15cal_debug_block(1)=l15cal_debug_length - 1
C
      RETURN
      END
