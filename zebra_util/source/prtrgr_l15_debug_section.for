      SUBROUTINE PRTRGR_L15_DEBUG_SECTION(LUN, LDEBS_START,
     &                                      L15_DEBUG_BLOCK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Print out the information from the
C-                         L1.5 Datablock Debag Section
C-
C-   Inputs  : LUN           The unit number to write the information to
C-             L15_DEBUG_BLOCK      The array of the Debug Section words
C-   Outputs : none
C-   Controls: none
C-
C-   Created  13-DEC-1993   Johannes V. (Djoko) Wirjawan
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:L15CALDBB_DATA_BLOCK.PARAMS'
C
      CHARACTER*8 PRTRGR_INT_TO_HEX, MROW, NROW
      EXTERNAL PRTRGR_INT_TO_HEX
      CHARACTER*2 V1, V2, W1, W2, W4
      CHARACTER*4 V5
C
      INTEGER LUN, L15_DEBUG_BLOCK(L15CAL_DEBUG_LENGTH)
      INTEGER I, HEAD_ENT, ADD_LW, STATUS
      INTEGER LDEBS_START, LENTY_START
C
C---------------------------------------------------------------------
C
  100   FORMAT(' ', A, T45, ': ', A, ' (H)')
  101   FORMAT(' ', A, T45, ': ', A )
  104   FORMAT(' ', A, T45, ': ', I5)
  105   FORMAT(' ', A, T45, ': ', I3)
        WRITE (LUN, 104) 'Debug Section Longwords to follow',
     &                    L15_DEBUG_BLOCK(1)
        WRITE(LUN,*)
        I = 0
        HEAD_ENT = 2
        LENTY_START = LDEBS_START + 2
        DO WHILE ((HEAD_ENT-2) .LT. L15_DEBUG_BLOCK(1))
          I = I + 1
  109     FORMAT('          ENTRY NUMBER : ', I5)
          WRITE(LUN,109) I
          WRITE(LUN,*)'         --------------------'
          WRITE(LUN,*)
          MROW = PRTRGR_INT_TO_HEX(L15_DEBUG_BLOCK(HEAD_ENT))
          V1   = MROW(7:8)
          V2   = MROW(5:6)
          CALL HEX_TO_INT(MROW(1:4), ADD_LW)
          WRITE (LUN,100) 'DSP or 68K generated this entry', V1
          WRITE (LUN,100) 'Entry type', V2
          WRITE (LUN,105) 'Additional longwords follow this entry',
     &                     ADD_LW
          WRITE (LUN,*)
          IF (V2 .EQ. '00') THEN
            WRITE (LUN,101) 'Mark and Force Pass Flag',
     &              PRTRGR_INT_TO_HEX(L15_DEBUG_BLOCK(HEAD_ENT+1))
            WRITE (LUN,*)
          ELSEIF (V2 .EQ. '01') THEN
            CALL PRTRGR_L15_DEBUG_TYPE1(LUN,V1,ADD_LW,IQ(LENTY_START))
          ELSEIF (V2 .EQ. '02') THEN
            CALL PRTRGR_L15_DEBUG_TYPE2(LUN,V1,ADD_LW,IQ(LENTY_START))
          ELSEIF (V2 .EQ. '03') THEN
            CALL PRTRGR_L15_DEBUG_TYPE3(LUN,ADD_LW,IQ(LENTY_START))
          ELSE
            NROW  =  PRTRGR_INT_TO_HEX(L15_DEBUG_BLOCK(HEAD_ENT+1))
            W1    = NROW(7:8)
            W2    = NROW(5:6)
            W4    = NROW(1:2)
            WRITE (LUN,100) 'Terms to Evaluate Mask', W1
            WRITE (LUN,100) 'Flags Byte', W2
            WRITE (LUN,100) 'TAS Byte', W4
            WRITE (LUN,*)
            IF (L15_DEBUG_BLOCK(3).EQ.0) GO TO 999 ! non MPF event
          ENDIF
          HEAD_ENT = HEAD_ENT + ADD_LW + 1
          LENTY_START = LENTY_START + ADD_LW + 1
        ENDDO
C----------------------------------------------------------------------
  999 RETURN
      END
