      SUBROUTINE PRTRGR_L15_FRAME_PARAM_SECTION (LUN, L15_FPS_BLOCK,
     &                                           NLWF)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Print out the summary of the Level 1.5 Datablock
C-                         from the selected TRGR bank.
C-
C-   Inputs  : LUN         The unit number to write to.
C-             L15_FPS_BLOCK The array of Frame Parameter Section words
C-
C-   Outputs : NLWF        Number of longwords to follow
C-   Controls: none
C-
C-   Created  28-MAR-1994   Johannes V. (Djoko) Wirjawan
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:L15CALDBB_DATA_BLOCK.PARAMS'
C
      CHARACTER*8 PRTRGR_INT_TO_HEX, MROW, NROW, KROW(FRAME_UNI_NLW)
      CHARACTER*8 TERMS
      CHARACTER*2 J1, J2, J4
      CHARACTER*2 K1(FRAME_UNI_NLW), K2(FRAME_UNI_NLW)
      INTEGER I1, I2, I3, I4
      INTEGER I, J, TERM_DEF, LUN, FPS_ID, NLWF
      INTEGER L15_FPS_BLOCK(L15CAL_FRAME_PARAM_LENGTH)
C
C----------------------------------------------------------------------
C
  104 FORMAT(' ',A, T49, ': ', A, ' (H)')
  109 FORMAT(' ',A, T49, ': ', I3)
      WRITE(LUN, 109) 'Longwords to follow',
     &                     L15_FPS_BLOCK(1)
      NLWF = L15_FPS_BLOCK(1)
      MROW = PRTRGR_INT_TO_HEX(L15_FPS_BLOCK(2))
      CALL HEX_TO_INT(MROW(7:8),I1)
      CALL HEX_TO_INT(MROW(5:6),I2)
      CALL HEX_TO_INT(MROW(3:4),I3)
      CALL HEX_TO_INT(MROW(1:2),I4)
      WRITE(LUN,*)
      WRITE(LUN,109)'Number of Universal Blocks', I1
      WRITE(LUN,109)'Number of Lw per Universal Block', I2
      WRITE(LUN,109)'Number of Frame Term Blocks', I3
      WRITE(LUN,109)'Number of Lw per Frame Param Term Block', I4
      WRITE(LUN,*)
      WRITE(LUN,*)'      Universal Parameter Data for this Crate'
      NROW = PRTRGR_INT_TO_HEX(L15_FPS_BLOCK(3))
      J1 = NROW(7:8)
      J2 = NROW(5:6)
      J4 = NROW(1:2)
      WRITE(LUN,*)
      WRITE(LUN,104)'Memory Map Revision Number', J1
      WRITE(LUN,104)'Memory Map Version Number', J2
      WRITE(LUN,104)'Crate ID', J4
      WRITE(LUN,*)
      IF (L15_FPS_BLOCK(4).NE.0)
     &WRITE(LUN, 104) 'Number of Terms defined for this Crate',
     &                     PRTRGR_INT_TO_HEX(L15_FPS_BLOCK(4))
      IF (L15_FPS_BLOCK(5).NE.0)
     &WRITE(LUN, 104) 'Number of Univ. Params defined for this Crate',
     &                     PRTRGR_INT_TO_HEX(L15_FPS_BLOCK(5))
      DO I = 1, 13
  110    FORMAT(' This Crate''s Universal Parameter # ', I2,
     &                '          : ', A)
         IF (L15_FPS_BLOCK(I+5).NE.0) THEN
            WRITE(LUN,110) I, PRTRGR_INT_TO_HEX(L15_FPS_BLOCK(I+5))
         ENDIF
      ENDDO
      WRITE(LUN,*)
      FPS_ID = 18
      I = 0
      DO WHILE (FPS_ID .LT. (L15_FPS_BLOCK(1)+1))
         I = I + 1
  120    FORMAT('     Frame Parameter Data for this Crate''s Term #',
     &                I3 )
         WRITE(LUN,120) I-1
         WRITE(LUN,*)
         DO J = 1, FRAME_UNI_NLW
            FPS_ID = (I-1)*FRAME_UNI_NLW + J + 18
            KROW(J) = PRTRGR_INT_TO_HEX(L15_FPS_BLOCK(FPS_ID))
            K1(J)   = KROW(J)(7:8)
            K2(J)   = KROW(J)(5:6)
            IF (KROW(J).NE.'00000000') THEN
              IF (J .EQ. 1) THEN
                WRITE(LUN,104)
     &          'Term # for this Term''s Frame Param Blocks', K1(J)
                WRITE(LUN,104)'Parameter Block Type Flag', K2(J)
              ELSEIF (J .EQ. 2) THEN
                WRITE(LUN,104)'Pass_1_of_N Value for this Term',
     &                          KROW(J)
              ELSEIF (J .EQ. 3) THEN
                WRITE(LUN,104)
     &          'Mask of Spec Trig mapped to this Term', KROW(J)
              ELSEIF (J .EQ. 4) THEN
                WRITE(LUN,104)
     &          'Number of Frame Parameters for this Term', KROW(J)
              ELSE
  130           FORMAT(' This Term''s Frame Parameter # ', I2,
     &                 '               : ', A)
                WRITE(LUN,130) (J-4), KROW(J)
              ENDIF
            ENDIF
         ENDDO
         WRITE(LUN,*)
      ENDDO
      WRITE(LUN,*)
C----------------------------------------------------------------------
  999 RETURN
      END
