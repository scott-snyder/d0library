      SUBROUTINE PRTRGR_L15_TOOL_PARAM_SECTION(LUN, L15_TPS_BLOCK, NLWF)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Print out the information from the
C-                         L1.5 Datablock Tool Parameter Section
C-
C-   Inputs  : LUN          The unit number to write the information to
C-             L15_TPS_BLOCK The array of Tool Parameter Section words
C-
C-   Outputs : NLWF         Number of longwords to follow
C-   Controls: none
C-
C-   Created   1-DEC-1993   Johannes V. (Djoko) Wirjawan
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:L15CALDBB_DATA_BLOCK.PARAMS'
C
      CHARACTER*8 PRTRGR_INT_TO_HEX, NROW, MROW(LOC_NLW)
      CHARACTER*8 KROW(GLB_NLW)
      CHARACTER*2 J1, J2, J3, J4, K1, K2
      EXTERNAL PRTRGR_INT_TO_HEX
C
      INTEGER LUN, NLWF
      INTEGER I, I1, I2, I3, I4, J, PRM_ID
      INTEGER L15_TPS_BLOCK(L15CAL_TOOL_PARAM_LENGTH)
C
C-------------------------------------------------------------------
C
  100 FORMAT(' ',A, T45, ': ', I3)
  109 FORMAT(' ',A, T45, ': ', A, ' (H)')
      WRITE(LUN, 100) 'Longwords to follow',
     &                     L15_TPS_BLOCK(1)
      NLWF = L15_TPS_BLOCK(1)
C
      NROW = PRTRGR_INT_TO_HEX(L15_TPS_BLOCK(2))
      CALL HEX_TO_INT(NROW(7:8),I1)
      CALL HEX_TO_INT(NROW(5:6),I2)
      CALL HEX_TO_INT(NROW(3:4),I3)
      CALL HEX_TO_INT(NROW(1:2),I4)
      WRITE(LUN,*)
      WRITE(LUN,100)'Number of Local Term Blocks', I1
      WRITE(LUN,100)'Number of Lw per Local Param Term Block', I2
      WRITE(LUN,100)'Number of Global Term Blocks', I3
      WRITE(LUN,100)'Number of Lw per Global Param Term Block', I4
      WRITE(LUN,*)
      IF (I1 .LE. LOC_NTERM) THEN
        DO  I = 1, I1
         DO J = 1, LOC_NLW
          PRM_ID = 2 + (I-1)*LOC_NLW + J
          MROW(J) = PRTRGR_INT_TO_HEX(L15_TPS_BLOCK(PRM_ID))
          IF (L15_TPS_BLOCK(PRM_ID).NE.0) THEN
            IF (J .EQ. 1) THEN
  110         FORMAT(' PARAMETER DATA FOR THE TERM LOCAL DSP CRATE #',
     &                   I2)
              WRITE(LUN,110) I-1
              J1= MROW(J)(7:8)
              J2= MROW(J)(5:6)
              J3= MROW(J)(3:4)
              J4= MROW(J)(1:2)
              WRITE(LUN,*)
              WRITE(LUN,109)'Term Number for this Param Term Block',
     &                       J1
              WRITE(LUN,109)'Local DSP Entry Flag', J2
              WRITE(LUN,109)'Ref Set Type used for this Term', J3
              WRITE(LUN,109)'Number of the matching L1 Ref Set', J4
              WRITE(LUN,*)
            ELSEIF(J .EQ. 2) THEN
              WRITE(LUN,109)'Tool ID Number used by this Term',
     &                       MROW(J)
            ELSEIF(J .EQ. 3) THEN
              WRITE(LUN,109)'Number of Tool Parameters', MROW(J)
            ELSE
  125         FORMAT(' Tool Dependent Parameter #', I2,
     &                ' is            : ', A)
              WRITE(LUN,125) J-3, MROW(J)
            ENDIF
          ENDIF
         ENDDO
         WRITE(LUN,*)
        ENDDO
      ELSE
        WRITE(LUN,*)
     &  'Number of Local Term Blocks exceeds the maximum value'
        WRITE(LUN,*)
      ENDIF
      IF (I3 .LE. GLB_NTERM) THEN
        DO I = 1, I3
         DO J = 1, GLB_NLW
          PRM_ID  = 2 + LOC_NTERM*LOC_NLW + (I-1)*GLB_NLW + J
          KROW(J) = PRTRGR_INT_TO_HEX(L15_TPS_BLOCK(PRM_ID))
          IF (L15_TPS_BLOCK(PRM_ID).NE.0) THEN
            IF (J .EQ. 1) THEN
              WRITE(LUN,*)
  150         FORMAT
     &  (' PARAMETER DATA FOR GLOBAL TOOL OF THIS CRATE''S TERM #',
     &               I2)
              WRITE(LUN,150) I-1
              K1 = KROW(J)(7:8)
              K2 = KROW(J)(5:6)
              WRITE(LUN,*)
              WRITE(LUN,109)
     &        'Term Number for this Global Param Block Term', K1
              WRITE(LUN,109)'Parameter Block Type''s Flag', K2
              WRITE(LUN,*)
            ELSEIF(J .EQ. 2) THEN
              WRITE(LUN,109)'Tool ID Number used by this Term ',
     &                       KROW(J)
            ELSEIF(J .EQ. 3) THEN
              WRITE(LUN,109)'Number of Tool Parameters ',
     &                      KROW(J)
            ELSE
              WRITE(LUN,125) J-3, KROW(J)
            ENDIF
          ENDIF
         ENDDO
         WRITE(LUN,*)
        ENDDO
      ELSE
        WRITE(LUN,*)
     &  'Number of Global Term Blocks exceeds the maximum value'
        WRITE(LUN,*)
      ENDIF
C----------------------------------------------------------------------
  999 RETURN
      END
