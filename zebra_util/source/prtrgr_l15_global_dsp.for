      SUBROUTINE PRTRGR_L15_GLOBAL_DSP(LUN, L15_GDSP_BLOCK, NLWF)

C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Print out the information from the
C-                         L1.5 Datablock Global DSP
C-
C-   Inputs  : LUN           The unit number to write the information to
C-             L15_GDSP_BLOCK    The array of the Global DSP words
C-
C-   Outputs : NLWF         Number of longwords to follow
C-   Controls: none
C-
C-   Created   6-DEC-1993   Johannes V. (Djoko) Wirjawan
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:L15CALDBB_DATA_BLOCK.PARAMS'
C
      CHARACTER*8 PRTRGR_INT_TO_HEX, NROW(GLB_NLW_ENT), MROW
      EXTERNAL PRTRGR_INT_TO_HEX
      CHARACTER*2 FIRST, SECOND, THIRD, LAST
      CHARACTER*2 V1(GLB_NLW_ENT), V2(GLB_NLW_ENT)
      CHARACTER*4 V5(GLB_NLW_ENT)
      INTEGER V3(GLB_NLW_ENT), V4(GLB_NLW_ENT)
C
      INTEGER LUN, STATUS, L15_GDSP_BLOCK(L15CAL_GLOBAL_DSP_LENGTH)
      INTEGER I, J, GDSP_ID, ENT_NUM, INT, NLWF
      REAL    ENERGY
C
C-----------------------------------------------------------------------
C
  100 FORMAT(' ', A, T45, ': ', I4)
  101 FORMAT(' ', A, T45, ': ', I3)
  104 FORMAT(' ', A, T45, ': ', A, ' (H)')
  105 FORMAT(' ', A, T45, ': ', F6.2)
      WRITE (LUN, 100) 'Global DSP Longwords to follow',
     &                  L15_GDSP_BLOCK(1)
      WRITE(LUN,*)
      NLWF = L15_GDSP_BLOCK(1)
C
        WRITE(LUN,*)'Header from DSP B2 (eta -20:-19)'
        WRITE(LUN,*)'================================'
        WRITE(LUN,*)
        MROW=PRTRGR_INT_TO_HEX(L15_GDSP_BLOCK(2))
        FIRST = MROW(1:2)
        SECOND= MROW(3:4)
        THIRD = MROW(5:6)
        LAST  = MROW(7:8)
        WRITE(LUN,104)'DSP source of this list', LAST
        WRITE(LUN,104)'Number of Valid Entries', THIRD
        WRITE(LUN,104)'Number of Entries in this list', SECOND
        WRITE(LUN,104)'Number of Longwords per entry', FIRST
        WRITE(LUN,*)
        CALL HEX_TO_INT(THIRD, ENT_NUM)
        IF (ENT_NUM .LE. GLB_NUM_ENT) THEN
          DO I = 1, ENT_NUM
  109       FORMAT('            ENTRY NUMBER :', I2)
            DO J=1, GLB_NLW_ENT
              GDSP_ID = (I-1)*GLB_NLW_ENT + J + 2
              NROW(J) = PRTRGR_INT_TO_HEX(L15_GDSP_BLOCK(
     &                                  GDSP_ID))
              V1(J) = NROW(J)(7:8)
              V2(J) = NROW(J)(5:6)
              CALL HEX_TO_SINT(NROW(J)(3:4),V3(J))
              CALL HEX_TO_SINT(NROW(J)(1:2),V4(J))
              V5(J) = NROW(J)(1:4)
              CALL HEX_TO_INT(V5(J), INT)
              ENERGY = FLOAT(INT) / 4.0
              IF (NROW(J).NE. '00000000') THEN
                IF (J .EQ. 1) THEN
                   WRITE(LUN,*)
                   WRITE(LUN,109) I
                   WRITE(LUN,*)  '           ================'
                   WRITE(LUN,*)
                  WRITE(LUN,104)'Term Number generated this entry',
     &                            V1(J)
                  WRITE(LUN,104)'Local DSP Tool generated this entry',
     &                            V2(J)
                  WRITE(LUN,101)'Eta coordinate of the Trigger Tower',
     &                            V3(J)
                  WRITE(LUN,101)'Phi coordinate of the Trigger Tower',
     &                            V4(J)
                ELSEIF(J .EQ. 2) THEN
                  WRITE(LUN,104)'Object Type Code of this entry',
     &                            V1(J)
                  WRITE(LUN,104)'Real or Mark and Pass Data category',
     &                          V2(J)
                  WRITE(LUN,105) 'Object Energy (in Gev)', ENERGY
                ELSE
  110             FORMAT(' Tool Specific information # ', I1,
     &                   '              : ', A)
                  WRITE(LUN,110) J-2, NROW(J)
                ENDIF
              ENDIF
            ENDDO
          ENDDO
        ELSE
          WRITE(LUN,*)'Number of entries exceeds its maximum value'
        ENDIF
C----------------------------------------------------------------------
  999 RETURN
      END
