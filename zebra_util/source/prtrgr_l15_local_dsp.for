      SUBROUTINE PRTRGR_L15_LOCAL_DSP(LUN, L15_LDSP_BLOCK, NLWF)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Print out the information from the
C-                         L1.5 Datablock Local DSP
C-
C-   Inputs  : LUN          The unit number to write the information to
C-             L15_LDSP_BLOCK The array of the Local DSP words
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
      CHARACTER*8 PRTRGR_INT_TO_HEX, MROW(11), NROW(LOC_NLW_ENT)
      EXTERNAL PRTRGR_INT_TO_HEX
      CHARACTER*2 FIRST(11), SECOND(11), THIRD(11), LAST(11)
      CHARACTER*2 V1(LOC_NLW_ENT), V2(LOC_NLW_ENT)
      CHARACTER*4 V5(LOC_NLW_ENT)
      INTEGER     V3(LOC_NLW_ENT), V4(LOC_NLW_ENT)
C
      INTEGER LUN, L15_LDSP_BLOCK(L15CAL_LOCAL_DSP_LENGTH), NLWF
      INTEGER I, J, K, LDSP_ID, ENT_NUM, INT, STATUS
      REAL    ENERGY
      INTEGER LDBLOCK_START, PBLOCK_START, LBLOCK_START
C
C----------------------------------------------------------------------
C
C
  100 FORMAT(' ', A, T45, ': ', F6.2)
  104 FORMAT(' ', A, T45, ': ', I4)
  108 FORMAT(' ', A, T45, ': ', I3)
  109 FORMAT(' ', A, T45, ': ', A, ' (H)')
      WRITE (LUN, 104) 'Local DSP Longwords to follow',
     &                  L15_LDSP_BLOCK(1)
      WRITE(LUN,*)
      NLWF = L15_LDSP_BLOCK(1)
C
        DO I = 1, 11
          IF(I .EQ. 1)THEN
            WRITE(LUN,*)
            WRITE(LUN,*)'Header from DSP A2 (eta -20:-19)'
            WRITE(LUN,*)'================================'
            WRITE(LUN,*)
          ELSEIF(I .EQ. 2)THEN
            WRITE(LUN,*)
            WRITE(LUN,*)'Header from DSP A3 (eta -18:-15)'
            WRITE(LUN,*)'================================'
            WRITE(LUN,*)
          ELSEIF(I .EQ. 3)THEN
            WRITE(LUN,*)
            WRITE(LUN,*)'Header from DSP A4 (eta -14:-11)'
            WRITE(LUN,*)'================================'
            WRITE(LUN,*)
          ELSEIF(I .EQ. 4)THEN
            WRITE(LUN,*)
            WRITE(LUN,*)'Header from DSP A1 (eta -10:-7)'
            WRITE(LUN,*)'================================'
            WRITE(LUN,*)
          ELSEIF(I .EQ. 5)THEN
            WRITE(LUN,*)
            WRITE(LUN,*)'Header from DSP B3 (eta -6:-3)'
            WRITE(LUN,*)'=============================='
            WRITE(LUN,*)
          ELSEIF(I .EQ. 6)THEN
            WRITE(LUN,*)
            WRITE(LUN,*)'Header from DSP B4 (eta -2:+2)'
            WRITE(LUN,*)'=============================='
            WRITE(LUN,*)
          ELSEIF(I .EQ. 7)THEN
            WRITE(LUN,*)
            WRITE(LUN,*)'Header from DSP B1 (eta +3:+6)'
            WRITE(LUN,*)'=============================='
            WRITE(LUN,*)
          ELSEIF(I .EQ. 8)THEN
            WRITE(LUN,*)
            WRITE(LUN,*)'Header from DSP C3 (eta +7:+10)'
            WRITE(LUN,*)'==============================='
            WRITE(LUN,*)
          ELSEIF(I .EQ. 9)THEN
            WRITE(LUN,*)
            WRITE(LUN,*)'Header from DSP C4 (eta +11:+14)'
            WRITE(LUN,*)'================================'
            WRITE(LUN,*)
          ELSEIF(I .EQ. 10)THEN
            WRITE(LUN,*)
            WRITE(LUN,*)'Header from DSP C1 (eta +15:+18)'
            WRITE(LUN,*)'================================'
            WRITE(LUN,*)
          ELSE
            WRITE(LUN,*)
            WRITE(LUN,*)'Header from DSP C2 (eta +19:+20)'
            WRITE(LUN,*)'================================'
            WRITE(LUN,*)
          ENDIF
          LDSP_ID = (I-1)*LOC_NUM_ENT*LOC_NLW_ENT + I + 1
          MROW(I) = PRTRGR_INT_TO_HEX(L15_LDSP_BLOCK(LDSP_ID))
          FIRST(I)  = MROW(I)(7:8)
          SECOND(I) = MROW(I)(5:6)
          THIRD(I)  = MROW(I)(3:4)
          LAST(I)   = MROW(I)(1:2)
          WRITE(LUN,109)'DSP source of this list', FIRST(I)
          WRITE(LUN,109)'Number of Valid Entries', SECOND(I)
          WRITE(LUN,109)'Number of Entries in this list', THIRD(I)
          WRITE(LUN,109)'Number of Longwords per entry', LAST(I)
          WRITE(LUN,*)
          CALL HEX_TO_INT(SECOND(I), ENT_NUM)
          IF (ENT_NUM .LE. LOC_NUM_ENT) THEN
            DO J = 1, ENT_NUM
  110         FORMAT('                ENTRY NUMBER :', I2 )
              DO K=1, LOC_NLW_ENT
                LDSP_ID = (I-1)*LOC_NUM_ENT*LOC_NLW_ENT+
     &                    (J-1)*LOC_NLW_ENT + I + K + 1
                NROW(K)=PRTRGR_INT_TO_HEX(L15_LDSP_BLOCK(
     &                                    LDSP_ID))
                V1(K) = NROW(K)(7:8)
                V2(K) = NROW(K)(5:6)
                CALL HEX_TO_SINT(NROW(K)(3:4),V3(K))
                CALL HEX_TO_SINT(NROW(K)(1:2),V4(K))
                V5(K) = NROW(K)(1:4)
                CALL HEX_TO_INT(V5(K), INT)
                ENERGY = FLOAT(INT) / 4.0
                IF (NROW(K).NE.'00000000') THEN

                  IF (K .EQ. 1) THEN
                    WRITE(LUN,*)
                    WRITE(LUN,110) J
                    WRITE(LUN,*)  '               ================'
                    WRITE(LUN,*)
                    WRITE(LUN,109)
     &              'Term Number generated this entry', V1(K)
                    WRITE(LUN,109)
     &              'Local DSP Tool generated this entry', V2(K)
                    WRITE(LUN,108)
     &              'Eta coordinate of the Triger Tower', V3(K)
                    WRITE(LUN,108)
     &              'Phi coordinate of the Triger Tower', V4(K)
                    WRITE(LUN,*)
                  ELSEIF(K .EQ. 2) THEN
                    WRITE(LUN,109)
     &              'Object Type Code of this entry',V1(K)
                    WRITE(LUN,109)
     &              'Real or Mark and Pass Data category', V2(K)
                    WRITE(LUN,100)
     &              'Object Energy (in GeV)', ENERGY
                    WRITE(LUN,*)
                  ELSE
  111               FORMAT(' Tool Specific information # ', I1,
     &                     '              : ', A)
                    WRITE(LUN,111) K-2, NROW(K)
                  ENDIF
                ENDIF
              ENDDO
            ENDDO
          ELSE
            WRITE(LUN,*) 'Number of entries exceeds its maximum value'
          ENDIF
        ENDDO
C----------------------------------------------------------------------
  999 RETURN
      END
