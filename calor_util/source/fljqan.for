      SUBROUTINE FLJQAN(LJETS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Add link fron JETS bank to correction factor
C-                         quantities
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  18-JUL-1997   Bob Hirosky
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZJQAN.LINK'
      INTEGER LJETS, LJQAN, I
      REAL JET_QUANS(50), SCALE
C----------------------------------------------------------------------
      LJQAN = LQ(LJETS-IZJQAN)
      IF (LJQAN.NE.0) THEN
        CALL ERRMSG('JQAN already exists!','FLJQAN',
     &                'correction quantities filling skipped','W')
        GOTO 999
      ENDIF
      CALL BKJQAN(LJQAN)
      CALL QCD_JET_CORRECTION_QUANS(JET_QUANS)
      DO I = 1,50
        Q(LJQAN+I) = JET_QUANS(I)
      ENDDO
      LQ(LJETS-IZJQAN) = LJQAN
  999 RETURN
C----------------------------------------------------------------------
      ENTRY JQUAN_EMFIX(LJETS,SCALE)
C-    corrects JQAN factors for jets w/ EM clusters removed 
C-    scale correction fractions/errors by (JET_ET-EM_ET)/JET_ET
C----------------------------------------------------------------------
C
C jet quans energy fraction words are incorrect if EM clusters are restored
C get these words from JETS bank (now fixed up by CORRECT_JETS_EM_REMOVE)
C
      Q(LJQAN+1) = Q( LJETS + 14)
      Q(LJQAN+2) = Q( LJETS + 17)
      Q(LJQAN+3) = Q( LJETS + 18)
      DO I = 4,50
        Q(LJQAN+I) = Q(LJQAN+I)*SCALE
      ENDDO
C
      RETURN
      END
