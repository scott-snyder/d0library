      SUBROUTINE ZDCDDN(ICDD,PRUNIT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Dump out the contents of ZEBCOM surrounding
C-                         and including the selected CDD bank.  Start
C-                         ten words before bank and end ten words after
C-                         bank.
C-
C-   Inputs  : ICDD   = CDD bank desired (1-4)
C-             PRUNIT = Unit to receive printout
C-   Outputs : printout of bank
C-
C-   Created   3-JUL-1990   Jeffrey Bantly
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZCDD1.LINK'
      INTEGER ICDD,PRUNIT
      INTEGER LCDDNU,CDDEND,J
      INTEGER WORD,ISTART,IEND
C&IF VAXVMS
      INTEGER*4  WORD4(1)
      INTEGER*2  WORD2(2)
      BYTE       WORD1(4)
      EQUIVALENCE (WORD4,WORD2(1)),(WORD4,WORD1(1))
C&ENDIF
C----------------------------------------------------------------------
      IF(ICDD.LT.1 .OR. ICDD.GT.4) GOTO 999
      LCDDNU= LQ(LHEAD-IZCDD1-(ICDD-1))
      IF(LCDDNU .LE. 0 ) GOTO 999
      CDDEND = LCDDNU + IQ(LCDDNU-1)
      WRITE(PRUNIT,2)
    2 FORMAT(' LONGWORD',11X,'WORD',5X,'HALF1',5X,'HALF2',4X,
     &         'BYTE1',4X,'BYTE2',4X,'BYTE3',4X,'BYTE4')
      ISTART = LCDDNU-10
      IEND = CDDEND+10
C&IF VAXVMS
      DO J=ISTART,IEND
        WORD=IQ(J)
        WORD4(1) = WORD
        WRITE(PRUNIT,1) J,IQ(J),WORD2(1),WORD2(2),
     &             WORD1(1),WORD1(2),WORD1(3),WORD1(4)
    1   FORMAT( I9,I15,2I10,4I9)
      END DO                        ! J loop
C&ENDIF
C----------------------------------------------------------------------
  999 RETURN
      END
