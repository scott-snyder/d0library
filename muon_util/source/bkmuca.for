      SUBROUTINE BKMUCA(NC,ITR,LMUCA)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book MUCA bank
C-
C-   Inputs  :
C-              NC     number of cells
C-              ITR    muon track number
C-   Outputs :
C-              LMUCA  address of bank, MUCA
C-   Controls:
C-
C-   Created  08-APR-1991   S. ABACHI
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER NC,ITR,LMUCA
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZMUCA.LINK'
C
      INTEGER IOH,LMUON,GZMUON,NDAT,NR
      LOGICAL FIRST
      SAVE FIRST, IOH
      DATA FIRST /.TRUE./
C----------------------------------------------------------------------
C
      IF(FIRST) THEN
        CALL MZFORM('MUCA','3I/3I2F',IOH)
        FIRST = .FALSE.
      ENDIF
C
      LMUON = GZMUON(ITR)
      NR = 5
      NDAT = NC*NR + 3
C
      CALL MZBOOK(IXMAIN,LMUCA,LMUON,-IZMUCA,
     &                      'MUCA',1,1,NDAT,IOH,-1)
      IQ(LMUCA + 1) = 1
      IQ(LMUCA + 2) = NR
      IQ(LMUCA + 3) = NC
C
  999 RETURN
      END
