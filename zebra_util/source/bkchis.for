      SUBROUTINE BKCHIS(NDATA)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book the Bank CHIS.
C-   CONTAINS CHISQUARED CONTRIBUTIONS
C-   OF INDIVIDUAL LAYERS
C-
C-   Inputs  : NDATA = Number of words to book in Bank
C-   Outputs : Link of Booked CHIS Bank
C-   Controls: None
C-
C-   Updated  27-JUN-1992   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IXIO
C
C--   ZEBRA BANKS
      INCLUDE 'D0$INC:ZEBSTP.INC/LIST'
      INCLUDE 'D0$INC:ZHMATRIX.INC'
C
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C
      INTEGER NDATA
      INTEGER NPREV
C----------------------------------------------------------------------
C
C--   INITIALIZE
C
      IF(FIRST)THEN
C
        CALL MZFORM('CHIS','-F',IXIO)        ! Describe Bank format
C
      ENDIF
C
      IF ( LCHIS.NE.0 ) THEN
        NPREV = IC(LCHIS-1) !NUMBER OF WORDS IN EXISTING BANK.

        IF ( NPREV.LT.NDATA ) THEN
          CALL MZDROP(IXSTP,LCHIS,' ')
          LCHIS = 0
        ENDIF
      ENDIF
C
      IF(LCHIS.EQ.0)THEN
C
        CALL MZBOOK
     &    (IDVSTP,LCHIS,0,2,'CHIS',0,0,NDATA,IXIO,0)
      ENDIF
  999 RETURN
      END
