C DEC/CMS REPLACEMENT HISTORY, Element HISTIT.FOR
C *1    28-SEP-1987 14:41:03 JONCKHEERE "Histogram Gain/Peds"
C DEC/CMS REPLACEMENT HISTORY, Element HISTIT.FOR
      SUBROUTINE HISTIT(TMAIN,SCREEN,NBLS1,NBLS2,IADC,NOFF,CONT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fill and print Histograms
C-
C-   Inputs  : TMAIN = Main title
C-             SCREEN= Logical flag for Screen/File
C-             NBLS1 = FIRST BLS
C-             NBLS2 = LAST  BLS
C-             IADC  = ADC
C-             NOFF  = Offset for Hist ID's (by bank)
C-             CONT  = Contents of the bank
C-   Outputs : Histograms of contents /w errors
C-
C-   Created  23-SEP-1987   A.M.Jonckheere
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*20 TMAIN        ! MAIN TITLE
      INTEGER NBLS1,NBLS2,IADC,NOFF
      LOGICAL SCREEN
      INTEGER ID,ICHAN,IBLS
      REAL VAL(48),ERVAL(48),CONT(768)
      CHARACTER*20 THARD        ! HARDWARE TITLE
      CHARACTER*60 TITLE        ! HISTOGRAM TITLE
C----------------------------------------------------------------------
      DO IBLS=NBLS1,NBLS2
        WRITE(THARD,101) IADC,IBLS
        ID=IADC*10+IBLS
        TITLE=THARD//TMAIN//'$'
        CALL HBOOK1(ID,%REF(TITLE),48,0.,48.)
        DO ICHAN=1,48                         
          VAL(ICHAN)  =CONT(IBLS*96+2*ICHAN-1)
          ERVAL(ICHAN)=CONT(IBLS*96+2*ICHAN)
        END DO              
        CALL HPAK(ID,VAL)
        CALL HPAKE(ID,ERVAL)
        CALL HBARX(ID)
        CALL HMINIM(ID,0.)
        CALL HPRINT(ID)
        IF(SCREEN) CALL HDELET(ID)
      END DO
  101 FORMAT(' ADC=',I2,'; BLS=',I2)
  999 RETURN
      END
