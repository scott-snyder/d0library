      PROGRAM SURVEY_RCP
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Convert W. Smart's Survey format to
C-                         RCP format 
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  14-NOV-1990   Stephen Kahn
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER CARD*80, LAB1*4, LAB2*4, LAB3*4, LAST*8
      REAL X, Y, Z, XF(3), DL
      INTEGER IWORD, IARRAY, LUNI, LUNO
      DATA LAST /'        '/
      DATA LUNI /10/, LUNO /11/, IWORD /0/, IARRAY /0/
C
C     OPEN(UNIT=LUNI, FILE='CC_SURVEY.DAT', FORM='FORMATTED',
      OPEN(UNIT=LUNI, FILE='CC_TEV.PHY', FORM='FORMATTED',
     +    ACCESS='SEQUENTIAL', STATUS='OLD')
      OPEN(UNIT=LUNO, FILE='CC_SURVEY.RCP', FORM='FORMATTED',
     +    ACCESS='SEQUENTIAL', STATUS='NEW')
C
  100 READ( LUNI,'(1X,A)', END=500, ERR=510) CARD
      IF(CARD(8:11) .EQ. 'Data') THEN
        READ(CARD, FMT=110) DL, XF
  110   FORMAT(18X,F7.5,5X,F6.3,1X,F8.3,1X,F6.3)
        WRITE (LUNO, 120) CARD
  120   FORMAT('!',A80)
      ELSE IF (CARD(1:2).EQ.'CH' .OR. CARD(1:2).EQ.'FH' .OR.
     +    CARD(1:2).EQ.'EM') THEN
        IF( IWORD.EQ.0) THEN   ! first read data -- write alpha, xf
          WRITE(LUNO, 130) DL
  130     FORMAT(' THERMAL_COEFF ',F10.5)
          WRITE(LUNO, 140) XF
  140     FORMAT(' \ARRAY FIXED_POINT ',/,3(5X,F8.3),/' \END')
          IWORD = 4
          IARRAY = 2
        END IF
        LAB1 = 'CC'//CARD(1:2)
        LAB2 = CARD(3:6)
        LAB3 = CARD(7:10)
        READ(CARD,'(11X,3F13.3)') X, Y, Z        
        IF( LAB1 .NE. LAST(1:4) .OR. LAB2(2:4) .NE. LAST(6:8)) THEN
          IF(IARRAY .NE. 2) WRITE(LUNO, 145)
  145     FORMAT(' \END')
          WRITE(LUNO, 150) LAB1(3:4),LAB2(2:3)
  150     FORMAT(' \ARRAY ',2A2)
        END IF
        LAST = LAB1//LAB2
        WRITE(LUNO, 160) LAB1, LAB2, LAB3, X, Y, Z
  160   FORMAT(2X,3(1H',A4,1H',1X),3(F10.3,1X))
        IWORD = IWORD + 6
        IARRAY = IARRAY + 1
      ELSE
        WRITE(LUNO, 120) CARD
      END IF
      GO TO 100
C
  500 WRITE(6, '(2I10)') IWORD, IARRAY
      STOP 500
  510 WRITE(6,*)  ' READ ERROR '
      STOP 510
C----------------------------------------------------------------------
      END
