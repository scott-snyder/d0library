      PROGRAM ECS_SURVEY_RCP
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Convert B. Cooper's Survey format to
C-                         RCP format 
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Author:   Stephen Kahn       23-Aug-1991
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER CARD*80, LAB1*4, LAB2*4, LAB3*4, LAB4*4, LAST*8
      REAL X, Y, Z, XF(3), DL
      INTEGER IWORD, IARRAY, LUNI, LUNO, ISIZE, JSIZE
      LOGICAL USE_THERMAL, US
      INTEGER I
      DATA LAST /'        '/
      DATA LUNI /10/, LUNO /11/, IWORD /0/, IARRAY /0/
      DATA ISIZE, JSIZE /2932, 127/
      DATA DL/ 0.99727 /
      DATA XF / 3*0. /
      DATA USE_THERMAL /.FALSE./
C
      OPEN(UNIT=LUNI, FILE='ECS_TEV.PHY', FORM='FORMATTED',
     +    ACCESS='SEQUENTIAL', STATUS='OLD')
      OPEN(UNIT=LUNO, FILE='ECS_SURVEY.RCP', FORM='FORMATTED',
     +    ACCESS='SEQUENTIAL', STATUS='NEW')
C
      WRITE(LUNO,20)
   20 FORMAT(' \START         ECS_SURVEY_RCP ')
      WRITE(LUNO,30) ISIZE, JSIZE
   30 FORMAT(' \SIZE         ',2I10)
      WRITE(LUNO,40) USE_THERMAL
   40 FORMAT(' USE_THERMAL      ',L2)
      WRITE(LUNO,50) DL
   50 FORMAT(' THERMAL_COEFF    ',F10.5)
      WRITE(LUNO,60) XF
   60 FORMAT(' \ARRAY FIXED_POINT'/3F13.4/' \END')
C
  100 READ( LUNI,'(1X,A)', END=500, ERR=510) CARD
      IF (CARD(1:3).EQ.'NMH' .OR. CARD(1:3).EQ.'NOH' .OR.
     +    CARD(1:3).EQ.'NIH' .OR. CARD(1:3).EQ.'SMH' .OR.
     +    CARD(1:3).EQ.'SOH' .OR. CARD(1:3).EQ.'SIH' .OR.
     +    CARD(1:2) .EQ. 'EM') THEN
        US = .FALSE.      ! flag to indicate whether blank is imbedded
        DO 110 I = 12, 1, -1  ! descend along label characters
          IF( CARD( I:I) .NE. ' ') THEN
            US = .TRUE.   ! from here on space will be inbedded
          ELSE IF (US) THEN
            CARD( I:I) = '_'    ! imbedded blanks replaced by '_'
          END IF
  110   CONTINUE
        LAB1 = 'ECS_' 
        LAB2 = CARD(1:4)
        LAB3 = CARD(5:8)
        LAB4 = CARD(9:12)
C        IF(LAB2 .EQ. 'EM-E') THEN
C          IF(CARD(8:10) .EQ. 'EST') CARD(8:11) = 'EAST'
C          IF(CARD(8:10) .EQ. 'WST') CARD(8:11) = 'WEST'
C          LAB2 = 'EM'//CARD(8:9)
C          LAB3 = CARD(10:12)
C          LAB4 = '   '
C        END IF
        IF(CARD(4:8) .EQ. 'BOT_.') LAB3 = 'OTT.'
        READ(CARD,'(14X,3F13.5)') X, Y, Z        
          WRITE(LUNO, 150) LAB1, LAB2, LAB3, LAB4
  150     FORMAT(' \ARRAY ',4A4)
        LAST = LAB1//LAB2
        WRITE(LUNO, 160) LAB1, LAB2, LAB3, LAB4, X, Y, Z
  160   FORMAT(2X,4(1H',A4,1H',1X),3(F10.3,1X))
        WRITE(LUNO, 165)
  165   FORMAT(' \END')
        IWORD = IWORD + 6
        IARRAY = IARRAY + 1
      END IF
      GO TO 100
C
  500 WRITE(6, '(2I10)') IWORD, IARRAY
      STOP 500
  510 WRITE(6,*)  ' READ ERROR '
      STOP 510
C----------------------------------------------------------------------
      END
