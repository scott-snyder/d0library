      INTEGER FUNCTION MTC_ISUBETA(ICAL,ISUB,IETA)
C----------------------------------------------------------------------
C- MTC_ISUBETA: part of MTC (Muon Tracking in the Calorimeter) package
C-
C-   Purpose and Methods : Get my calorimeter sublayer eta index
C-      MTC_ISUBETA from
C-   Inputs  :
C-      ICAL - calorimeter section (range 1-10)
C-      ISUB - calorimeter sublayer in section ICAL (range 1-5)
C-      IETA - normal calorimeter eta
C-   Outputs : value of MTC_ISUBETA
C-      (range 1-22, 0 for nonexistant sublayer at this ieta)
C-
C-   Created  12-JUL-1993   Elizabeth Gallas
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
C- /MTC_CALSECTS/ contains arrays specifying all cal sections, sublayers and
C- eta ranges -- acaltype(10), isublayer(10), islnum(10,5), isleta(10,5)
      INCLUDE 'D0$INC:MTC_CALSECTS.INC'
      INCLUDE 'D0$INC:MTC_ACALSECTS.INC'

      INTEGER ICAL,ISUB,IETA
      INTEGER INUM, IN_ETA, ISIGN
C----------------------------------------------------------------------
C- check the input arguments ...
      IF(ICAL.LE.0 .OR. ICAL.GE.11) THEN
        WRITE(6,*) ' MTC_ISUBETA: ICAL = ',ICAL
        GO TO 666
      END IF
      IF(ISUB.LE.0 .OR. ISUB.GE.6) THEN
        WRITE(6,*) ' MTC_ISUBETA: ISUB = ',ISUB
        GO TO 666
      END IF
      IF(ABS(IETA).GE.38) THEN
        WRITE(6,*) ' MTC_ISUBETA: ieta = ',IETA
        GO TO 666
      END IF

      MTC_ISUBETA = 0
      ISIGN   = +1
      IF(IETA.LE.0) ISIGN = -1
      DO 10 INUM=1,ISLNUM(ICAL,ISUB)
        IN_ETA = ISIGN * (ISLETA(ICAL,ISUB) + (INUM-1))
        IF(IN_ETA.EQ.IETA) THEN
          MTC_ISUBETA = INUM
          RETURN
        END IF
   10 CONTINUE
  666 CONTINUE
      WRITE(6,80) ICAL,ISUB,IETA
   80 FORMAT(' !!! - MTC_ISUBETA fatal error ical,isub,ieta=',3I5)
C----------------------------------------------------------------------
  999 RETURN
      END
