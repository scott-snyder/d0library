      LOGICAL FUNCTION MTC_LEXIST(IETAIN,IPHIIN,ILYR)
C----------------------------------------------------------------------
C- MTC_LEXIST: part of MTC (Muon Tracking in the Calorimeter) package
C-
C-   Purpose and Methods : Determine whether the input cal cell specified
C-      by ietain,iphiin,ilyr exists in the arrays of /MTC_ETOWERS/
C-
C-   Returned value  : true if the cell exists, false if not
C-   Inputs  : IETAIN,IPHIIN the IETA,IPHI index relative to the current
C-      IETATOWER(ILYR),IPHITOWER(ILYR) in /MTC_ETOWERS/
C-
C-   Created  24-JAN-1994   Elizabeth Gallas
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:MTC_ETOWERS.INC'
C- input
      INTEGER IETAIN,IPHIIN,ILYR
C- local
      INTEGER IETA,IPHI, IETA2,IPHI2, JPHI, IHERE
C- functns
      INTEGER MTC_IWHERE
C----------------------------------------------------------------------
      IF(ILYR.LE.0 .OR. ILYR.GE.18) GO TO 666
      IF(ABS(IETAIN).GT.ITTHI .OR. ABS(IPHIIN).GT.ITTHI) GO TO 666

      IETA = IETATOWER(ILYR)
      IPHI = IPHITOWER(ILYR)

      IETA2 = IETA + IETAIN
      IF(IETA2.EQ.0) IETA2 = IETA2 + IETAIN
      IF(ABS(IETA2).GE.38) GO TO 666

      IPHI2 = IPHI + IPHIIN
      JPHI = IPHI2
      IF (IPHI2.GE.65) JPHI = MOD(IPHI2,64)
      IF (IPHI2.LE.0)  JPHI = 64 + IPHI2
      IPHI2 = JPHI

C- determine which calorimeter section I am in ...
      IHERE = MTC_IWHERE(IETA,IPHI,ILYR)
C- my convension is that EM3 includes EM4-EM6 so reset IHERE if neccessary
      IF(ABS(IETA).LE.26 .AND. ILYR.EQ.3) THEN
        IF(IHERE.EQ.0 .AND.
     &        ABS(IETA).NE.12 .AND. ABS(IETA).NE.14) GO TO 666
        IF(ABS(IETA).EQ.14) IHERE = 2       ! ECEM
        IF(ABS(IETA).EQ.12) IHERE = 1       ! CCEM
      END IF

      MTC_LEXIST = .TRUE.
      GO TO 999
C----------------------------------------------------------------------
  666 CONTINUE
      MTC_LEXIST = .FALSE.
C----------------------------------------------------------------------
  999 RETURN
      END
