      LOGICAL FUNCTION QCD_NTUP_FILTER
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : check event vs requested filters
C-                         and skip processing if requested filt(s) missing
C-   Returned value  :
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  23-JAN-1996   Bob Hirosky
C-   Updated  29-MAR-1996   Andrew Brandt  add L2NAME_PASSED_EXCL choice
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:QCD_NTUP_INFO.INC/LIST'
      INCLUDE 'D0$INC:QCD_JUTL_HEAD.INC/LIST'
      LOGICAL L2NAME_PASSED, OK, QCDJETS, QCD_TRIGON
      LOGICAL L2NAME_PASSED_EXCL
      CHARACTER*32 L2TRIG
      INTEGER IBSET, JBIT, I
C----------------------------------------------------------------------
      QCD_NTUP_FILTER = .FALSE.
C
C Set path as chosen by RCP switch
C
        CALL PATHST(UPATH)
C
C  Get JUTL information
C
      CALL QCD_UPK_JUTL_HEAD
C
C
C  Check for requested QCD bits if UNFILTERED and NOQCD are false
C  UNFILTERED .TRUE. allows you to skip check of filter bits
C  If NOQCD is true check L2names and make new bit mask
C
      IF(.NOT.UNFILTERED) THEN
        IF(NOQCD) THEN
          FILT_MASK=0
          FILT_MASK2=0
          DO I=1,NNOQCD
            L2TRIG=NOQCD_TRIG(I)
            IF(I.LE.32) THEN
C
C Add possibility of using exclusive names
C
              IF(EXCLU_NAME) THEN
                IF(L2NAME_PASSED_EXCL(L2TRIG))
     &             FILT_MASK=IBSET(FILT_MASK,I-1)
              ELSE
                IF(L2NAME_PASSED(L2TRIG))
     &             FILT_MASK=IBSET(FILT_MASK,I-1)
              END IF
            ELSE
              IF(EXCLU_NAME) THEN
                IF(L2NAME_PASSED(L2TRIG))
     &             FILT_MASK2=IBSET(FILT_MASK2,I-33)
              ELSE
                IF(L2NAME_PASSED(L2TRIG))
     &             FILT_MASK2=IBSET(FILT_MASK2,I-33)
              END IF
            END IF
          END DO
          IF(FILT_MASK.EQ.0.AND.FILT_MASK2.EQ.0) GO TO 999
C
C Set bit 14 to make sure word is stored correctly
C
          FILT_MASK=IBSET(FILT_MASK,14)
          FILT_MASK2=IBSET(FILT_MASK2,14)
        ELSE
C
C Initialize QCD_MASK and verify QCD event
C
          OK=QCDJETS()
          IF(.NOT.OK) GO TO 999
C
C Check if filter is requested
C
          IF(.NOT.QCD_TRIGON()) GO TO 999
C
C Set bit 14 to make sure word is stored correctly
C
          FILT_MASK=IBSET(FILT_MASK,14)
        ENDIF  !(.NOT.UNFILTERED)
C
C ****  DECODE FILT_MASK INTO LOGICAL ARRAY
C
        DO I = 1,64
          IF (I.LE.32) THEN           ! Exclude bits which are always set
            QCDFILT(I) = (JBIT(FILT_MASK,I).EQ.1) .AND. (I .NE. 15)
          ELSE
            QCDFILT(I) = (JBIT(FILT_MASK2,I-32).EQ.1) .AND. (I .NE. 47)
          ENDIF
        ENDDO
      ENDIF
C
      QCD_NTUP_FILTER = .TRUE.
C
  999 RETURN
      END
