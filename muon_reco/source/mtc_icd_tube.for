      SUBROUTINE MTC_ICD_TUBE(IETA,IPHI,ICOUNT,ATUBE)
C----------------------------------------------------------------------
C- MTC_ICD_TUBE: part of MTC (Muon Tracking in the Calorimeter) package
C-
C-   Purpose and Methods : identify the tube type of the ICD tile
C-      at the input IETA and IPHI
C-
C-   Outputs : ATUBE - character*4 variable is
C-                      R647 for the Hamamatsu tubes,
C-                      PM60 for the Russian tubes and
C-                      NULL if the tile does not exist
C-             ICOUNT is the designated tile number at this eta,phi:
C-                      It ranges from 1 to 768
C-
C-   Created  17-JUL-1993   Elizabeth Gallas
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C- /MTC_CSFICD/ contains the channel to channel variation in MIPs/ADC
C-    from tile to tile in the ICD - CSF_ICD_N(1:384),CSF_ICD_S(1:384)
C-    AICD_TYPE(1:768) - icd tube type
      INCLUDE 'D0$INC:MTC_CSFICD.INC'
      INCLUDE 'D0$INC:MTC_ACSFICD.INC'
C- input
      INTEGER IETA, IPHI
C- output
      CHARACTER*4 ATUBE
      INTEGER ICOUNT
C- Local
      INTEGER ISETA,FIRST,ICHAR,IER
      DATA FIRST/0/
C----------------------------------------------------------------------
C- If they haven't been filled, Fill ICD PMT types for each channel ...
      IF(FIRST.EQ.0) THEN
        FIRST = 1
        IF(AICD_TYPE(1).ne.'PM60' .and. aicd_type(1).ne.'R647') THEN
          WRITE(6,*) ' MTC_ICD_TUBE:  initializing ICD_TYPE_RCP'
          CALL INRCP('ICD_TYPE_RCP',IER)
          IF(IER.EQ.0) THEN
            CALL EZPICK('ICD_TYPE_RCP')
            DO 5 ICHAR=1,768
              CALL EZGETC('AICD_TYPE',ICHAR,4,AICD_TYPE(ICHAR),IER)
    5       CONTINUE
            CALL EZRSET
          ELSE
            WRITE(6,*) ' MTC_ICD_TUBE:  ICD_TYPE_RCP IER = 0'
          ENDIF
        END IF
      END IF

      ISETA = ABS(IETA) - 8
      ICOUNT = 6 * (IPHI-1) + ISETA
      IF(IETA.LT.0) ICOUNT = ICOUNT + 384
      ATUBE  = AICD_TYPE(ICOUNT)
C----------------------------------------------------------------------
  999 RETURN
      END
