      INTEGER FUNCTION MTC_IWHLAYER(IHERE,ILYR)
C----------------------------------------------------------------------
C- MTC_IWHLAYER: part of MTC (Muon Tracking in the Calorimeter) package
C-
C-   Purpose and Methods :  Each calorimeter section has a different
C-      number of sublayers as indicated in the table below.
C-      Given the section number IHERE (range 1-10)
C-      and the layer number ILYR (range 1-17)
C-      determine the sublayer number MTC_IWHLAYER (range 1-5).
C-
C-   Returned value  :  MTC_IWHLAYER indicating the sublayer number
C-                      in calorimeter section IHERE
C-   Inputs  :  IHERE (det by prev call to MTC_IWHERE(ieta,iphi,ilyr))
C-              ILYR  standard CAL_OFFLINE.PARAMS layer number (1-17)
C-   Outputs :  MTC_IWHLAYER
C----------------------------------------------------------------------
C- IHERE  Caltype       Number of Sublayers
C-   1     CCEM          4
C-   2     ECEM          4
C-   3     CCMG          1
C-   4     ICD           1
C-   5     ECMG          1
C-   6     CCFH          3
C-   7     ECIH          5
C-   8     ECMH          5
C-   9     CCCH          1
C-  10     ECOH          3
C-
C-   Created   3-JUN-1993   Elizabeth Gallas
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IHERE,ILYR

      MTC_IWHLAYER = 0
C- CCCH, CCMG, ICD, ECMG
      IF(IHERE.EQ.9 .OR. IHERE.EQ.3 .OR. IHERE.EQ.4 .OR.
     &   IHERE.EQ.5 ) THEN
        MTC_IWHLAYER = 1
        RETURN
      END IF
C- CCEM
      IF(IHERE.EQ.1) THEN
        IF(ILYR.EQ.7) THEN
          MTC_IWHLAYER = 4
        ELSE IF(ILYR.GE.3 .AND. ILYR.LE.6) THEN
          MTC_IWHLAYER = 3
        ELSE IF(ILYR.EQ.2 .OR. ILYR.EQ.1) THEN
          MTC_IWHLAYER = ILYR
        ELSE
          WRITE(6,*) ' MTC_IWHLAYER: error',IHERE,ILYR
        END IF
        RETURN
      END IF
C- ECEM
      IF(IHERE.EQ.2) THEN
        IF(ILYR.EQ.7) THEN
          MTC_IWHLAYER = 4
        ELSE IF(ILYR.GE.3 .AND. ILYR.LE.6) THEN
          MTC_IWHLAYER = 3
        ELSE IF(ILYR.EQ.2 .OR. ILYR.EQ.1) THEN
          MTC_IWHLAYER = ILYR
        ELSE
          WRITE(6,*) ' MTC_IWHLAYER: error',IHERE,ILYR
        END IF
        RETURN
      END IF
C- CCFH
      IF(IHERE.EQ.6) THEN
        IF(ILYR.GE.11 .AND. ILYR.LE.13) THEN
          MTC_IWHLAYER = ILYR - 10
        ELSE
          WRITE(6,*) ' MTC_IWHLAYER: error',IHERE,ILYR
        END IF
        RETURN
      END IF
C- ECIH
      IF(IHERE.EQ.7) THEN
        IF(ILYR.GE.11 .AND. ILYR.LE.15) THEN
          MTC_IWHLAYER = ILYR - 10
        ELSE
          WRITE(6,*) ' MTC_IWHLAYER: error',IHERE,ILYR
        END IF
        RETURN
      END IF
C- ECMH
      IF(IHERE.EQ.8) THEN
        IF(ILYR.GE.11 .AND. ILYR.LE.15) THEN
          MTC_IWHLAYER = ILYR - 10
        ELSE
          WRITE(6,*) ' MTC_IWHLAYER: error',IHERE,ILYR
        END IF
        RETURN
      END IF
C- ECOH
      IF(IHERE.EQ.10) THEN
        IF(ILYR.GE.15 .AND. ILYR.LE.17) THEN
          MTC_IWHLAYER = ILYR - 14
        ELSE
          WRITE(6,*) ' MTC_IWHLAYER: error',IHERE,ILYR
        END IF
        RETURN
      END IF
C----------------------------------------------------------------------
  999 RETURN
      END
