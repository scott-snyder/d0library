      INTEGER FUNCTION MTC_IWHERE(NETA,NPHI,NLYR)
C----------------------------------------------------------------------
C- MTC_IWHERE: part of MTC (Muon Tracking in the Calorimeter) package
C-
C-   Purpose and Methods : tell me which part of the calorimeter I
C-      am in.
C-                                      layer   eta
C-   Returned value : MTC_IWHERE = 1 CCEM  1-7     (0.,1.2)
C-                               = 2 ECEM  1-7     (1.3,4.1)
C-                               = 3 CCMG  8       (.7,1.2)
C-                               = 4 ICD   9       (.8,1.4)
C-                               = 5 ECMG  10      (0.7,1.4)
C-                               = 6 CCFH  11-13   (0.,1.0)
C-                               = 7 ECIH  11-15   (1.6,4.5 to 5.2)
C-                               = 8 ECMH  11-15   (1.0,2.0)
C-                               = 9 CCCH  15      (0.,0.6)
C-                               =10 ECOH  15-17   (0.7,1.5)
C-                               = 0 calorimeter cell dne (does not exist)
C-                                 (or it is ganged to another cell)
C-   Inputs  : (neta,nphi,nlyr)
C-   Outputs : integer MTC_IWHERE ranging from 1-10
C-             returns zero if cell does not exist
C-
C-   Created  27-MAY-1993   Elizabeth Gallas
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER NETA,NPHI,NLYR
C----------------------------------------------------------------------
      MTC_IWHERE = 0
C- this routine must be run top down
      IF(NETA.EQ.0) RETURN
C- is it an EM layer ?
      IF(NLYR.LE.7) THEN
        IF(ABS(NETA).LE.13) THEN
C-        must be CCEM unless it dne ...
          MTC_IWHERE = 1
          IF(NETA.EQ.+12 .AND. NLYR.GE.5) MTC_IWHERE = 0
          IF(NETA.EQ.-12) THEN
            IF(NLYR.EQ.3 .OR. NLYR.EQ.4 .OR. NLYR.EQ.7)
     &        MTC_IWHERE = 0
C            IF(NLYR.EQ.5 .OR. NLYR.EQ.6 .OR. NLYR.EQ.1 .OR. NLYR.EQ.2)
C     &        MTC_IWHERE = 1
          END IF
          IF(ABS(NETA).EQ.13) MTC_IWHERE = 0
          RETURN
        END IF
        IF(ABS(NETA).LE.35) THEN
C-        must be an ECEM unless it dne ...
          MTC_IWHERE = 2
          IF(NETA.EQ.+14 .AND. NLYR.LE.4) MTC_IWHERE = 0
          IF(NETA.EQ.-14) THEN
            IF(NLYR.EQ.1 .OR. NLYR.EQ.2 .OR. NLYR.EQ.5 .OR. NLYR.EQ.6)
     &        MTC_IWHERE = 0
          END IF
          IF(( NLYR.GE.4 .AND. NLYR.LE.6 ).AND.
     &       ( ABS(NETA).GE.27 )) MTC_IWHERE = 0
          RETURN
        END IF
C- it is not an EM layer, is it MG or ICD ?
      ELSE IF(NLYR.EQ.8) THEN
C-      must be an CCMG unless it dne ...
        IF(ABS(NETA).GE.8 .AND. ABS(NETA).LE.12) MTC_IWHERE =  3
        RETURN
      ELSE IF(NLYR.EQ.9) THEN
C-      must be an ICD unless it dne ...
        IF(ABS(NETA).GE.9 .AND. ABS(NETA).LE.14) MTC_IWHERE =  4
        RETURN
      ELSE IF(NLYR.EQ.10) THEN
C-      must be an ECMG unless it dne ...
        IF(ABS(NETA).GE.8 .AND. ABS(NETA).LE.13) MTC_IWHERE =  5
        RETURN
C- it is not an EM layer, nor is it MG or ICD,
C- It must be CCFH,CH,ECOH,MH or IH
      ELSE IF(NLYR.GE.11 .AND. NLYR.LE.17) THEN
C- find ECOH by brute force (ignore OCH3 tied to MCH)
        IF(NLYR.EQ.15 .AND.
     &    (ABS(NETA).GE.8 .AND. ABS(NETA).LE.12)) MTC_IWHERE = 10
        IF(NLYR.EQ.16 .AND.
     &    (ABS(NETA).GE.9 .AND. ABS(NETA).LE.13)) MTC_IWHERE = 10
        IF(NLYR.EQ.17 .AND.
     &    (ABS(NETA).GE.11 .AND. ABS(NETA).LE.14)) MTC_IWHERE = 10
        IF(MTC_IWHERE.EQ.10) RETURN
C- there are no more layers ge 16 ...
        IF(NLYR.GE.16) RETURN
C- Is it CCCH (check for main ring beam pipe around iphi=18)?
        IF(NLYR.EQ.15 .AND. ABS(NETA).LE.6) THEN
          MTC_IWHERE =  9
          IF( (NPHI.GE.17 .AND. NPHI.LE.19) ) MTC_IWHERE = 0
          RETURN
        END IF
C- Is it CCFH ?
        IF(NLYR.LE.13 .AND. (ABS(NETA)+NLYR).LE.21) THEN
          MTC_IWHERE = 6
          RETURN
        END IF
C- Is it ECIH ?
        IF(( ABS(NETA)-NLYR ).GE.6) THEN
          MTC_IWHERE = 7
          IF(NLYR.LE.12) THEN
            IF(ABS(NETA).GE.37) MTC_IWHERE = 0
          ELSE
            IF(ABS(NETA).GE.38) MTC_IWHERE = 0
          END IF
          RETURN
        END IF
C- Is it ECMH ?
        IF( (NLYR-ABS(NETA).LE.1) .AND. (NLYR-ABS(NETA).GE.-5)   ) THEN
          MTC_IWHERE = 8
          IF(NLYR.LE.13 .AND. NLYR-ABS(NETA).EQ.1) MTC_IWHERE = 0
          IF( (NLYR.EQ.13.OR.NLYR.EQ.14) .AND.
     &        (NLYR-ABS(NETA).EQ.-5) ) MTC_IWHERE = 0
          RETURN
        ELSE
          MTC_IWHERE = 0
        END IF
      END IF
C----------------------------------------------------------------------
  999 RETURN
      END
