      SUBROUTINE L1ESUM_MUON
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fills the TRGR ESUM summary bank from the
C-                         TRGR bank for muon L1 triggers
C-   Controls: None.
C-
C-   Created 12-JUL-1994  M. Fortner
C-
C----------------------------------------------------------------------
C
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:ESUM.PARAMS'
      REAL    ETMUON
      REAL    MU_ETA,MU_PHI
      INTEGER FLAG_WORD
      INTEGER MU_REGIONS
      PARAMETER(MU_REGIONS=7)
      INTEGER GZMTRG,JERR,MUON_L1(7)
      INTEGER MU_TASK,MU_TRIG,MU_REGION
      INTEGER CENTRAL,OCTANTS,QUADRANTS
      PARAMETER (CENTRAL=4)
      PARAMETER (OCTANTS=8)
      PARAMETER (QUADRANTS=4)
      INTEGER PHI_BIT(OCTANTS,MU_REGIONS)
      INTEGER ITRIG,NTRIG,ISEG,NSEG,IREGION
      INTEGER NPHI,IPHI
      REAL    FUDGE
      INTEGER EXTRA_PHI,OVERLAP_PHI
      PARAMETER (EXTRA_PHI=1)
      PARAMETER (OVERLAP_PHI=0)
      REAL    MUON_ETA_REGION(MU_REGIONS)
      INTEGER MU_PHI_SEG(OCTANTS)
      REAL    OCT_MUON_PHI_REGION(OCTANTS)
      REAL    QUAD_MUON_PHI_REGION(QUADRANTS)
C
      DATA MU_TASK/6/
      DATA MU_TRIG/0/
      DATA MU_REGION/4/
      DATA PHI_BIT/ 2, 3, 4, 5, 0, 0, 0, 0,
     &             12,13,14,15, 0, 0, 0, 0,
     &             12,13,14,15, 0, 0, 0, 0,
     &             12,13,14,15,16,17,18,19,
     &             12,13,14,15, 0, 0, 0, 0,
     &             12,13,14,15, 0, 0, 0, 0,
     &              2, 3, 4, 5, 0, 0, 0, 0/
      DATA MUON_ETA_REGION/-2.9,-2.05,-1.35,0.0,1.35,2.05,2.9/
      DATA OCT_MUON_PHI_REGION/0.396,1.18,1.96,2.74,3.52,4.32,5.1,5.88/
      DATA QUAD_MUON_PHI_REGION/0.78,2.34,3.92,5.5/
C
C  Create MTRG bank if muons not previously unpacked
C
      IF (GZMTRG(0).EQ.0) CALL MUANLZ(JERR,MU_TASK,MU_TRIG,MU_REGION)
      CALL MOTCCT(MUON_L1(4),MUON_L1(3),MUON_L1(5),
     &            MUON_L1(2),MUON_L1(6),MUON_L1(1),MUON_L1(7))
C
C  Find CCT objects
C
      ETMUON = 5.0  ! Dummy momentum to get past ESUM thresold cut
      MU_ETA = 0.0
      MU_PHI = 0.0
      FLAG_WORD= 0
      DO IREGION = 1, 7
        NTRIG = IBITS(MUON_L1(IREGION),0,2)
        IF(NTRIG.GT.0) THEN
          IF(IREGION.EQ.CENTRAL)THEN   !Find the Phi regions
            NSEG = OCTANTS
          ELSE
            NSEG = QUADRANTS
          ENDIF
          NPHI=0
          DO ISEG = 1,NSEG
            IF(BTEST(MUON_L1(IREGION),PHI_BIT(ISEG,IREGION)))THEN
              NPHI = NPHI+1
              MU_PHI_SEG(NPHI) = ISEG
            ENDIF  !find how many phi segments fired
          ENDDO   !loop over phi regions
          IF(NPHI.EQ.0) THEN   !something is wrong with the information
                               !that was passed
            CALL ERRMSG(
     &          'No phi region for l1 muon candidate',
     &          'L1ESUM_TRGR','l1 Muons not reported in ESUM','W')
            GOTO 999 !don't even try the other regions, something is
                     !is really messed up  
          ENDIF
          IF(NTRIG.LE.NPHI)THEN ! each trigger has a distinct phi region
            IF(NTRIG.LT.NPHI)FLAG_WORD=IBSET(FLAG_WORD,EXTRA_PHI)
            DO ITRIG = 1, NTRIG
              MU_ETA = MUON_ETA_REGION(IREGION)
              IF(IREGION.EQ.CENTRAL)THEN
                MU_PHI = OCT_MUON_PHI_REGION(MU_PHI_SEG(ITRIG))
              ELSE
                MU_PHI = QUAD_MUON_PHI_REGION(MU_PHI_SEG(ITRIG))
              ENDIF
              CALL ESUMFL('TRGR',ID_MUON,ETMUON,MU_ETA ,
     &              MU_ETA,  MU_PHI,   FLAG_WORD)
            ENDDO !add each trigger to L1ESUM
          ELSE IF(NTRIG.GT.NPHI) THEN !some of the triggers overlap in phi bin
!  Since ESUM is careful not to admit the same object twice, have to modify
!  the phi bin.
            IPHI = 0
            FUDGE = 0.0
            FLAG_WORD=IBSET(FLAG_WORD,OVERLAP_PHI)
            DO ITRIG = 1, NTRIG
              IF (IPHI.LT.NPHI) THEN
                IPHI = IPHI + 1
              ELSE IF(IPHI.EQ.NPHI) THEN
                FUDGE = FUDGE + 0.19   !ADD PI/16
              ENDIF
              MU_ETA = MUON_ETA_REGION(IREGION)
              IF(IREGION.EQ.CENTRAL)THEN
                MU_PHI =
     &              OCT_MUON_PHI_REGION(MU_PHI_SEG(IPHI)) + FUDGE
              ELSE
                MU_PHI =  QUAD_MUON_PHI_REGION(MU_PHI_SEG(IPHI))+FUDGE
              ENDIF
              CALL ESUMFL('TRGR',ID_MUON,ETMUON,MU_ETA ,
     &              MU_ETA,  MU_PHI,   FLAG_WORD)
            ENDDO !add each trigger to L1ESUM
          ENDIF
        ENDIF     ! muon trigger found in the region
      ENDDO       !loop over eta regions
C
  999 RETURN
      END
