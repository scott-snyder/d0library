      SUBROUTINE TRD_NUM_LAYERS(LCLUS,GEOMETRY,BADTRACK,TGEO)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : output number good layers crossed and acceptance
C-            information for trd
C-
C-   Inputs  : LCLUS          I    zebra link to PELC/PPHO bank             
C-   Outputs : GEOMETRY(3)    L    GEOMETRY(i) = .TRUE. if layer i crossed
C-             BADTRACK(10)   L    BADTRACK(i) = .TRUE. if failed cut i
C-              where i = 1 for bad runs
C-                        2 for bad sectors
C-                        3 for micro-DST version 1, 2 and TGEO = 1,2
C-                        4 for bad RECO (RECO < 12.21 has tight theta cut)
C-                                       (RECO 12.20 had bug for 1-layer trk)
C-                        5 for call to trd_check_integrity
C-                        6 if PPHO or PMUO for RECO < 12.20
C-                        7 if called for PMUO on micro-DST
C-                        8...10 spare
C-             TGEO           I    = 1, 2, or 3 good layers crossed by track)
C-   Controls:
C-
C-   Created  24-JAN-1996   Bob Kehoe
C-   Updated  12-FEB-1996   L.T. Goss modified to make independent of entry
C-                                    point in trd_electron_pion.for
C-   Updated  11-AUG-1996   L.T. Goss final mod's to make backwards compatible
C-                                    with udst vrs 1 & 2.
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:PI.DEF'
      INCLUDE 'D0$INC:TRD_PHI_Z.INC'
      INTEGER LCLUS,LZTRK,LCACL,LZFIT,TGEO,GZUDST,K,VERSION,PASS,I
      INTEGER UDST_VRS,LUDST,NPAR,LTRDT,NS,IETA,LOC,IER
      PARAMETER (NPAR = 10)
      REAL VIN(6),PHI,THETA,ST,RECO_VRS,THETA_PRIME,UDST_VALUE
      REAL REAL_TRD
      LOGICAL GEOMETRY(3),BADTRACK(NPAR),TRD_BADRUN,BAD_SECTOR
      LOGICAL BDSCTR,TRD_CHECK_INTEGRITY,MONTE_CARLO_DATA,FIRST
      LOGICAL DOCCVTXONLY
      CHARACTER*4 BANK
      DATA FIRST /.TRUE./
C----------------------------------------------------------------------
      IF (FIRST) THEN
        FIRST = .FALSE.
        CALL EZLOC('TRD_ANALYSIS_RCP',LOC)
        IF (LOC.LE.0) CALL INRCP('TRD_ANALYSIS_RCP',IER)
        CALL EZPICK ('TRD_ANALYSIS_RCP')
        CALL EZGET('DOCCVTXONLY',DOCCVTXONLY,IER)
      ENDIF
C
      DO I = 1,3                   ! initialization
        GEOMETRY(I) = .FALSE.
      ENDDO
      DO I = 1,NPAR
        BADTRACK(I) = .FALSE.
      ENDDO
      BAD_SECTOR  = .FALSE.
      TGEO        = 0
      THETA_PRIME = 0.
      UDST_VRS    = 0
      REAL_TRD    = 0
C
      CALL UHTOC(IQ(LCLUS-4),4,BANK,4)
C
      IF (BANK.EQ.'PMUO') THEN
        NS = IQ(LCLUS-2)
        LZTRK = LQ(LCLUS-5)
      ELSE
        LCACL = LQ(LCLUS-2)
        LZTRK = LQ(LCLUS-3)
        IETA = Q(LCLUS+19)
      ENDIF
C
      LUDST = GZUDST()
      IF (LUDST.GT.0) UDST_VRS = IQ(LUDST+1)
C
C        *** determine if hardware bad ***
      IF ((.NOT.MONTE_CARLO_DATA()).AND.
     &    (UDST_VRS.NE.1.AND.UDST_VRS.NE.2)) BADTRACK(1) = TRD_BADRUN()
C
      IF(BANK.EQ.'PPHO'.AND.LCACL.GT.0) THEN
        VIN(1) = Q(LCACL + 14)
        VIN(2) = Q(LCACL + 15)
        VIN(3) = Q(LCACL + 16)
        THETA  = Q(LCACL + 11)
        PHI    = Q(LCACL + 12)
        ST     = SIN(THETA)
        VIN(4) = ST*COS(PHI) !CX
        VIN(5) = ST*SIN(PHI) !cy
        VIN(6) = COS(THETA)  !cz
      ELSEIF (LZTRK.GT.0) THEN
        LZFIT = LQ(LZTRK - 1)
        IF (LZFIT.GT.0) THEN
          VIN(1) = Q(LZFIT + 11)
          VIN(2) = Q(LZFIT + 12)
          VIN(3) = Q(LZFIT + 15)
          PHI    = Q(LZFIT + 10)
          THETA  = Q(LZFIT + 13)
          ST     = SIN(THETA)
          IF(GZUDST().GT.0) THEN
            VIN(4) = ST*COS(PHI) !CX
            VIN(5) = ST*SIN(PHI) !cy
            VIN(6) = COS(THETA)  !cz
          ELSE
            VIN(4) = Q(LZFIT + 20) !CX
            VIN(5) = Q(LZFIT + 22) !cy
            VIN(6) = Q(LZFIT + 24) !cz
          ENDIF
        ENDIF
      ENDIF
C
      CALL TRD_INTERSECTION(VIN,THETA,PHI,GEOMETRY)
C
      IF (GEOMETRY(1)) TGEO = 1
      CALL TRD_BADSECTOR(PHI_TRD(1),BDSCTR)
      IF (BDSCTR) BAD_SECTOR = .TRUE.
C
      IF (GEOMETRY(1).AND.GEOMETRY(2)) TGEO = 2
      CALL TRD_BADSECTOR(PHI_TRD(2),BDSCTR)
      IF (BDSCTR) BAD_SECTOR = .TRUE.
C
      IF (GEOMETRY(1).AND.GEOMETRY(2).AND.GEOMETRY(3)) TGEO = 3
      CALL TRD_BADSECTOR(PHI_TRD(3),BDSCTR)
      IF (BDSCTR) BAD_SECTOR = .TRUE.
C
      IF ((UDST_VRS.EQ.1.OR.UDST_VRS.EQ.2)) THEN
        IF (BANK.EQ.'PELC') THEN
          REAL_TRD = UDST_VALUE(BANK,'TRDACC',IQ(LCLUS-5),IER)
        ELSEIF(BANK.EQ.'PPHO') THEN
          REAL_TRD = UDST_VALUE(BANK,'TRDACP',IQ(LCLUS-5),IER)
        ENDIF
        IF (REAL_TRD.NE.0) THEN
          TGEO = 3
          GEOMETRY(3) = .TRUE.
        ENDIF
        IF ((TGEO.EQ.1.OR.TGEO.EQ.2)) BADTRACK(3) = .TRUE.
      ENDIF
C
      IF ((.NOT.DOCCVTXONLY).AND.(TGEO.EQ.1.OR.TGEO.EQ.2).AND.
     &  (ABS(IETA).LE.12)) TGEO = 0
C
      IF ((.NOT.MONTE_CARLO_DATA()).AND.
     &    (UDST_VRS.NE.1.AND.UDST_VRS.NE.2)) BADTRACK(2) = BAD_SECTOR
C
      CALL RECO_VERSION(VERSION,PASS)
      RECO_VRS = FLOAT(VERSION) + 0.01*FLOAT(PASS)
C
C-        *** apply theta cut -- for reco v12.20 and earlier, reject tracks
C-        *** within 15-degrees of beampipe. for reco >= 12.21, cut is
C-        *** 8-degrees.  remove 1-layer tracks in v12.20 due to reco bug.
      IF (GEOMETRY(1)) THETA_PRIME = ABS((PI/2.0)-THETA_TRD(1))
C
      IF ((RECO_VRS.LE.12.2.AND.THETA_PRIME.GE.1.31).OR.
     &    (RECO_VRS.GT.12.2.AND.THETA_PRIME.GE.1.43).OR.
     &    (RECO_VRS.EQ.12.2.AND.TGEO.EQ.1)) THEN
        IF (UDST_VRS.NE.1.AND.UDST_VRS.NE.2) BADTRACK(4) = .TRUE.
      ENDIF
C        *** check integrity on DST and STA's ***
      IF (BANK.EQ.'PELC'.AND.LZTRK.GT.0.AND.UDST_VRS.EQ.0) THEN
        LTRDT = LQ(LZTRK - 9)
        IF (LTRDT.GT.0) THEN
          IF (.NOT.TRD_CHECK_INTEGRITY(LZTRK)) BADTRACK(5) = .TRUE.
        ENDIF
      ENDIF
C
      IF ((BANK.EQ.'PPHO'.OR.BANK.EQ.'PMUO').AND.RECO_VRS.LT.12.2)
     &  BADTRACK(6) = .TRUE.
C
      IF (BANK.EQ.'PMUO'.AND.UDST_VRS.GT.0) BADTRACK(7) = .TRUE.
C
C        *** if anything wrong, don't bother ***
      DO K = 1,NPAR
        IF (BADTRACK(K)) TGEO = 0
      ENDDO
C
  999 RETURN
      END
