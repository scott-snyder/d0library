      SUBROUTINE TOP_LEPTONS_CLEANEM_INIT_SUMMRY(LUN)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : CLEANEM Initialization Summary
C-
C-   Inputs  : 
C-              LUN - i/o unit no for printout
C-   Outputs : 
C-   Controls: 
C-
C-   Created   2-FEB-1993   Stephen J. Wimpenny
C-   Modified 17-Mar-1993   Routine name change for library compatibility
C-   Updated  23-MAR-2004   sss - compile with g77.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      LOGICAL LVL2_MATCH
C
      INTEGER ICLUS,IV,I,N,LUN,IER
      INTEGER ELECTRON_MASK(32),NOEL_BITS,PHOTON_MASK(32),NOPH_BITS
      INTEGER MIN_CELLS,MAX_ZTRAKS
C
      REAL COARSE_HMATRIX(37),FINE_HMATRIX(37),EM_THRESHOLD,CORE_EFRAC
      REAL DISPERSION,SIG5_SIG3,DR_ISOL_CONES(2),ISOL_CONE_ECUT(4)
      REAL ISOl_CONE_ETCUT(4),CC_CRACK,L1_ET_THRESH,L2_ET_THRESH
      REAL CC_DIST_CUT(2),CC_OFFSET_DIST(2),CC_ERR_DIST(2)
      REAL EC_DIST_CUT(4),EC_OFFSET_DIST(4),EC_ERR_DIST(4)
      REAL SIG_TRK_MATCH,TRK_CONE_SIZE
      REAL ISOL_CONE_TOT_ETCUT(4),ISOL_CONE_TOT_ECUT(4)
      REAL CDC_MIP_CUT(2),FDC_MIP_CUT(2),VTX_MIP_CUT(2)
      REAL TRD_LIKELIHOOD,TRD_TRUNCATED_MEAN
      REAL IMPACT_XY(2),IMPACT_Z,EM3_WEIGHT
C
C *** Read Electron and Photon Cut Masks from RCP file TOP_LEPTONS
C
      CALL EZPICK('TOP_LEPTONS_RCP')
C
      CALL EZGETA('ELECTRON_MASK',0,0,0,NOEL_BITS,IER)
      IF(NOEL_BITS.LT.32) THEN
        WRITE(LUN,1000) NOEL_BITS
        IER=-1
      ELSE
        IF(IER.EQ.0) 
     1    CALL EZGETA('ELECTRON_MASK',1,NOEL_BITS,1,ELECTRON_MASK,IER)
      ENDIF
C
      CALL EZGETA('PHOTON_MASK',0,0,0,NOPH_BITS,IER)
      IF(NOPH_BITS.LT.32) THEN
        WRITE(LUN,1010) NOPH_BITS
        IER=-1
      ELSE
        IF(IER.EQ.0)
     1     CALL EZGETA('PHOTON_MASK',1,NOPH_BITS,1,PHOTON_MASK,IER)
      ENDIF
C
      CALL EZRSET
      IF(IER.NE.0) CALL ERRMSG('Error in TOP_LEPTONS_RCP',
     &    'CLEANEM_INIT_SUMMRY',' ','F')
C
C *** Next read cut values from CLEANEM RCP
C
      CALL INRCP('CLEANEM_RCP',IER)
      IF(IER.NE.0)
     1  CALL ERRMSG('CLEANEM_RCP not found',
     2    'CLEANEM_INIT_SUMMRY',' ','W')
      CALL EZPICK('CLEANEM_RCP')
C
      CALL EZGETA('COARSE_CHISQUARED',0,0,0,N,IER)
      IF(IER.EQ.0) 
     1  CALL EZGETA('COARSE_CHISQUARED',1,N,1,COARSE_HMATRIX,IER)
      IF(IER.EQ.0) CALL EZGETA('FINE_CHISQUARED',0,0,0,N,IER)
      IF(IER.EQ.0) CALL EZGETA('FINE_CHISQUARED',1,N,1,FINE_HMATRIX,IER)
C
      IF(IER.EQ.0) CALL EZGET('CLUSTER_EM_RATIO_THRESHOLD',EM_THRESHOLD,
     &  IER)
      IF(IER.EQ.0) CALL EZGET('CORE_ENERGY_FRACTION_CUT',CORE_EFRAC,IER)
      IF(IER.EQ.0) CALL EZGET('DISPERSION_CUT',DISPERSION,IER)
      IF(IER.EQ.0) CALL EZGET('DSIGMA_CUT',SIG5_SIG3,IER)
      IF(IER.EQ.0) CALL EZGET('ISOLATION_CONES',DR_ISOL_CONES,IER)
      IF(IER.EQ.0) CALL EZGET('ISOLATION_CONE_ENERGY_CUT',
     &  ISOL_CONE_ECUT,IER)
      IF(IER.EQ.0) CALL EZGET('ISOLATION_CONE_ET_CUT',ISOL_CONE_ETCUT,
     &  IER)
      IF(IER.EQ.0) CALL EZGET('ISOLATION_CONE_TOT_ENERGY_CUT',
     &  ISOL_CONE_TOT_ECUT,IER)
      IF(IER.EQ.0) CALL EZGET('ISOLATION_CONE_TOT_ET_CUT',
     1  ISOL_CONE_TOT_ETCUT,IER)
      IF(IER.EQ.0) CALL EZGET('CC_CRACK_WIDTH',CC_CRACK,IER)
      IF(IER.EQ.0) CALL EZGET('MINIMUM_NUMBER_OF_CELLS',MIN_CELLS,IER)
      IF(IER.EQ.0) CALL EZGET('MATCH_LEVEL2_ELECTRON',LVL2_MATCH,IER)
      IF(IER.EQ.0) CALL EZGET('L1ET_THRESHOLD',L1_ET_THRESH,IER)
      IF(IER.EQ.0) CALL EZGET('L2ET_THRESHOLD',L2_ET_THRESH,IER)
      IF(IER.EQ.0) CALL EZGET('DISTANCE_CC_CUT',CC_DIST_CUT,IER)
      IF(IER.EQ.0) CALL EZGET('OFFSET_DISTANCE_CC',CC_OFFSET_DIST,IER)
      IF(IER.EQ.0) CALL EZGET('ERR_DISTANCE_CC',CC_ERR_DIST,IER)
      IF(IER.EQ.0) CALL EZGET('DISTANCE_EC_CUT',EC_DIST_CUT,IER)
      IF(IER.EQ.0) CALL EZGET('OFFSET_DISTANCE_EC',EC_OFFSET_DIST,IER)
      IF(IER.EQ.0) CALL EZGET('ERR_DISTANCE_EC',EC_ERR_DIST,IER)
      IF(IER.EQ.0) CALL EZGET('SIG_TRKMATCH_CUT',SIG_TRK_MATCH,IER)
      IF(IER.EQ.0) CALL EZGET('TRACK_CONE_SIZE',TRK_CONE_SIZE,IER)
      IF(IER.EQ.0) CALL EZGET('NZTRAKS_CUT',MAX_ZTRAKS,IER)
      IF(IER.EQ.0) CALL EZGET('CDC_MIP_CUT',CDC_MIP_CUT,IER)
      IF(IER.EQ.0) CALL EZGET('FDC_MIP_CUT',FDC_MIP_CUT,IER)
      IF(IER.EQ.0) CALL EZGET('VTX_MIP_CUT',VTX_MIP_CUT,IER)
      IF(IER.EQ.0) CALL EZGET('TRD_LIKELIHOOD_CUT',TRD_LIKELIHOOD,IER)
      IF(IER.EQ.0) CALL EZGET('TRD_TRUNCATED_MEAN_CUT',
     &  TRD_TRUNCATED_MEAN,IER)
      IF(IER.EQ.0) CALL EZGET('IMPACT_PARAMETER_XY',IMPACT_XY,IER)
      IF(IER.EQ.0) CALL EZGET('IMPACT_PARAMETER_Z',IMPACT_Z,IER)
      IF(IER.EQ.0) CALL EZGET('WEIGHT_CUT',EM3_WEIGHT,IER)
C
      CALL EZRSET
      IF (IER.NE.0) CALL ERRMSG('Error in CLEANEM_RCP',
     &    'CLEANEM_INIT_SUMMRY',' ','F')
C
C *** Loop over electron bits and print for chosen bits
C
      DO ICLUS=1,2
        IF(ICLUS.EQ.1) THEN
          WRITE(LUN,2000)
        ELSE
          WRITE(LUN,3000)
        ENDIF
        DO IV=1,32
          IF(ICLUS.EQ.1) THEN
            IF(ELECTRON_MASK(IV).GT.0) THEN
              GO TO (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,
     &          20,21,22,23,24,25,26,27,28,29,30,31,32),IV
            ELSE
              GO TO 500
            ENDIF
          ELSE
            IF(PHOTON_MASK(IV).GT.0) THEN
              GO TO (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,
     &          20,21,22,23,24,25,26,27,28,29,30,31,32),IV
            ELSE
              GO TO 500
            ENDIF
          ENDIF
   1      CONTINUE
C
C *** Bit 0
C
          WRITE(LUN,2010) (COARSE_HMATRIX(I),I=1,12),
     1      (COARSE_HMATRIX(I),I=13,25),(COARSE_HMATRIX(I),I=26,34),
     1      (COARSE_HMATRIX(I),I=35,37)
          GO TO 500
   2      CONTINUE
C
C *** Bit 1
C
          WRITE(LUN,2012) (FINE_HMATRIX(I),I=1,12),
     1      (FINE_HMATRIX(I),I=13,25),(FINE_HMATRIX(I),I=26,34),
     1      (FINE_HMATRIX(I),I=35,37)
          GO TO 500
   3      CONTINUE
C
C *** Bit 2
C
          WRITE(LUN,2014)
          GO TO 500
   4      CONTINUE
C
C *** Bit 3
C
          WRITE(LUN,2016) EM_THRESHOLD
          GO TO 500
   5      CONTINUE
C
C *** Bit 4
C
          WRITE(LUN,2018) CORE_EFRAC
          GO TO 500
   6      CONTINUE
C
C *** Bit 5
C
          WRITE(LUN,2020) DISPERSION
          GO TO 500
   7      CONTINUE
C
C *** Bit 6
C
          WRITE(LUN,2022) SIG5_SIG3
          GO TO 500
   8      CONTINUE
C
C *** Bit 7
C
          WRITE(LUN,2024) ISOL_CONE_ECUT(1),ISOL_CONE_ECUT(3)
          GO TO 500
   9      CONTINUE
C
C *** Bit 8
C
          WRITE(LUN,2026) ISOL_CONE_ECUT(2),ISOL_CONE_ECUT(4)
          GO TO 500
  10      CONTINUE
C
C *** Bit 9
C
          WRITE(LUN,2028) ISOL_CONE_ETCUT(1),ISOL_CONE_ETCUT(3)
          GO TO 500
  11      CONTINUE
C
C *** Bit 10
C
          WRITE(LUN,2030) ISOL_CONE_ETCUT(2),ISOL_CONE_ETCUT(4)
          GO TO 500
  12      CONTINUE
C
C *** Bit 11
C
          WRITE(LUN,2032)  
          GO TO 500
  13      CONTINUE
C
C *** Bit 12
C
          WRITE(LUN,2034) MIN_CELLS
          GO TO 500
  14      CONTINUE
C
C *** Bit 13
C
          WRITE(LUN,2036) 
          GO TO 500
  15      CONTINUE
C
C *** Bit 14 - Isolation cut Et (large cone)
C
          WRITE(LUN,2038) DR_ISOL_CONES(2),ISOL_CONE_TOT_ETCUT(2),
     1      ISOL_CONE_TOT_ETCUT(4)
          GO TO 500
  16      CONTINUE
C
C *** Bit 15 - not used
C
          GO TO 500
  17      CONTINUE
C
C *** Bit 16
C
          WRITE(LUN,2042) 
          GO TO 500
  18      CONTINUE
C
C *** Bit 17
C
          WRITE(LUN,2044)
          GO TO 500
  19      CONTINUE
C
C *** Bit 18
C
          WRITE(LUN,2046) SIG_TRK_MATCH
          GO TO 500
  20      CONTINUE
C
C *** Bit 19
C
          GO TO 500
  21      CONTINUE
C
C *** Bit 20
C
          WRITE(LUN,2050) MAX_ZTRAKS
          GO TO 500
  22      CONTINUE
C
C ** Bit 21 - not used
C
          GO TO 500
  23      CONTINUE
C
C *** Bit 22
C
          WRITE(LUN,2054) CDC_MIP_CUT(1),CDC_MIP_CUT(2)
          GO TO 500
  24      CONTINUE
C
C *** Bit 23
C
          WRITE(LUN,2056) FDC_MIP_CUT(1),FDC_MIP_CUT(2)
          GO TO 500
  25      CONTINUE
C
C *** Bit 24
C
          WRITE(LUN,2058) VTX_MIP_CUT(1),VTX_MIP_CUT(2)
          GO TO 500
  26      CONTINUE
C
C *** Bit 25
C
          WRITE(LUN,2060) TRD_LIKELIHOOD
          GO TO 500
  27      CONTINUE
C
C *** Bit 26
C
          WRITE(LUN,2062) TRD_TRUNCATED_MEAN
          GO TO 500
  28      CONTINUE
C
C *** Bit 27 - not used
C
          GO TO 500
  29      CONTINUE
C
C *** Bit 28
C
          WRITE(LUN,2066) IMPACT_XY(1),IMPACT_XY(2)
          GO TO 500
  30      CONTINUE
C
C *** Bit 29
C
          WRITE(LUN,2068) IMPACT_Z
          GO TO 500
  31     CONTINUE
C
C *** Bit 30 - not used
C
          GO TO 500
  32      CONTINUE
C
C *** Bit 31 - noy used
C
  500     CONTINUE
        ENDDO
      ENDDO
C
C----------------------------------------------------------------------
  999 RETURN
 1000 FORMAT(5X,' TOP_LEPTONS_CLEANEM_INIT_SUMMRY : RCP Error in',
     1  ' Electron Bitmask',/15X,' expected 32 bits , found ',
     2  I3,' bits') 
 1010 FORMAT(5X,' TOP_LEPTONS_CLEANEM_INIT_SUMMRY : RCP Error in',
     1  ' Photon Bitmask ',/15X,' expected 32 bits , found ',
     2  I3,' bits') 
 2000 FORMAT(//5X,' Electron id : ',/)
 2010 FORMAT(15X,' Max Hmatrix Chisquare cuts : ',/,
     1 20X,'   0 < eta < 1.2 = ',5F8.1,/,
     2 20X,'                   ',5F8.1,/,
     3 20X,'                   ',2F8.1,/,
     4 20X,' 1.3 < eta < 2.4 = ',5f8.1,/,
     5 20X,'                   ',5F8.1,/,
     6 20X,'                   ',3F8.1,/,
     7 20X,' 2.5 < eta < 3.4 = ',5F8.1,/,
     8 20X,'                   ',4F8.1,/,
     9 20X,' 3.4 < eta < 3.7 = ',3F8.1)
 2012 FORMAT(15X,' Max Hmatrix Chisquare cuts : ',/,
     1 20X,'   0 < eta < 1.2 = ',5F8.1,/,
     2 20X,'                   ',5F8.1,/,
     3 20X,'                   ',2F8.1,/,
     4 20X,' 1.3 < eta < 2.4 = ',5f8.1,/,
     5 20X,'                   ',5F8.1,/,
     6 20X,'                   ',3F8.1,/,
     7 20X,' 2.5 < eta < 3.4 = ',5F8.1,/,
     8 20X,'                   ',4F8.1,/,
     9 20X,' 3.4 < eta < 3.7 = ',3F8.1)
 2014 FORMAT(15X,' Cut on CC em flag ')
 2016 FORMAT(15X,' Minimum em fraction           = ',F4.2)
 2018 FORMAT(15X,' Minimum core energy fraction  = ',F4.2)
 2020 FORMAT(15X,' Maximum transverse dispersion = ',F4.2)
 2022 FORMAT(15X,' Sigma5-Sigma3 cut = ',F4.2)
 2024 FORMAT(15X,' Energy Isolation cut   = ',2F6.2,' (CC,EC)')
 2026 FORMAT(15X,' Energy Isolation cut   = ',2F6.2,' (CC,EC)')
 2028 FORMAT(15X,' Et Isolation cut       = ',2F6.2,' (CC,EC)')
 2030 FORMAT(15X,' Et Isolation cut       = ',2F6.2,' (CC,EC)')
 2032 FORMAT(15X,' Cut on close to crack flag ')
 2034 FORMAT(15X,' Minimum no of cells fired = ',I3)
 2036 FORMAT(15X,' Require Level 2 electron match ')
 2038 FORMAT(15X,' Max Total Et in Cone of ',F4.2,' = ',2F6.2,
     1 ' (CC,EC)')
 2042 FORMAT(15X,' Apply distance 1 cut (Rdphi for CC and EC) ')
 2044 FORMAT(15X,' Apply distance 2 cut (dz for CC and dR for EC) ')
 2046 FORMAT(15X,' Min shower centriod/track match significance = ',
     1 F8.2)
 2050 FORMAT(15X,' Max no of Ztraks within dR cone = ',I3)
 2054 FORMAT(15X,' Limits for CDC 1 mip cut = ',2F6.2)
 2056 FORMAT(15X,' Limits for FDC 1 mip cut = ',2F6.2)
 2058 FORMAT(15X,' Limits for VTX 1 mip cut = ',2F6.2)
 2060 FORMAT(15X,' TRD Likelihood cut     = ',F6.2)
 2062 FORMAT(15X,' TRD Truncated mean cut = ',F6.2)
 2066 FORMAT(15X,' Transverse impact parameter cuts = ',2F6.2)
 2068 FORMAT(15X,' Z impact parameter cut = ',F6.2)
 3000 FORMAT(//,5X,' Photon id : ',/) 
      END
