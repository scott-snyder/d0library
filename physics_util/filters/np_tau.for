      LOGICAL FUNCTION NP_TAU()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Get clean tau data sample for further analysis
C-
C-
C-   Returned value: .TRUE. to keep the event, .FALSE. on error.
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   ENTRY  NP_TAU_EOJ  performs end-of-job summary for this filter.
C-   Created for taus 16-FEB-1993   Hailin Li
C----------------------------------------------------------------------
      IMPLICIT  NONE
      INCLUDE   'D0$INC:ZEBCOM.INC'
      INCLUDE   'D0$INC:PI.DEF'
      LOGICAL   NP_TAU_EOJ
C----------------------------------------------------------------------
      INTEGER   LJETS, GZJETS, LPNUT, GZPNUT, NZBANK
      INTEGER   ICHOICE,IER, NJETS,LJET2,NSUM,NSUM_MET
      INTEGER   NSUM_EMFRAC, NSUM_TOP1JET,NSUM_TOPGE2JETS
      EXTERNAL  GZJETS, GZPNUT, NZBANK
      REAL      TEMPLATE(5,4),PHIMET,PHIJ1,PHIJ2, CALLS,AVRG
      REAL      DPHIJ12,DPHIJ1MET,DPHIJ12_CUT, DPHIJ1MET_CUT
      REAL      EM_FRACTION_CUT,EM_FRACTION,MET_CUT,MET
      REAL      RSUMMARY(20)
      LOGICAL   FIRST
      SAVE      DPHIJ12_CUT, DPHIJ1MET_CUT, FIRST, NSUM, CALLS
      DATA      FIRST/.TRUE./
      DATA      ICHOICE/1/
      DATA      TEMPLATE/
     &  1.,6.,0.7,0.,0.,      ! CONE R=0.7
     &  1.,6.,0.5,0.,0.,      ! CONE R=0.5
     &  1.,6.,0.3,0.,0.,      ! CONE R=0.3
     &  2.,7.,2.,8.,2./       ! NN 2x2
C----------------------------------------------------------------------
      IF ( FIRST ) THEN
        NSUM=0
        NSUM_MET=0
        NSUM_EMFRAC=0
        NSUM_TOP1JET=0
        NSUM_TOPGE2JETS=0
        CALLS=0.0
        FIRST = .FALSE.
        CALL INRCP ('NP_TAU_RCP', IER)
C
        IF ( IER .NE. 0 ) THEN
          CALL ERRMSG ( 'RCP error',
     &      'NP_TAU', 'Could not find NP_TAU_RCP', 'F')
        ENDIF                           ! if ier .eq. 0
C
        CALL EZPICK('NP_TAU_RCP')
C
        CALL EZGET_i('JETS_ALGORITHM',ICHOICE,IER)
        IF ( IER .NE. 0 ) THEN
          CALL ERRMSG ( 'RCP read error',
     &      'NP_TAU', 'Could not find parameter JETS_ALGORITHM', 'F')
        ENDIF                           ! if ier .eq. 0
C
        CALL EZGET('DPHIJ12_CUT', DPHIJ12_CUT, IER)
        IF ( IER .NE. 0 ) THEN
          CALL ERRMSG ( 'RCP read error',
     &      'NP_TAU', 'Could not find parameter DPHIJ12_CUT', 'F')
        ENDIF                           ! if ier .eq. 0
C
        CALL EZGET('DPHIJ1MET_CUT', DPHIJ1MET_CUT,IER)
        IF ( IER .NE. 0 ) THEN
          CALL ERRMSG ( 'RCP read error',
     &      'NP_TAU', 'Could not find parameter DPHIJ1MET_CUT', 'F')
        ENDIF                           ! if ier .eq. 0
C
        CALL EZGET('EM_FRACTION_CUT', EM_FRACTION_CUT,IER)
        IF ( IER .NE. 0 ) THEN
          CALL ERRMSG ( 'RCP read error',
     &      'NP_TAU', 'Could not find parameter EM_FRACTION_CUT', 'F')
        ENDIF                           ! if ier .eq. 0
C
        CALL EZGET('MET_CUT', MET_CUT,IER)
        IF ( IER .NE. 0 ) THEN
          CALL ERRMSG ( 'RCP read error',
     &      'NP_TAU', 'Could not find parameter MET_CUT', 'F')
        ENDIF                           ! if ier .eq. 0
C
        CALL EZRSET
C
      ENDIF                             ! if first
C----------------------------------------------------------------------
      NP_TAU = .FALSE.               ! reject by default
      CALLS=CALLS+1.0
C
      LPNUT = GZPNUT(2)                       ! use ICD corrected PNUT
      IF ( LPNUT .LE. 0 ) RETURN              ! no PNUT(2) --- reject
      MET=Q(LPNUT+7)
      PHIMET = Q(LPNUT+10)
C
C       Missing Et cut
      IF(MET.LT.MET_CUT) RETURN
      NSUM_MET=NSUM_MET+1
C
C       jets
C
      IF(ICHOICE.EQ.1) CALL SET_CAPH('CONE_JET',TEMPLATE(1,1),IER)
      IF(ICHOICE.EQ.2) CALL SET_CAPH('CONE_JET',TEMPLATE(1,2),IER)
      IF(ICHOICE.EQ.3) CALL SET_CAPH('CONE_JET',TEMPLATE(1,3),IER)
      IF(ICHOICE.EQ.4) CALL SET_CAPH('NN_JET',TEMPLATE(1,4),IER)
C
C     Get the total number of JETS for all possible jets
      LJETS=GZJETS()
      NJETS=NZBANK(IXCOM,LJETS)
C
C
      LJETS=GZJETS()
      IF(LJETS.NE.0) THEN
C          sort banks so they are in increasing order of Et
C          NOTE: after each reordering of banks the pointer
C                LJETS must be refetched
        CALL ZSORT(IXCOM,LJETS,6)
        LJETS=GZJETS()
        CALL ZTOPSY(IXCOM,LJETS)
        LJETS=GZJETS()
C
C     EM fraction cut
        EM_FRACTION=Q(LJETS+14)
        IF (EM_FRACTION.LT.EM_FRACTION_CUT) THEN
          CALL RESET_CAPH
          RETURN
        ENDIF
        NSUM_EMFRAC=NSUM_EMFRAC+1
C
C      Make topological cuts according to number of JETS
        PHIJ1=Q(LJETS+8)
        IF(NJETS.EQ.1) THEN
          DPHIJ1MET=ABS(PHIJ1-PHIMET)*180.0/PI
          IF(DPHIJ1MET.GT.180.0) DPHIJ1MET=360.0-DPHIJ1MET
          IF(DPHIJ1MET.LT.DPHIJ1MET_CUT) THEN
            CALL RESET_CAPH
            RETURN
          ENDIF
          NSUM_TOP1JET=NSUM_TOP1JET+1
        ELSE
          LJET2=LQ(LJETS)
          PHIJ2=Q(LJET2+8)
          DPHIJ12=ABS(PHIJ1-PHIJ2)*180.0/PI
          IF(DPHIJ12.GT.180.0)DPHIJ12=360.0-DPHIJ12
          IF(DPHIJ12.GT.DPHIJ12_CUT) THEN
            CALL RESET_CAPH
            RETURN
          ENDIF
          NSUM_TOPGE2JETS=NSUM_TOPGE2JETS+1
        ENDIF
      ENDIF
C
      NSUM=NSUM+1
      NP_TAU = .TRUE.
C
      CALL RESET_CAPH
      RETURN
C#######################################################################
      ENTRY NP_TAU_EOJ (RSUMMARY)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : End of job summary for this filter.  Dummy for now.
C-
C-   Inputs  : none
C-   Outputs : RSUMMARY   [R(20)] array of 20 reals
C-   Controls: none
C-
C-   Created  18-DEC-1992   Marc Paterno
C-   Modified 15-FEB-1993   Hailin Li
C----------------------------------------------------------------------
      NP_TAU_EOJ = .TRUE.
      RSUMMARY(1)=NSUM_MET        ! No. of evts passing Missing Et cut
      RSUMMARY(2)=NSUM_EMFRAC     ! No. of evts passing EM fraction cut
      RSUMMARY(3)=NSUM_TOP1JET    ! No. of evts passing topological cut(1 jet)
      RSUMMARY(4)=NSUM_TOPGE2JETS ! No. of evts passing
                                  ! topological cut (>=2jets)
      RSUMMARY(5)=NSUM            ! No. of evts passed
      RSUMMARY(6)=CALLS           ! No. of total evts during this run
      RETURN
      END
