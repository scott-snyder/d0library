      LOGICAL FUNCTION NP_CLEAN_TAU()
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
C-   ENTRY  NP_CLEAN_TAU_EOJ  performs end-of-job summary for this filter.
C-   Created for taus 16-FEB-1993   Hailin Li
C-   Updated   4-MAY-1994   Hailin LI   Added JET Et threshold parameter
C-   Updated   12-MAY-1994  Hailin LI   Added trigger requirements
C-   Updated   14-MAY-1994  Hailin LI   originally created for RGE
C-                                      adapted for STA streaming
C----------------------------------------------------------------------
      IMPLICIT  NONE
      INCLUDE   'D0$INC:ZEBCOM.INC'
      INCLUDE   'D0$INC:PI.DEF'
      LOGICAL   NP_CLEAN_TAU_EOJ
C----------------------------------------------------------------------
      CHARACTER*8 ALGORITHM(4)
      CHARACTER*80 MSG
      CHARACTER*32 L2NAME(5)
      INTEGER   LJETS, GZJETS, LPNUT, GZPNUT,I
      INTEGER   ICHOICE,IER, NJETS,LJET2,NSUM,NSUM_MET,IERR,NSUM_FILT
      INTEGER   NSUM_EMFRAC, NSUM_TOP1JET,NSUM_TOPGE2JETS,NFILT
      REAL      TEMPLATE(5,4),PHIMET,PHIJ1,PHIJ2, CALLS
      REAL      DPHIJ12,DPHIJ1MET,DPHIJ12_MAX, DPHIJ1MET_MIN
      REAL      EMFRAC_MIN,EMFRAC,MET_MIN,MET,ET_MIN
      REAL      RSUMMARY(20),ET
      LOGICAL   FIRST,EZERR,L2NAME_PASSED,OK
      SAVE      DPHIJ12_MAX, DPHIJ1MET_MIN,ET_MIN
      SAVE      MET_MIN,EMFRAC_MIN,L2NAME,ICHOICE,NFILT
      SAVE      FIRST, NSUM, CALLS
      DATA      FIRST/.TRUE./
      DATA      ICHOICE/1/
      DATA      TEMPLATE/
     &  1.,6.,0.7,0.,0.,      ! CONE R=0.7
     &  1.,6.,0.5,0.,0.,      ! CONE R=0.5
     &  1.,6.,0.3,0.,0.,      ! CONE R=0.3
     &  2.,7.,2.,8.,2./       ! NN 2x2
      DATA ALGORITHM/'CONE_JET','CONE_JET','CONE_JET','NN_JET'/
C----------------------------------------------------------------------
      IF ( FIRST ) THEN
        NSUM=0
        NSUM_FILT = 0
        NSUM_MET=0
        NSUM_EMFRAC=0
        NSUM_TOP1JET=0
        NSUM_TOPGE2JETS=0
        CALLS=0.0
        FIRST = .FALSE.
        CALL INRCP ('NP_CLEAN_TAU_RCP', IER)
C
        IF ( IER .NE. 0 ) THEN
          CALL ERRMSG ( 'RCP error',
     &      'NP_CLEAN_TAU', 'Could not find NP_CLEAN_TAU_RCP', 'F')
        ENDIF                           ! if ier .eq. 0
C
        CALL EZPICK('NP_CLEAN_TAU_RCP')
        IF ( EZERR(IER) ) THEN
          CALL ERRMSG ( 'RCP error',
     &      'NP_CLEAN_TAU', 'Could not find NP_CLEAN_TAU_RCP bank', 'F')
        ELSE
          IERR = 0
          CALL EZGET('JETS_ALGORITHM',ICHOICE,IER)
          IERR = IERR + ABS(IER)
          CALL EZGET('DPHIJ12_MAX', DPHIJ12_MAX, IER)
          IERR = IERR + ABS(IER)
          CALL EZGET('DPHIJ1MET_MIN', DPHIJ1MET_MIN,IER)
          IERR = IERR + ABS(IER)
          CALL EZGET('EMFRAC_MIN', EMFRAC_MIN,IER)
          IERR = IERR + ABS(IER)
          CALL EZGET('MET_MIN', MET_MIN,IER)
          IERR = IERR + ABS(IER)
          CALL EZGET('ET_MIN', ET_MIN,IER)
          IERR = IERR + ABS(IER)
          CALL EZ_GET_CHARS('FILTERS', NFILT, L2NAME, IER)
          IERR = IERR + ABS(IER)
          IF  ( IERR .NE. 0 ) CALL ERRMSG ( 'RCP param not found',
     &      'NP_CLEAN_TAU', ' ', 'F')
          IF (NFILT .LE. 0) THEN
            CALL ERRMSG('No filters specifiled','NP_CLEAN_TAU',' ','W')
          ELSE IF (NFILT .GT. 5) THEN
            CALL ERRMSG('Too many filters','NP_CLEAN_TAU',
     &                  'maximum 5 only','F')
          ENDIF
        ENDIF
c
        CALL EZRSET
C
      ENDIF                             ! if first
C----------------------------------------------------------------------
      NP_CLEAN_TAU = .FALSE.               ! reject by default
      CALLS=CALLS+1.0
C
C   triggers
      IF ( NFILT .GT. 0 ) THEN
        OK = .FALSE.
        DO I = 1, NFILT
          OK = OK .OR. L2NAME_PASSED(L2NAME(I))
        ENDDO
        IF ( .NOT. OK ) THEN
          CALL ERRMSG('Failed triggers','NP_CLEAN_TAU',' ','W')
          RETURN
        ENDIF
        NSUM_FILT = NSUM_FILT + 1
      ENDIF
C
C   missing Et requirement
      LPNUT = GZPNUT(2)                       ! use ICD corrected PNUT
      IF ( LPNUT .LE. 0 ) THEN
        CALL ERRMSG('No PNUT(2)','NP_CLEAN_TAU',' ','W')
        RETURN              ! no PNUT(2) --- reject
      ENDIF
      MET=Q(LPNUT+7)
      PHIMET = Q(LPNUT+10)
C
C       Missing Et cut
      IF(MET.LT.MET_MIN) THEN
        CALL ERRMSG('Failed missing Et cut','NP_CLEAN_TAU',' ','W')
        RETURN
      ENDIF
      NSUM_MET=NSUM_MET+1

C
C       jets
      CALL SET_CAPH(ALGORITHM(ICHOICE),TEMPLATE(1,ICHOICE),IER)
      IF ( IER .NE. 0 ) THEN
        WRITE(MSG,9)ALGORITHM(ICHOICE)
        CALL ERRMSG(MSG,'NP_CLEAN_TAU',' ','W')
        CALL RESET_CAPH
        RETURN
      ENDIF
C
      LJETS=GZJETS()
      IF ( LJETS .LE. 0 ) THEN
        WRITE(MSG,99)ALGORITHM(ICHOICE),TEMPLATE(3,ICHOICE)
        CALL ERRMSG(MSG,'NP_CLEAN_TAU',' ','W')
        CALL RESET_CAPH
        RETURN
      ENDIF
C
      CALL ZSORT(IXCOM,LJETS,6)
      LJETS=GZJETS()
      CALL ZTOPSY(IXCOM,LJETS)
      LJETS=GZJETS()

      NJETS = 0
      DO WHILE ( LJETS .GT. 0 )
        ET = Q(LJETS+6)
        IF ( ET .GT. ET_MIN ) NJETS = NJETS + 1
        LJETS = LQ(LJETS)
      ENDDO

      IF ( NJETS .LT. 1 ) THEN
        WRITE(MSG,999)ET_MIN
        CALL ERRMSG(MSG,'NP_CLEAN_TAU',' ','W')
        CALL RESET_CAPH
        RETURN
      ENDIF

      LJETS=GZJETS()
C
C     EM fraction cut
      EMFRAC=Q(LJETS+14)
      IF (EMFRAC.LT.EMFRAC_MIN) THEN
        CALL ERRMSG('failed EMF cut','NP_CLEAN_TAU',' ','W')
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
        IF(DPHIJ1MET.LT.DPHIJ1MET_MIN) THEN
          CALL ERRMSG('failed DPHIJ1MET cut','NP_CLEAN_TAU',' ','W')
          CALL RESET_CAPH
          RETURN
        ENDIF
        NSUM_TOP1JET=NSUM_TOP1JET+1
      ELSE
        LJET2=LQ(LJETS)
        PHIJ2=Q(LJET2+8)
        DPHIJ12=ABS(PHIJ1-PHIJ2)*180.0/PI
        IF(DPHIJ12.GT.180.0)DPHIJ12=360.0-DPHIJ12
        IF(DPHIJ12.GT.DPHIJ12_MAX) THEN
          CALL ERRMSG('failed DPHIJ12 cut','NP_CLEAN_TAU',' ','W')
          CALL RESET_CAPH
          RETURN
        ENDIF
        NSUM_TOPGE2JETS=NSUM_TOPGE2JETS+1
      ENDIF
C
C
      CALL RESET_CAPH
      NSUM=NSUM+1
      NP_CLEAN_TAU = .TRUE.

    9 FORMAT('Jet path ',A8,' not found')
   99 FORMAT('Jet ',A8,1X,F3.1,' not found')
  999 FORMAT('No jet with',F7.1)
      RETURN
C#######################################################################
      ENTRY NP_CLEAN_TAU_EOJ (RSUMMARY)
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
      NP_CLEAN_TAU_EOJ = .TRUE.
      RSUMMARY(1)=NSUM_FILT        ! No. of evts passing filter requirements
      RSUMMARY(2)=NSUM_MET        ! No. of evts passing Missing Et cut
      RSUMMARY(3)=NSUM_EMFRAC     ! No. of evts passing EM fraction cut
      RSUMMARY(4)=NSUM_TOP1JET    ! No. of evts passing topological cut(1 jet)
      RSUMMARY(5)=NSUM_TOPGE2JETS ! No. of evts passing
                                  ! topological cut (>=2jets)
      RSUMMARY(6)=NSUM            ! No. of evts passed
      RSUMMARY(7)=CALLS           ! No. of total evts during this run
      RETURN
      END
