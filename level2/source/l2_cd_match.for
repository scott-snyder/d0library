      SUBROUTINE L2_CD_MATCH(ETA,PHI,XYZ_CLUS,WETA,WPHI,
     &  RETURN_FLAG,NO_DECISION)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Check for charged track in the core of ETA,PHI
C-
C-   Inputs  : Eta,PHI,Weta,Wphi
C-
C-      Reflects the current implementation within Xia Yi's ELECTRON Tool
C-
C-      Eta, Phi are center positions of the EM3 (eta,phi) tower 
C-               showing the maximum energy
C-               Need to be scaled * 0.10 to be real eta,phi
C-      Weta, Wphi are simply road widths selected by parameter file,
C-               not a calculated resolution
C-
C-   Outputs : Return_flag
C-   Controls: 
C-
C-   Created  19-AUG-1991   Daniel R. Claes
C-            08-NOV-1991   Modifying for final tool form
C-            17-AUG-1992   New L2_EM position algorithm passes EM cluster
C-                          position within the array XYZ_CLUS(3)
C-            18-NOV_1993   For VETO options in L2_EM return NO_DECISION
C-                          when hitfinding not attempted.
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:PI.DEF/LIST'                             
      INCLUDE 'D0$INC:L2TRAK.INC/LIST'                             
      INCLUDE 'D0$LINKS:IZL2CD.LINK'
      INCLUDE 'D0$PARAMS:L2CD.PARAMS'
C
      REAL ETA,PHI,ROAD(1:6,0:6,0:3),WETA,WPHI
      REAL TEST_EDGE, TRTIME, XYZ_CLUS(3)
      INTEGER EVTOLD, GZL2CD, L2CD, LSUP1, LSUP2
      LOGICAL RETURN_FLAG, NO_DECISION
      CHARACTER*4 OLD_PATH            ! Current PATH to restore upon RETURN
C                                         
C For error message routine
C
      CHARACTER*80 NOWERRMESS           ! Error message
      CHARACTER*16 NOWSMESS             ! short description
C
C----------------------------------------------------------------------
      LOGICAL L2TRAK_INIT
      COMMON /L2TRKINIT/ L2TRAK_INIT
C
      LOGICAL CDD1_FLG, CDD2_FLG, CDD3_FLG
      COMMON /RECDD_FLAGS/ CDD1_FLG, CDD2_FLG, CDD3_FLG
C
C----------------------------------------------------------------------
C
      DATA EVTOLD/ -1 /
      DATA TRTIME /0.0/               ! A constant offset to be determined
C                                     ! early in run.  In COSMIC or 
C                                     ! TESTBEAM data changes event by evt
C
C     CDC_EDGE,FDC_EDGE/ 1.4?, 1.1?/ ! Eta boundaries of CDC/FADC
C                                    ! coverage - Check Jeff/Sue
C----------------------------------------------------------------------
C
      NO_DECISION = .TRUE.
      RETURN_FLAG = .FALSE.
C
      CALL PATHGT(OLD_PATH)
      CALL PATHST('FILT')
C
C  If initialization failed (unable to access SL2H banks) can do NO filtering
      IF (.NOT. L2TRAK_INIT ) THEN
        NOWSMESS = 'L2_CD_MATCH'
        NOWERRMESS =
     &    'L2_CD_MATCH failed to init : NO TRACKING attempted'
        RETURN_FLAG = .TRUE.  ! Accept event to be safe.
        GOTO 995
      END IF
C
      IF (MC_FLAG) THEN
        IF (LHEAD.EQ.0) THEN              ! Check if Top bank exists
          GO TO 999
        ENDIF
      ENDIF
      IF (COS_FLAG) THEN
        CALL L2_TRTIME(TRTIME)                      ! Needed for COSMIC tests
C
        CALL COSMIC_TRGR(ETA, PHI, WETA, WPHI, ROAD)! Corrects for beam trgr
      ELSE
        CALL L2_ROAD(ETA, PHI, XYZ_CLUS, WETA, WPHI, ROAD)! Returns ETAmin and
      ENDIF                                               ! ETAmax road limits
C
C      PHImin = ROAD(1,WIRE,LAYER)
C      PHImax = ROAD(2,WIRE,LAYER)
C      ETAmin = ROAD(3,WIRE,LAYER)
C      ETAmax = ROAD(4,WIRE,LAYER)
C      Zmin   = ROAD(5,WIRE,LAYER)
C      Zmax   = ROAD(6,WIRE,LAYER)
C
C       Use ETA values to check CDC/FADC coverage
C
      IF (ETA.GE.0) THEN
        TEST_EDGE = ROAD(4,0,0)
      ELSE
        TEST_EDGE = ABS(ROAD(3,0,0))
      ENDIF
      IF (TEST_EDGE.LT.CDC_EDGE) THEN ! Road is fully contained within central
C                                     ! region. Perform quick CDC hit counting
        L2CD = GZL2CD()               ! and demand that a track  be  seen here.
C
        IF (.NOT.CDC_FLAG) THEN
          RETURN_FLAG = .TRUE.
          GOTO 999
        ENDIF
        IF (MC_FLAG) THEN                   ! Need to re-format DATA
          IF (IQ(LHEAD+7) .NE. EVTOLD) THEN ! New event
            EVTOLD = IQ(LHEAD+7)
!                        ! Chris' routine to reformat the
!            CALL RECDD2 ! MC data is now handled externally 
!                        ! as a separate package.
            IF (.NOT.CDD2_FLG) GOTO 999
          ENDIF
        ENDIF
C
        IF (L2CD .GT. 0) THEN ! Loop over all L2CD banks in the linear chain
C                             ! for any previous call with this same candidate
          LSUP1 = L2CD
  400     CONTINUE
          IF (Q(LSUP1+PETA).EQ.ETA .AND. Q(LSUP1+PPHI).EQ.PHI) THEN
            IF (Q(LSUP1+PDETA).LE.WETA .AND. 
     &        Q(LSUP1+PDPHI).LE.WPHI) THEN
              IF (IQ(LSUP1+PSTAT).EQ.L2CD_TRK) THEN
                RETURN_FLAG = .TRUE.
                NO_DECISION = .FALSE.
                GOTO 999        ! Track already identified within road
              ENDIF
            ENDIF
            IF (Q(LSUP1+PDETA).GE.WETA .AND.
     &        Q(LSUP1+PDPHI).GE.WPHI) THEN
              IF (IQ(LSUP1+PSTAT).NE.L2CD_TRK) THEN ! Already know  there
                NO_DECISION = .FALSE.               ! is NO track in road
                GOTO 999                            
              ENDIF
            ENDIF
          ENDIF
          LSUP2 = LQ(LSUP1)
          IF (LSUP2 .GT. 0) THEN
            LSUP1 = LSUP2
            GOTO 400
          ENDIF
        ENDIF
C
        CALL CDC_ROAD(ETA, PHI, XYZ_CLUS, WETA, WPHI, ROAD)! Returns PHImin and
C                                                          ! PHImax road limits
C
        CALL L2_CDC(ROAD,TRTIME, L2CD, RETURN_FLAG) ! Returns latest L2CD in
C                                                   ! linear chain of banks
C
        IF (L2CD.EQ.0) THEN                         ! No L2CD bank booked.
C                                                   ! This can  happen on
          NOWSMESS = 'L2_CD_MATCH'                  ! events with  NO CDD
C                                                   ! raw or L2 hit banks
          NOWERRMESS =                              ! or L2_CDCUNP failed.
     &    'Problems w/raw CDD2 banks : NO TRACKING attempted.'
          RETURN_FLAG = .TRUE.                      ! Accept event to be safe.
          GOTO 995        
        ENDIF
C
        NO_DECISION = .FALSE.
C
        Q(L2CD + PETA) = ETA
        Q(L2CD + PPHI) = PHI
        Q(L2CD + PDETA) = WETA
        Q(L2CD + PDPHI) = WPHI
C
        IF (RETURN_FLAG) THEN
          IQ(L2CD + PSTAT) = L2CD_TRK ! Track identified by hit-counting
        ELSE
          IQ(L2CD + PSTAT) = L2CD_NOTRK
        ENDIF
        GOTO 999                      ! No need to examine other regions
      ENDIF
C
      IF (ETA.GE.0) THEN
        TEST_EDGE = ROAD(3,0,0)
      ELSE
        TEST_EDGE = ABS(ROAD(4,0,0))
      ENDIF
      IF (TEST_EDGE.GT.FDC_EDGE) THEN ! Road is fully contained within forward
        IF (.NOT.FDC_FLAG) THEN       ! region. Perform quick FDC hit counting
          RETURN_FLAG = .TRUE.        ! and demand that a track  be  seen here.
          GOTO 999
        ENDIF
C
        CALL L2_FDC(ETA,PHI,WETA,WPHI,RETURN_FLAG)  
C
        NO_DECISION = .FALSE.
        GOTO 999        
C
      ENDIF
C
C     Road overlaps GAP region between CDC/FDC.  Must pass event.
C
      RETURN_FLAG = .TRUE.
      GOTO 999
C
  995 CONTINUE
      CALL ERRMSG(NOWSMESS,'L2_CD_MATCH',NOWERRMESS,'W')
C
  999 CONTINUE
C
C--   Return to initial path
C
      CALL PATHST(OLD_PATH)
C
      RETURN
      END
