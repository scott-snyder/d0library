      SUBROUTINE FLJNEP
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :  For every JETS bank in the event, determine
C-                          whether any of its cells are shared by a
C-                          reconstructed electron or photon.  If yes,
C-                          book and fill a JNEP bank to hang from
C-                          the JETS bank which will contain the same
C-                          variables as JETS, but their values will be
C-                          those determined from only those cells which
C-                          are NOT shared with any PELC/PPHO and also
C-                          provide reference links from JETS to the
C-                          overlapping PELC/PPHO.
C-
C-   Inputs  :    None
C-   Outputs :    None
C-   Controls:
C-
C-   Created  22-NOV-1991   Dhiman Chakraborty
C-   Updated  11-AUG-1992   Qizhong Li-Demarteau
C-                            fixed the bug on CAPH path
C-   Updated  04-NOV-1992   Dhiman Chakraborty
C-                            fixed the bug causing overwrite in case of
C-                            too many JETS/PELC/PPHO banks in an event.
C-   Updated  04-NOV-1992   Qizhong Li-Demarteau  added protection to
C-                                  avoid crash in case of illegal bank
C-                                  adddress.
C-   Updated  05-NOV-1992   Dhiman Chakraborty
C-                            ordered PELC/PPHOs in decreasing order of
C-                            Et to make sure that the most energetic
C-                            ones aren't ignored in case there are
C-                            more than NMAX_EP PELC+PPHO banks in an
C-                            event.
C-   Updated  15-JUL-1993   N. Graf, R. Astur : Make sure RESET_CAPH is
C-                            called, even when there is an error.
C-   Updated   5-JAN-1994   Dhiman Chakraborty
C-                            increased MAX_NCELL_J from 1000 to 3000 and
C-                            added check to cover pathological jets which
C-                            have NCELL_J>MAX_NCELL_J
C-   Updated  22-JAN-1994   Meenakshi Narain : fixed bugs
C-                        a) initialize L correctly in loop over EM/JET
C-                            cell match
C-                        b) compute CELL_PHI and CELL_ETA correctly
C-                        c) do not make JNEP if the EM/JET overlap is
C-                          100%
C-                        d) in the calculation of RMSETA and RMSPHI checks
C-                        added to test for positove values, else reset to 0.
C-                        e) check added to see whether jets E is positive
C-                        in computing E(jnep)/E(jets)
C-                        f) add errmsg calls
C-   Updated  31-OCT-1995   Dhiman Chakraborty
C-                        1. Use CASH for EM clusters if CACH doesn't exist
C-                        2. Some streamlining
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKC.INC'
      INCLUDE 'D0$INC:ZLINKA.INC'
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$INC:PTCAEP.INC'
      INCLUDE 'D0$PARAMS:BYTE_ORDER.PARAMS'
      INTEGER GZCAEH,GZPELC,GZPPHO,GZJETS
C----------------------------------------------------------------------
      INTEGER PACKED_ADDRESS
      BYTE BYTES(4)
      EQUIVALENCE (PACKED_ADDRESS,BYTES)
C----------------------------------------------------------------------
      INTEGER LEP,ADDR_JTSH
      INTEGER NMAX_EP,NMAX_J
      PARAMETER (NMAX_EP = 20)
      PARAMETER (NMAX_J = 50)
      INTEGER ADDR_EP(NMAX_EP),ADDR_J(NMAX_J),INDEX(NMAX_EP)
      INTEGER NUM_EP,NUM_J,EP,IJET,II,JJ,KK,LL,MM
      INTEGER NCELL_EP,NCELL_J,NCELLS,MAX_NCELL_J
      INTEGER IETA,IPHI,LAYER
      PARAMETER( MAX_NCELL_J = 3000 )
      INTEGER LPTR,NREP
      INTEGER NFLAG,NEP
      INTEGER FLAG(MAX_NCELL_J)
      REAL    IER
      REAL    ETAPHI_J(2),DEL_PHI,DEL_R(NMAX_EP)
      REAL    ETAPHI_EP(2*NMAX_EP),TEMP_EP(2*NMAX_EP)
      REAL    TEMPLATE(5,4)
      REAL    FSTORE(13)
      REAL    CONE_RAD,ADD_DR,NN_CONE_RAD
      REAL    THETA,PHI,ETA,ETEM
      REAL    CELL_PHI,CELL_ETA,CELL_THETA,JET_PHI,DCELL_PHI
      REAL    SUMPHI,SUMPHISQ,SUMETA,SUMETASQ
      REAL    MEANPHI,MEANPHISQ,MEANETA,MEANETASQ,RMSPHI,RMSETA
      REAL    PI
      REAL    VSUM
C
      INTEGER IVEC
      REAL    EVEC(4)
C
      CHARACTER*8 ALGO(4)
      CHARACTER*8 LNK_NAM_EP(NMAX_EP),LNK_NAM_J(NMAX_J)
      LOGICAL FIRST,EZERR,QEXIST_JNEP,CEXIST,USE_CASH
      EXTERNAL CEXIST
      SAVE FIRST
      DATA FIRST/.TRUE./
      DATA TEMPLATE/
     &  1.,6.,0.7,0.,0.,    !  CONE  R=0.7
     &  1.,6.,0.5,0.,0.,    !  CONE  R=0.5
     &  1.,6.,0.3,0.,0.,    !  CONE  R=0.3
     &  2.,7.,2.,8.,2./     !  NN 2x2
      DATA ALGO/'CONE_JET','CONE_JET','CONE_JET','NN_JET'/
      PARAMETER( PI = 3.14159 )
C----------------------------------------------------------------------
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        CALL EZPICK('CAJNEP_RCP')       ! select CAJNEP_RCP bank
C
        IF (EZERR(IER)) THEN
          CALL ERRMSG('CAJNEP','FLJNEP',
     &      'CAJNEP RCP bank not found in FLJNEP.','W')
        ELSE
          CALL EZGET('NN_CONE_RAD',NN_CONE_RAD,IER) ! Get CONE_RAD to be
C                                         ! assumed for NN_JET algorithm
          CALL EZGET('ADD_DR',ADD_DR,IER) ! and del_R to be allowed
                                          ! (same for all algorithms)
          CALL EZRSET
        ENDIF
      ENDIF
      LCAEH = GZCAEH()              ! Pointer to CAEH
      IF(LCAEH.LE.0)THEN
        CALL ERRMSG('CAJNEP','FLJNEP',
     &          'CAEH not found in FLJNEP.','W')
        GOTO 999
      ENDIF
      NREP = IQ(LCAEH+2)
C
C     loop over all PELC and PPHO banks in the event, make one array
C     of their addresses and another of their eta's & phi's
C
      CALL VZERO(ETAPHI_EP,2*NMAX_EP)
      LPELC = GZPELC()           ! Pointer to the first PELC bank
      IF(LPELC.NE.0) THEN
C          sort banks so they are in increasing order of Et
C          NOTE: after each reordering of banks the pointer
C                LPELC must be refetched
        CALL ZSORT(IXCOM,LPELC,7)
        LPELC = GZPELC()
        CALL ZTOPSY(IXCOM,LPELC) ! reverse the order to decreasing
        LPELC = GZPELC()
      ENDIF
      NUM_EP = 0
      DO WHILE ((LPELC.GT.0).AND.(NUM_EP.LT.NMAX_EP/2))
        NUM_EP = NUM_EP+1
        WRITE(UNIT=LNK_NAM_EP(NUM_EP),FMT=10) NUM_EP
   10   FORMAT ('LNK_EP',I2.2)
        CALL GSLINK(LNK_NAM_EP(NUM_EP),ADDR_EP(NUM_EP))
        LSLINK(ADDR_EP(NUM_EP)) = LPELC     ! Storing addresses of PELC/PPHO
        ETAPHI_EP(2*NUM_EP-1) = Q(LPELC+9)  ! Storing eta's
        ETAPHI_EP(2*NUM_EP) = Q(LPELC+10)   ! and phi's
        LPELC = LQ(LPELC)
      ENDDO
C
      LPPHO = GZPPHO()           ! Pointer to the first PPHO bank
      IF(LPPHO.NE.0) THEN
C          sort banks so they are in increasing order of Et
C          NOTE: after each reordering of banks the pointer
C                LPPHO must be refetched
        CALL ZSORT(IXCOM,LPPHO,7)
        LPPHO = GZPPHO()
        CALL ZTOPSY(IXCOM,LPPHO) ! reverse the order to decreasing
        LPPHO = GZPPHO()
      ENDIF
      DO WHILE ((LPPHO.GT.0).AND.(NUM_EP.LT.NMAX_EP))
        NUM_EP = NUM_EP+1
        WRITE(UNIT=LNK_NAM_EP(NUM_EP),FMT=10) NUM_EP
        CALL GSLINK(LNK_NAM_EP(NUM_EP),ADDR_EP(NUM_EP))
        LSLINK(ADDR_EP(NUM_EP)) = LPPHO     ! Storing addresses of PELC/PPHO
        ETAPHI_EP(2*NUM_EP-1) = Q(LPPHO+9)  ! Storing eta's
        ETAPHI_EP(2*NUM_EP) = Q(LPPHO+10)   ! and phi's
        LPPHO = LQ(LPPHO)
      ENDDO
      IF (NUM_EP.EQ.0)GOTO 999  ! no electrons/photons in this event
C
C     loop over jet algorithms
C
      DO 501 II = 1,4
        CALL SET_CAPH(ALGO(II),TEMPLATE(1,II),IER)
        IF(II.LE.3) THEN
          CONE_RAD = TEMPLATE(3,II)
        ELSE
          CONE_RAD = NN_CONE_RAD
        ENDIF
C
C     loop over all JETS banks in the event and make an array
C     of their addresses
C
        NUM_J = 0
        LJETS = GZJETS()        ! Pointer to the first JETS bank
        DO WHILE ((LJETS.GT.0).AND.(NUM_J.LT.NMAX_J))
          IF(IQ(LJETS+1).EQ.1)THEN             ! If JETS version is 1
            CALL MZPUSH(IXCOM,LJETS,4,4,' ')   ! update it to version 2
            ADDR_JTSH = LQ(LJETS-2)
            IQ(LJETS+1) = 2
            Q(LJETS+12) = Q(ADDR_JTSH+3)
            Q(LJETS+13) = Q(ADDR_JTSH+4)
            Q(LJETS+14) = Q(ADDR_JTSH+5)
            IQ(LJETS+15) = 0
            CALL MZDROP(IXCOM,ADDR_JTSH,' ')
          ENDIF
          NUM_J = NUM_J+1
          WRITE(UNIT=LNK_NAM_J(NUM_J),FMT=20) NUM_J
   20     FORMAT ('LNK_J',I2.2)
          CALL GSLINK(LNK_NAM_J(NUM_J),ADDR_J(NUM_J))
          LSLINK(ADDR_J(NUM_J)) = LJETS     ! Storing addresses of JETS
          LJETS = LQ(LJETS)
        ENDDO
        IF (NUM_J.EQ.0)GOTO 501  ! no jets with this algo in this event
C
C     loop over the JETS banks to select PELC and PPHO banks and compare
C     cells with them
C
        DO 401 IJET = 1,NUM_J
          LJETS = LSLINK(ADDR_J(IJET))
          LJPTS = LQ(LJETS-1)         ! Pointer to JPTS  from JETS
          IF(LJPTS.LE.0)THEN
            CALL ERRMSG('CAJNEP','FLJNEP',
     &          'Link from JETS to JPTS not found in FLJNEP.','W')
            GOTO 997
          ENDIF
C
          NCELL_J = IQ(LJPTS+2)    ! # of cells in the JETS cluster
          IF(NCELL_J.GT.MAX_NCELL_J) THEN
            NCELL_J = MAX_NCELL_J
            CALL ERRMSG('TOO MANY JPTS CELLS FOR JNEP','FLJNEP',
     &        'USING ONLY FIRST MAX_NCELL_J CELLS IN JPTS','W')
          ENDIF

          ETAPHI_J(1) = Q(LJETS+9)
          ETAPHI_J(2) = Q(LJETS+8)
          JET_PHI = Q(LJETS+8)
C
C     In eta-phi space, find the distance of jet from electrons/photons.
C     The INDEX(1) th electron/photon is closest to the jet & so on...
C     But, first take care of the fact that phi=0 and phi = 2*pi coincide.
C
          CALL VFILL(TEMP_EP,2*NMAX_EP,10.)
          DO EP = 1,NUM_EP
            TEMP_EP(2*EP-1) = ETAPHI_EP(2*EP-1)
            DEL_PHI = ABS(ETAPHI_EP(2*EP)-ETAPHI_J(2))
            IF(DEL_PHI.GT.PI)DEL_PHI = 2*PI - DEL_PHI
            TEMP_EP(2*EP) =  DEL_PHI
          ENDDO
          ETAPHI_J(2) = 0.
          CALL SORT_VEC_DIST(2,ETAPHI_J,NUM_EP,TEMP_EP,DEL_R,INDEX)
C
C     Pick only electrons/photons which are within (CONE_RAD + ADD_DR)
C     of the jet in eta-phi.  ADD_DR is picked up from CAJNEP_RCP file.
C     For cone_jet algorithms, CONE_RAD is the radius of the jet cone
C     and for NN algorithm, CONE_RAD is picked up from CAJNEP_RCP.
C
          CALL UFILL(FLAG,1,NCELL_J,1)  !  Preset first NCELL_J elements
C             of array FLAG to 1
          KK = 1
          NEP = 0
          DO WHILE ((DEL_R(INDEX(KK)).LE.(CONE_RAD+ADD_DR)).AND.
     &          (KK.LE.NUM_EP))
            LEP = LSLINK(ADDR_EP(INDEX(KK))) ! Pointer to PELC/PPHO
            IF (LEP .LE. 0) THEN
              CALL ERRMSG('CAJNEP','FLJNEP',
     &          ' illegal bank address for PELC/PPHO','W')
              GOTO 997
            ENDIF
            LCACL = LQ(LEP-2)               ! Pointer to CACL
            IF (LCACL .LE. 0) THEN
              CALL ERRMSG('CAJNEP','FLJNEP',
     &              'Link from PELC/PPHO to CACL not found','W')
              GOTO 997
            ENDIF
C
C  Now try to get the cell pointers (from CAEP) for PELC/PPHO
C
            LCACH = LQ(LCACL-1)             ! Pointer to CACH
            USE_CASH = LCACH.LE.0
            IF(USE_CASH)THEN
              LCASH = LQ(LCACL-2)             ! Pointer to CASH
              IF(LCASH.LE.0)THEN
                CALL ERRMSG('CAJNEP','FLJNEP',
     &            'Link from CACL to CACH/CASH not found in FLJNEP','W')
                GOTO 997
              ENDIF
              NCELL_EP = IQ(LCASH+2)       ! # of cells in the E/P cluster
            ELSE
              NCELL_EP = IQ(LCACH+2)       ! # of cells in the E/P cluster
            ENDIF
C
C     loop over the cells in the jet and EM cluster to (un)flag those
C     cells of jet which are shared with the EM cluster
C
            LL = 1
            NFLAG  = 0
            DO WHILE ((NFLAG.LT.NCELL_EP).AND.(LL.LE.NCELL_J))
              DO 201 MM = 1,NCELL_EP
                IF(USE_CASH) THEN
                  PACKED_ADDRESS = IQ(LCASH+2+2*MM-1)
                  IETA = BYTES(BYTE4)
                  IPHI = BYTES(BYTE3)
                  LAYER= BYTES(BYTE2)
                  IF(.NOT.CEXIST(IETA,IPHI,LAYER)) THEN
                    CALL ERRMSG('FLJNEP','CEXIST',
     &                'Cell pointed to by CASH does not exist','W')
                    GOTO 201
                  ENDIF
                  IF(PTCAEP(IETA,IPHI,LAYER).EQ.IQ(LJPTS+2+LL)) THEN
                    FLAG(LL) = 0
                    NFLAG = NFLAG+1
                  ENDIF
                ELSE
                  IF(IQ(LCACH+2+MM).EQ.IQ(LJPTS+2+LL)) THEN
                    FLAG(LL) = 0
                    NFLAG = NFLAG+1
                  ENDIF
                ENDIF
  201         CONTINUE
              LL = LL+1
            ENDDO
            IF(NFLAG.GT.0)THEN
              NEP = NEP + 1
              IF(NEP.LE.4)LQ(LJETS-2-NEP) = LEP  ! pointer FROM JETS to
C                                                ! overlapping PELC/PPHO.
            ENDIF
            KK = KK+1
          ENDDO
C
C       If there are cells in the jet which are shared with electron/
C       photon cluster(s), fill array FSTORE with quantities calculated
C       from the cells that are NOT shared with any e/p cluster.
C
          IF(NEP.GT.0)THEN
            CALL VZERO(FSTORE,13)
            ETEM = 0.
            SUMPHI = 0.
            SUMETA = 0.
            SUMPHISQ = 0.
            SUMETASQ = 0.
            QEXIST_JNEP=.FALSE.
            DO 351 MM = 1,NCELL_J
              LPTR = LCAEH + NREP*(IQ(LJPTS+2+MM)-1)
              IF (FLAG(MM).GT.0) THEN
                QEXIST_JNEP=.TRUE.
C
C     The following 7 quantities are obtained by adding those of each cell
C
                FSTORE(1) = FSTORE(1) + Q(LPTR+4)            ! Ex
                FSTORE(2) = FSTORE(2) + Q(LPTR+5)            ! Ey
                FSTORE(3) = FSTORE(3) + Q(LPTR+6)            ! Ez
                FSTORE(4) = FSTORE(4) + Q(LPTR+7)            ! E
                FSTORE(5) = FSTORE(5) + Q(LPTR+8)            ! Et
                FSTORE(9) = FSTORE(9) + Q(LPTR+9)            ! sig(Ex)**2
                FSTORE(10) = FSTORE(10) + Q(LPTR+10)         ! sig(Ey)**2
C
C     Compute the remaining quantities
C
                DO IVEC=1,4
                  EVEC(IVEC)=Q(LPTR+3+IVEC)
                ENDDO
                CALL ETOETA(EVEC,CELL_PHI,CELL_THETA,CELL_ETA)
                CELL_PHI = CELL_PHI - JET_PHI
                IF (ABS(CELL_PHI) .GT. PI) THEN
                  DCELL_PHI = 2*PI - ABS(CELL_PHI)
                  CELL_PHI = -SIGN(DCELL_PHI,CELL_PHI)
                ENDIF
                SUMPHI = SUMPHI + Q(LPTR+8)*CELL_PHI
                SUMETA = SUMETA + Q(LPTR+8)*CELL_ETA
                SUMPHISQ = SUMPHISQ + Q(LPTR+8)
     &            *CELL_PHI*CELL_PHI
                SUMETASQ = SUMETASQ + Q(LPTR+8)
     &            *CELL_ETA*CELL_ETA
C
                IF(IQ(LPTR+14).LE.7)            ! the cell is in EM cal
     &            ETEM = ETEM+Q(LPTR+8)          ! Et(em)
              ENDIF
  351       CONTINUE
C
C     Find theta, phi and eta for the electron/photon subtracted jet
C
            IF (QEXIST_JNEP) THEN
              CALL ETOETA(FSTORE,PHI,THETA,ETA)
              FSTORE(6) = THETA
              FSTORE(7) = PHI
              FSTORE(8) = ETA
C     Find rms phi and rms eta widths
              NCELLS = INT(VSUM(FLAG,NCELL_J))
              IF (FSTORE(5).GT.0) THEN
                MEANPHI = SUMPHI/FSTORE(5)
                MEANETA = SUMETA/FSTORE(5)
                MEANPHISQ = SUMPHISQ/FSTORE(5)
                MEANETASQ = SUMETASQ/FSTORE(5)
                IF ((MEANPHISQ - MEANPHI*MEANPHI).GT.0) THEN
                  RMSPHI = SQRT(ABS(MEANPHISQ - MEANPHI*MEANPHI))
                ELSE
                  CALL ERRMSG('JNEP RMS PHI ERROR','FLJNEP',' ','W')
                  RMSPHI = 0.0
                ENDIF
                IF ((MEANETASQ - MEANETA*MEANETA).GT.0) THEN
                  RMSETA = SQRT(ABS(MEANETASQ - MEANETA*MEANETA))
                ELSE
                  CALL ERRMSG('JNEP RMS ETA ERROR','FLJNEP',' ','W')
                  RMSETA = 0.0
                ENDIF
                FSTORE(11) = RMSETA             ! rms eta
                FSTORE(12) = RMSPHI             ! rms phi
                FSTORE(13) = ETEM/FSTORE(5)     ! Et(em)/Et(total)
              ENDIF
C
C     Book and fill a JNEP bank
C
              CALL BKJNEP(LJETS,LJNEP)
              CALL UCOPY(FSTORE,Q(LJNEP+2),13)
              IQ(LJNEP+15) = IQ(LJETS+15) ! Flag for merging/splitting
              IF (Q(LJETS+5).GT.0) THEN
                Q(LJNEP+16) = FSTORE(4)/Q(LJETS+5) ! E(JNEP)/E(JETS)
              ELSE
                CALL ERRMSG('JETS ET NOT POSITIVE','FLJNEP',' ','W')
                Q(LJNEP+16) = 0.0
              ENDIF
            ENDIF
          ENDIF
  401   CONTINUE
C
C ****  Loop over links and release each link
C
  997   DO JJ = 1, NUM_J
          CALL RSLINK(LNK_NAM_J(JJ),ADDR_J(JJ))
        ENDDO
C
  501 CALL RESET_CAPH
C  501 CONTINUE
C
  998 DO JJ = 1, NUM_EP
        CALL RSLINK(LNK_NAM_EP(JJ),ADDR_EP(JJ))
      ENDDO
C
  999 RETURN
      END
