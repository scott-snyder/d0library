      SUBROUTINE CHMATRIX_FILL_QUAN(EN_VIS,EN_DEAD,EN_CRACK,EN_CRYO,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Set up H matrix quantities event by event
C-                         Both Dead material and Live material
C-                         Energies
C-
C-   Inputs  :
C-
C-   Outputs :  ETAC = Etac of tower with maximum energy
C-              PHIC = Phi of tower with maximum energy
C-              EN_VIS - VISIBLE ENERGY IN CLUSTER
C-              EN_DEAD = DEAD ENERGY IN CLUSTER
C-              EN_CRACK = energy in crack
C-              ENER_CRYO = energy in cryostat. Obtained when
C-              use_dead_energy flag is set.
C-              IER  set on Error
C-
C-   Controls:  SHOULD CALL C_SETUP_ETAC_PHIC BEFORE THIS ROUTINE IS CALLED.
C-              this routine is similar to CHQUAN for
C               FULL and LONGITUDINAL MATRIX but the dependence
C               on MATRIX_CHOICE is very different and more flexible
C               It also has a choice of TRANSVERSE MATRIX.
C               plus it allows more combinations of position and
C               invisible quantites the coice of matrix being used.
C               Routine is controlled by the H MATRIX flag ACCUMULATE
C               If in accumulate mode, it will setup dead material energies
C               and positions to predict upon request
C
C-   Updated  25-FEB-1992   Meenakshi Narain
C-   Updated  14-MAR-1992   Rajendran Raja  changed name to chmatrix_fill_quan
C-   Updated  18-MAR-1992   Meenakshi Narain
C-                          Use the dimensions of the matrix
C-                          to setup the matrix elements.
C-   Updated  19-MAR-1992   Meenakshi Narain
C-                          make COOR_PTR RCP driven
C-   Updated  25-MAR-1992   Meenakshi Narain
C-                          determine correctly for negative etas the position
C-                          of the matrix quantities in the C(LQUAN) vector
C-   Updated  16-APR-1992   Rajendran Raja  Added Alog10(visible_energy )
C-   Squared
C-   Updated   9-AUG-1992   Meenakshi Narain
C-                          modified so that hmatrix can be rerun only using
C-                          info stored on DST (ie from CASH banks)
C-   Updated  12-AUG-1992   Meenakshi Narain
C-                          take care of dimensionality problem for ETA > 26
C-
C-   The dimensions of supported matrices are :
C-
C-    5 x 5 : use sum of energies in EM floor 1 through 4 and FH layer 1
C-
C-   16 x 16: use sum of energies in EM floor 1,2,4, FH layer 1,
C-            energies in the four cells of EM 3 floor in the core tower,
C-            and the eight nearest neighbour towers in EM 3.
C-
C-   40 x 40: use sum of energies in EM layer 1,2,4, FH layer 1,
C-            and the 36 cell energies in EM3 core tower and its 8 NBR towers
C-
C-   72 x 72: use energies in core tower and its eight NBR for
C-            EM layer 1-7 and FH layer 1
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$PARAMS:DEAD_MATERIALS.PARAMS'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:ZLINKC.INC'
      INCLUDE 'D0$INC:ZHMATRIX.INC'
      INCLUDE 'D0$INC:CHMATR_NEW.INC'
      INCLUDE 'D0$INC:CIMPACT.INC'
      INCLUDE 'D0$INC:CTRAK.INC'
      INCLUDE 'D0$INC:PI.DEF'
      INCLUDE 'D0$LINKS:IZCACH.LINK'
      INCLUDE 'D0$INC:HMATRIX_PARS.INC'
C
      INTEGER I, J, K
      INTEGER PCAEH,IETA,IPHI,ILYR,DELETA,DELPHI,ISIGN
      INTEGER NDATA,NREP,POINTER,IADDR,JBIT,JBYT,IENFL,GZCAEP
      INTEGER IDEPTH,IX,IET,IPH, COOR_PTR
      INTEGER IOFF, IER, NCELL, ILAYER, NCHARS
      INTEGER TRANS_MATRIX16(9)
      logical PHILIM, ETALIM
      INTEGER NCELLS,  PACKED_WORD
C
C
C ****  dimensions of matrices supported.
C
      INTEGER EXTRA_VARS,VIS_DIM5,VIS_DIM13,VIS_DIM16,VIS_DIM40
      INTEGER VIS_DIM45,VIS_DIM72
      PARAMETER( EXTRA_VARS = 1 )  !Number of extra variables
      PARAMETER( VIS_DIM5 =  5 + EXTRA_VARS)
      PARAMETER( VIS_DIM13 =  13 + EXTRA_VARS)
      PARAMETER( VIS_DIM16 =  16 + EXTRA_VARS)
      PARAMETER( VIS_DIM40 =  40 + EXTRA_VARS)
      PARAMETER( VIS_DIM45 =  45 + EXTRA_VARS)
      PARAMETER( VIS_DIM72 =  72 + EXTRA_VARS)
C
      REAL    ETACM,ETAIM,SIGN, EN_VIS, EN_DEAD
      REAL    EN_CRACK,EN_CRYO, ECELL
C
      CHARACTER*8 COOR_SYS
      CHARACTER*80 EMSG
      LOGICAL OK, FIRST, QLIM
      LOGICAL INVISIBLES, USE_CASH_INFO
      LOGICAL REWORK,USE_LOG_REWORK
      REAL    CNEIGH1_SPREAD,WCL_CENTROID,SCEXP
      REAL    XX,YY,ZZ,ETOT,EE
      REAL    XTEST
      CHARACTER*80 STRING
C
      DATA    FIRST/.TRUE./
C----------------------------------------------------------------------
      IX(IET,IPH,ILYR) = 1 + (IET-NETLO) + (IPH-NPHLO)*NETTOT +
     &  (ILYR-1)*NETTOT*NPHTOT                ! INDEX STATEMENT FUNCTION.
      QLIM(I,J,K) = ((I.GE.J) .AND. (I.LE.K))
C
      IER = 0
      IF (FIRST) THEN
        FIRST=.FALSE.
        IF(ACCUMULATE)THEN
          CALL EZPICK('HMATRIX_RCP')
        ELSE
          CALL EZPICK('HMATRIX_RZ_RCP')
        ENDIF
        CALL EZGET_iarr('TRANS_MATRIX16',TRANS_MATRIX16,IER)
        CALL EZRSET
C
        INVISIBLES = (INVIS_DIM .NE. 0)
C
        COOR_PTR = 0        ! default to Cartesian Coordinate System
        IF (USE_POSITION_INFO)THEN
          CALL EZ_GET_CHARS('USE_COOR_SYSTEM',NCHARS,COOR_SYS,IER)
          IF (COOR_SYS(1:3).EQ.'CYL'.OR. COOR_SYS(1:3).EQ.'cyl')
     &      COOR_PTR = 1
        ENDIF
        CALL EZPICK('HMATRIX_RCP')
        CALL EZGET_i('USE_CASH_INFO',USE_CASH_INFO,IER)
        CALL EZGET_l('REWORK_CLUSTER_CENTROID',REWORK,IER)
        CALL EZGET_l('USE_LOG_REWORK',USE_LOG_REWORK,IER)
        CALL EZGET('WINDOW_CLUSTER_CENTROID',WCL_CENTROID,IER)
        CALL EZGET('WINDOW_CNEIGH1_HMATRIX',CNEIGH1_SPREAD,IER)
        WRITE(EMSG,11) VIS_DIM
        CALL ERRMSG('CALORIMETER','CHMATRIX',EMSG,'I')
   11   FORMAT(' HMATRIX VISIBLE DIMENSIONS = ',I3)
        CALL EZRSET
C
        CALL EZPICK('CAPHEL_RCP')
        CALL EZGET('POWER_FOR_SHOWER_CENTER',SCEXP,IER)
        CALL EZRSET
      ENDIF
C
      EN_VIS = 0.
      EN_DEAD = 0.
      EN_CRACK = 0.
      EN_CRYO = 0.
      DO I = 1, TOT_DIM
        C(LQUAN + I) =0.
      END DO
C
      NCELLS = NCACH
      IF (USE_CASH_INFO) THEN
        LCASH  = LQ(LCACL-2)
        NCELLS = IQ(LCASH+2)
      END IF
      IF ( REWORK ) THEN
        CALL UZERO(EM3AV,1,3)
        ETOT=0.0
      ENDIF
      DO 100 I = 1,NCELLS

        IF (USE_CASH_INFO) THEN
          POINTER = LCASH + 2*(I-1)
          PACKED_WORD = IQ(POINTER+3)
          ECELL = Q(POINTER+4)
          CALL CAEP_INDICES(PACKED_WORD,IETA,IPHI,ILYR)
        ELSE
          PCAEH = IQ(LCACH+I+2)
          PCAEH = LCAEH + NRCAEH*(PCAEH-1)
          IETA  = IQ(PCAEH+12)
          IPHI  = IQ(PCAEH+13)
          ILYR  = IQ(PCAEH+14)
          ECELL = Q(PCAEH+7)
        END IF
        IDEPTH = 0
        CALL CNEIGH1(ETAC,PHIC,IETA,IPHI,DELETA,DELPHI,CNEIGH1_SPREAD)
        DELETA = SIGN_ETA*DELETA    ! take care of negative eta

        IF (VIS_DIM.EQ.VIS_DIM5) THEN
C
C:::      EM LAYERS
C
          IF(ILYR.LT.LYEM3A)IDEPTH = ILYR
          IF(ILYR.GE.LYEM3A .AND. ILYR.LE.LYEM3D)IDEPTH = 3
          IF(ILYR.GT.LYEM3D .AND. ILYR.LE.MXLYEM)IDEPTH = 4
C
C:::      FH
C
          IF(ILYR.EQ.MNLYFH)IDEPTH = 5      ! FINE HADRONIC PUNCH THROUGH

        ELSE IF (VIS_DIM.EQ.VIS_DIM16) THEN

          IF(ILYR.LT.LYEM3A) THEN
            IDEPTH = ILYR
          ELSEIF(ILYR.EQ.MXLYEM) THEN
            IDEPTH = 15
          ELSE IF (ILYR.EQ.MNLYFH) THEN     ! FINE HADRONIC PUNCH THROUGH
            IDEPTH = 16
          ELSE IF ( ILYR .GE. LYEM3A .AND. ILYR .LE. LYEM3D) THEN
            PHILIM = QLIM(DELPHI,-1,1)
            ETALIM = QLIM(DELETA,-1,1)
            IF (PHILIM .AND. ETALIM)THEN
              IDEPTH = TRANS_MATRIX16((DELPHI+2) + 3*(DELETA+1))
              IF (DELPHI.EQ.0 .AND. DELETA.EQ.0) THEN
                IDEPTH = ILYR
                IF (SIGN_ETA.EQ.-1) THEN
                  IDEPTH = ILYR - (4*((ILYR-1)/4)-2)
                END IF
              ENDIF
            ENDIF
          ENDIF

        ELSE IF (VIS_DIM.EQ.VIS_DIM40) THEN

          IF(ILYR.LT.LYEM3A) THEN
            IDEPTH = ILYR + 36
          ELSEIF(ILYR.EQ.MXLYEM) THEN
            IDEPTH = 39
          ELSE IF (ILYR.EQ.MNLYFH) THEN     ! FINE HADRONIC PUNCH THROUGH
            IDEPTH = 40
          ELSE IF ( ILYR .GE. LYEM3A .AND. ILYR .LE. LYEM3D) THEN
            ILAYER=ILYR-2
            IF (SIGN_ETA.EQ.-1) THEN
              ILAYER = ILYR - (4*((ILYR-1)/4)-2) - 2
            END IF
            PHILIM = QLIM(DELPHI,-1,1)
            ETALIM = QLIM(DELETA,-1,1)
            IF (PHILIM .AND. ETALIM)THEN
              IDEPTH = IX(DELETA,DELPHI,ILAYER)
            ENDIF
          ENDIF

        ELSE IF (VIS_DIM.EQ.VIS_DIM13) THEN

          IF(ILYR.LT.LYEM3A) THEN
            IDEPTH = ILYR + 9
          ELSEIF(ILYR.EQ.MXLYEM) THEN
            IDEPTH = 12
          ELSE IF (ILYR.EQ.MNLYFH) THEN     ! FINE HADRONIC PUNCH THROUGH
            IDEPTH = 13
          ELSE IF ( ILYR .GE. LYEM3A .AND. ILYR .LE. LYEM3D) THEN
            ILAYER = 1
            PHILIM = QLIM(DELPHI,-1,1)
            ETALIM = QLIM(DELETA,-1,1)
            IF (ETAC.EQ.32) THEN   ! take care of IPHI = ODD above eta 32
              IF (IETA.EQ.31.OR.IETA.EQ.32) THEN
                PHILIM = QLIM(DELPHI,-2,2)
                IF (PHILIM) DELPHI = ISIGN(1, DELPHI)
              END IF
            ELSE IF (ETAC.EQ.33) THEN
              IF (IETA.EQ.32) THEN
                PHILIM = QLIM(DELPHI,-2,2)
                IF (PHILIM) DELPHI = ISIGN(1, DELPHI)
              END IF
            ENDIF
            IF (PHILIM .AND. ETALIM)THEN
              IDEPTH = IX(DELETA,DELPHI,ILAYER)
            ENDIF
          ENDIF

        ELSE IF (VIS_DIM.EQ.VIS_DIM72) THEN

          PHILIM = QLIM(DELPHI,-1,1)
          ETALIM = QLIM(DELETA,-1,1)
C
          IF (PHILIM .AND. ETALIM)THEN
            IF ( ILYR .GE. MNLYEM .AND. ILYR. LE.MXLYEM ) THEN
              IF (ILYR .GE. LYEM3A .AND. ILYR .LE.LYEM3D) THEN
                IF (SIGN_ETA.EQ.-1) THEN
                  ILYR = ILYR - (4*((ILYR-1)/4)-2)
                END IF
              END IF
              IDEPTH = IX(DELETA,DELPHI,ILYR)
            ELSEIF(ILYR.EQ.MNLYFH)THEN
              IDEPTH = IX(DELETA,DELPHI,8)
            ENDIF
          ENDIF

        ELSE IF (VIS_DIM.EQ.VIS_DIM45) THEN

          PHILIM = QLIM(DELPHI,-1,1)
          ETALIM = QLIM(DELETA,-1,1)
C
          IF (ETAC.EQ.32) THEN       ! take care of IPHI = ODD above eta 32
            IF (IETA.EQ.31.OR.IETA.EQ.32) THEN
              PHILIM = QLIM(DELPHI,-2,2)
              IF (PHILIM) DELPHI = ISIGN(1, DELPHI)
            END IF
          ELSE IF (ETAC.EQ.33) THEN
            IF (IETA.EQ.32) THEN
              PHILIM = QLIM(DELPHI,-2,2)
              IF (PHILIM) DELPHI = ISIGN(1, DELPHI)
            END IF
          ENDIF
C
          IF (PHILIM .AND. ETALIM)THEN
            IF ( ILYR .LT. LYEM3A ) THEN
              IDEPTH = IX(DELETA,DELPHI,ILYR)
            ELSE IF (ILYR.LT.LYEM3D) THEN
              IDEPTH = IX(DELETA,DELPHI,3)
            ELSE IF (ILYR.EQ.MXLYEM) THEN
              IDEPTH = IX(DELETA,DELPHI,4)
            ELSE IF(ILYR.EQ.MNLYFH)THEN
              IDEPTH = IX(DELETA,DELPHI,5)
            ENDIF
          ENDIF

        ELSE
          CALL ERRMSG('CALORIMETER','CHMATRIX',
     &      ' HMATRIX DIMENSIONS NOT SUPPORTED ','W')
          GO TO 999

        ENDIF

        IF (IDEPTH.NE.0) THEN
          EN_VIS = EN_VIS + ECELL
          C(LQUAN+IDEPTH) = C(LQUAN+IDEPTH) + ECELL
        END IF
C
        IF ( REWORK ) THEN
          CALL CNEIGH1(ETAC,PHIC,IETA,IPHI,DELETA,DELPHI,WCL_CENTROID)
          PHILIM = QLIM(DELPHI,-1,1)
          ETALIM = QLIM(DELETA,-1,1)
          IF ( PHILIM.AND.ETALIM ) THEN
            CALL CELXYZ(IETA,IPHI,ILYR,XX,YY,ZZ,IER)
            IF(IER.NE.0)THEN
              CALL ERRMSG('CALORIMETER','CHMATRIX_FILL_QUAN',
     &          ' CELXYZ ERROR ','W')
              WRITE(STRING,1) IETA,IPHI,ILYR
    1         FORMAT(' ','IETA,IPHI,ILYR',3I5)
              CALL ERRMSG('CALORIMETER','CHMATRIX_FILL_QUAN',STRING,'W')
            ELSE
              EE = ABS(ECELL)**SCEXP
              IF(ECELL.LT.0.0)EE = 0.0
              EM3AV(1) = EM3AV(1) + XX*EE
              EM3AV(2) = EM3AV(2) + YY*EE
              EM3AV(3) = EM3AV(3) + ZZ*EE
              ETOT = ETOT +EE
            ENDIF
          ENDIF
        ENDIF
  100 CONTINUE
C
      IF ( REWORK ) THEN
        IF(ETOT.NE.0)THEN
          EM3AV(1) = EM3AV(1)/ETOT
          EM3AV(2) = EM3AV(2)/ETOT
          EM3AV(3) = EM3AV(3)/ETOT
          CALL UCOPY(EM3AV,Q(LCACL+14),3)   !STORE IT BACK IN CACL
          IF ( USE_LOG_REWORK ) THEN
            XTEST = XBAR3(1)**2 + XBAR3(2)**2 + XBAR3(3)**2
            IF ( XTEST.GT.0.0 ) THEN
C
C ****  LOG WEIGHTING HAS BEEN SUCCESSFUL
              CALL UCOPY(XBAR3,Q(LCACL+14),3)   !USE LOG CENTER INSTEAD
            ENDIF
          ENDIF
        ENDIF
      ENDIF
C
      IF ( EXTRA_VARS.GT.0 ) THEN
        C(LQUAN+VIS_DIM) = SIGN_ETA*VERT(3)/Z_VERTEX_SPREAD
C
C ****  PUTTING VERTEX AS AN EXTRA VARIABLE
C
      ENDIF
C
C ****  NOW TO ACCUMULATE DEAD MATERIAL ENERGIES
C
      IOFF =0
      IF(INVISIBLES.AND.ACCUMULATE) THEN
        IF (USE_DEAD_ENERGY) THEN
C
          CALL PATHST('GEAN')               ! Set path to Gean
          LCAEP = GZCAEP()                  ! Get link to CAEP bank
          LCAEP = LQ(LCAEP)                 ! Need to get second CAEP bank
          IF ( LCAEP.LE.0 ) GOTO 888        ! Error - No data
          NDATA = IQ(LCAEP-1)               ! Number of data words
          NCELL = IQ(LCAEP+3)               ! Number of cells with data
          NREP = IQ(LCAEP+2)                ! Repetition number
          DO 500 I = 1, NCELL               ! Loop over cells
            POINTER = NREP*(I-1)+LCAEP      ! Pointer
            IADDR = IQ(POINTER+4)           ! Packed Addr in Physics Indices
            IETA = JBYT(IADDR,25,8)
            IF(IETA.GE.128)IETA = IETA -256 ! 2'S COMPLEMENT
            IPHI = JBYT(IADDR,17,8)
            ILYR = JBYT(IADDR,9,8)
            IENFL = JBIT(IADDR,6)           !Flag to see if Energy in Gev.
            IF(IENFL.NE.0)THEN
              CALL ERRMSG('CALORIMETER','CHMATRIX',
     &          'DEAD MATERIAL ENERGY NOT IN GEV ','W')
            ENDIF
            IF(ILYR.GE.DEADLO.AND.ILYR.LE.DEADHI)THEN
              IF(ILYR.EQ.CCRACK.OR.ILYR.EQ.EN_CRACK)THEN
                IDEPTH = VIS_DIM + 1
                IF (VIS_DIM.EQ.VIS_DIM72) IDEPTH = IX(DELETA,DELPHI,9)
                EN_CRACK = EN_CRACK + Q(POINTER+5)
              ELSE
                IDEPTH = VIS_DIM + 2
                IF (VIS_DIM.EQ.VIS_DIM72) IDEPTH = IX(DELETA,DELPHI,10)
                EN_CRYO = EN_CRYO + Q(POINTER+5)
              ENDIF
              C(LQUAN+IDEPTH) = C(LQUAN+IDEPTH) + Q(POINTER+5)
              EN_DEAD = EN_DEAD + Q(POINTER+5)
            ENDIF
  500     CONTINUE
          IOFF = 2 !  assume that if dead energy used then they are the
C                     first 2 entries in the invisibles list
          IF (VIS_DIM.EQ.VIS_DIM72) IOFF = 9 * IOFF
          CALL PATHRS
          LCAEP = GZCAEP()                      ! Resetting to true CAEP
          GO TO 889
        ENDIF

  888   CONTINUE
        CALL PATHRS                           ! Reset to default path
        LCAEP = GZCAEP()                      ! Resetting to true CAEP

  889   CONTINUE
C
C ****  NOW TO ADD IMPACT POINT INFO
C
        IF (USE_POSITION_INFO) THEN
          CALL C_SETUP_TRACK(IER)    !SETUP TRACK
          IF ( IER.NE.0)GO TO 777
          CALL CAL_EM_IMPACT
          CALL CAL_EM_IMPACT2(DELTA_PHI,DELTA_Z,DELTA_R)
          IF (COOR_PTR.EQ.0) THEN
            DO 900 I = 1,3
              C(LQUAN+VIS_DIM+IOFF+I) = DEL_IMPACT(I) ! FILL IN POSITION INFO.
  900       CONTINUE
          ELSE
            C(LQUAN+VIS_DIM+IOFF+1) =  DELTA_PHI
            C(LQUAN+VIS_DIM+IOFF+2) =  DELTA_Z
            C(LQUAN+VIS_DIM+IOFF+3) =  DELTA_R
          END IF
C
        ENDIF
      ENDIF
C
C ****  NOW CHECK WHETHER ONE WANTS ENERGY RATIOS
C
  777 CONTINUE
      IF (USE_ENERGY_RATIO) THEN

        IF (EN_VIS.EQ.0.) THEN
          CALL ERRMSG('CALORIMETER','CHMATRIX',
     &      ' CLUSTER WITH ZERO ENERGY USED !!!! ','W')
          IER = IER + 1
          GO TO 999
        ENDIF

        DO I = 1, VIS_DIM - 1 - EXTRA_VARS
          C(LQUAN+I) = C(LQUAN + I)/EN_VIS
        END DO
        C(LQUAN + VIS_DIM -EXTRA_VARS) = ALOG10(EN_VIS)
        IF (INVISIBLES .AND. USE_DEAD_ENERGY.AND.ACCUMULATE) THEN
          DO I = VIS_DIM+1, VIS_DIM + IOFF
            C(LQUAN + I) = C(LQUAN + I)/EN_VIS
          END DO
        END IF
      ENDIF
      IER = 0
  999 RETURN
      END
