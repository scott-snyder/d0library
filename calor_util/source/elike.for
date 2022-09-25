      REAL FUNCTION ELIKE(LPELC,FHAD,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Perform Neyman-Pearson test on joint likelihood
C-                         of electron ID variables. To select electrons
C-                         require ELIKE(LPELC,FHAD,IER)<k, where k is a
C-                         positive number. The larger k is the higher is
C-                         the efficiency of the cut and the lower the
C-                         rejection. A typical value is k = 2 for an
C-                         efficiency of approximately 90%.
C-
C-   Returned value  : ratio likelihood(background)/likelihood(signal)
C-   Inputs  : LPELC - pointer to PELC/PPHO bank
C-             FHAD  - fraction of hadron overlaps in background
C-   Outputs : IER   - error flag (negative values indicate HARD FAILURES)
C-
C-                     0 = success,
C-                    10 = likelihood computed without TRDinfo even though
C-                         TRD was requested. This is due to either
C-                         missing TRD link or TRD acceptance was set false.
C-                    20 = VTXT only TRK (FDCT or DTRK missing)
C-                    21 = VTX only and not in TRD acceptance
C-
C-                    -1 = insufficient information (PELC,ZTRK,DTRK,TRDT,HMTE
C-                         or FDCT bank missing)
C-                    -2 = cluster outside CAl fiducial region
C-                    -3 = Chisq or EMfraction are zero
C-                    -4 = DTRK and VTXT LINK missing for CC cluster
C-                    -5 = FDCT, DTRK and VTXT link missing for EC cluster
C-                   -10 = NO ZTRK associated with electron
C-
C-
C-   Controls: ELIKE.RCP
C-
C-   Created  17-NOV-1994   Ulrich Heintz
C-   Updated   7-MAR-1995   Ian Adam  Add EC code
C-   Updated  27-MAR-1995   James T. McKinley Fix mask and error handling
C-   Updated  13-APR-1995   Ian Adam  Change mask bit order
C-   Updated  15-MAY-1995   Ian Adam  Allow CDC track for EC PELCs
C-   Updated  22-JUN-1995   Meenakshi Narain : add more error flags
C-   Updated   5-AUG-1995   Meenakshi Narain  ADD 1A and 1B tables
C-   Updated  11-OCT-1995   Meenakshi Narain  update to include VTX only tracks
C-   Updated  27-MAR-1996   Meenakshi Narain  update to include EC TRD
C-                                            tables from Bob Kehoe
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'

      INTEGER NVAR_CC,NVAR_EC,NVAR_VTX
      PARAMETER( NVAR_CC = 5 )
      PARAMETER( NVAR_EC = 5 )
      PARAMETER( NVAR_VTX = 4 )
      INTEGER NVAR_CC_NAMES,NVAR_EC_NAMES,NVAR_VTX_NAMES
      PARAMETER( NVAR_CC_NAMES = 2*NVAR_CC )
      PARAMETER( NVAR_EC_NAMES = 2*NVAR_EC )
      PARAMETER( NVAR_VTX_NAMES = NVAR_VTX )

      CHARACTER*25 NAME_CC(NVAR_CC_NAMES)
      DATA NAME_CC/ 'CDC_DEDX_1A',
     &              'H-MATRIX_CHISQUARED_1A',
     &              'TRACK_MATCH_1A',
     &              'EM_FRACTION_CC_1A',
     &              'TRD_EPSILON_1A',
     &              'CDC_DEDX_1B',
     &              'H-MATRIX_CHISQUARED_1B',
     &              'TRACK_MATCH_1B',
     &              'EM_FRACTION_CC_1B',
     &              'TRD_EPSILON_1B'/
      CHARACTER*25 NAME_EC(NVAR_EC_NAMES)
      DATA NAME_EC/ 'FDC_DEDX_1A',
     &              'H-MATRIX_CHISQUARED_EC_1A',
     &              'TRACK_MATCH_EC_1A',
     &              'EM_FRACTION_EC_1A',
     &              'TRD_EPSILON_EC_1A',
     &              'FDC_DEDX_1B',
     &              'H-MATRIX_CHISQUARED_EC_1B',
     &              'TRACK_MATCH_EC_1B',
     &              'EM_FRACTION_EC_1B',
     &              'TRD_EPSILON_EC_1B'/
      CHARACTER*25 NAME_VTX(NVAR_VTX_NAMES)
      DATA NAME_VTX/'VTX_DEDX',
     &              'VTX_TRACK_MATCH_CC',
     &              'VTX_TRACK_MATCH_EC',
     &              'VTX_TRD_EPSILON_EC'/

      INTEGER N_CC(NVAR_CC),N_EC(NVAR_EC),N_VTX(NVAR_VTX)
      REAL    BIN_CC(NVAR_CC),P_CC(150,NVAR_CC),X_CC(2,NVAR_CC)
      REAL    BIN_EC(NVAR_EC),P_EC(400,NVAR_EC),X_EC(2,NVAR_EC)
      REAL    BIN_VTX(NVAR_VTX),P_VTX(400,NVAR_VTX),X_VTX(2,NVAR_VTX)
      LOGICAL QVAR_CC(NVAR_CC),QVAR_EC(NVAR_EC)

      INTEGER LPELC,LZTRK,LDTRK,LHMTE,LFDCT,LVTXT
      INTEGER STATUS,IER,ISTATUS,IBIN,IVAR,I,IETA,IBLAST
      INTEGER MASK_CC,MASK_EC,JBIT
      INTEGER OLDRUN,CURRENT_RUN,NCTRD_EC
      INTEGER NBVAR,NEVAR,KRUN,J,IVTX,IVOFF,IFOFF,TRD_CROSSED_LAYERS
      REAL    CQUAN(50),TQUAN(50),VAR,PE,PH,PEE,FHAD
      REAL    ELIKE_SET_MASK_CC,ELIKE_SET_MASK_EC
      REAL    PXEE,PXH,PXEE_IBLAST,PXH_IBLAST,CTRD_EC(8)
      LOGICAL FIRST,OK,NOTRD,CHANGE_RCP,VTXONLY,BTEST
      LOGICAL GEOM(3),BADTRK(10),FDCVTX_TRK
      DATA FIRST / .TRUE. /
      DATA QVAR_CC/NVAR_CC*.TRUE./
      DATA QVAR_EC/NVAR_EC*.TRUE./
      SAVE OLDRUN
C----------------------------------------------------------------------
      IF( FIRST ) THEN
        FIRST = .FALSE.
        CALL INRCP('ELIKE_RCP',STATUS)
        IF(STATUS.EQ.0)CALL EZPICK('ELIKE_RCP')
C
C ****  USE same tables for 1A and 1B for VTX only tracks
C **** read only once at the beginning
C
        DO I=1,NVAR_VTX
          IF(STATUS.EQ.0)CALL EZGETA_iarr(NAME_VTX(I),0,0,0,N_VTX(I)
     &         ,STATUS)
          IF(STATUS.EQ.0)CALL EZGETA(NAME_VTX(I),1,N_VTX(I)-2,
     &      1,P_VTX(1,I),STATUS)
          IF(STATUS.EQ.0)CALL EZGETA(NAME_VTX(I),N_VTX(I)-1,N_VTX(I),1,
     &      X_VTX(1,I),STATUS)
          IF (STATUS.NE.0) CALL ERRMSG('ELIKE_RCP','ELIKE',
     &      'error reading RCP','F')
          IF (I.LT.NVAR_VTX) THEN
            N_VTX(I)= (N_VTX(I)-2)/2    ! only electrons and hadrons
          ELSE    ! for the last variable (TRD) special treatment of tables
            N_VTX(I)= (N_VTX(I)-2)/4    ! only electrons and hadrons
          ENDIF
          BIN_VTX(I)=(X_VTX(2,I)-X_VTX(1,I))/FLOAT(N_VTX(I))
        ENDDO
        CALL EZRSET
        OLDRUN = -1
      ENDIF
C
      CHANGE_RCP = .FALSE.
      KRUN = IQ(LHEAD+6)
      CURRENT_RUN = 0                 ! = 0 FOR RUN1A
      IF (KRUN.GE.70000) THEN
        CURRENT_RUN = 1               ! = 1 FOR RUN1B
      ENDIF
      IF (OLDRUN.EQ.CURRENT_RUN) THEN
        CHANGE_RCP  = .FALSE.
      ELSE
        OLDRUN = CURRENT_RUN
        CHANGE_RCP  = .TRUE.
      ENDIF

      IF (CHANGE_RCP) THEN
        CALL EZPICK('ELIKE_RCP')
        IF (CURRENT_RUN.EQ.1) THEN
          NBVAR =NVAR_CC_NAMES/2+1
          NEVAR = NVAR_CC_NAMES
        ELSE
          NBVAR = 1
          NEVAR = NVAR_CC_NAMES/2
        ENDIF
        DO I=NBVAR,NEVAR
          IF(I .GT. NVAR_CC_NAMES/2)THEN
            J = I - NVAR_CC_NAMES/2
          ELSE
            J = I
          ENDIF
          IF(STATUS.EQ.0)CALL EZGETA_iarr(NAME_CC(I),0,0,0,N_CC(J)
     &         ,STATUS)
          IF(STATUS.EQ.0)CALL EZGETA(NAME_CC(I),1,N_CC(J)-2,1,P_CC(1,J),
     &      STATUS)
          IF(STATUS.EQ.0)CALL EZGETA(NAME_CC(I),N_CC(J)-1,N_CC(J),1,
     &      X_CC(1,J),STATUS)
          IF (STATUS.NE.0) CALL ERRMSG('ELIKE_RCP','ELIKE',
     &      'error reading RCP','F')
          N_CC(J)=(N_CC(J)-2)/3
          BIN_CC(J)=(X_CC(2,J)-X_CC(1,J))/FLOAT(N_CC(J))
        ENDDO
        IF (CURRENT_RUN.EQ.1) THEN
          NBVAR = NVAR_EC_NAMES/2+1
          NEVAR = NVAR_EC_NAMES
        ELSE
          NBVAR = 1
          NEVAR = NVAR_EC_NAMES/2
        ENDIF
        DO I=NBVAR,NEVAR
          IF(I .GT. NVAR_EC_NAMES/2)THEN
            J = I - NVAR_EC_NAMES/2
          ELSE
            J = I
          ENDIF
          IF(STATUS.EQ.0)CALL EZGETA_iarr(NAME_EC(I),0,0,0,N_EC(J)
     &         ,STATUS)
          IF(STATUS.EQ.0)CALL EZGETA(NAME_EC(I),1,N_EC(J)-2,1,P_EC(1,J),
     &      STATUS)
          IF(STATUS.EQ.0)CALL EZGETA(NAME_EC(I),N_EC(J)-1,N_EC(J),1,
     &      X_EC(1,J),STATUS)
          IF (STATUS.NE.0) CALL ERRMSG('ELIKE_RCP','ELIKE',
     &      'error reading RCP','F')
          IF (I.LT.NEVAR) THEN
            N_EC(J)=(N_EC(J)-2)/3
          ELSE    ! for the last variable (TRD) special treatment of tables
            N_EC(J)=(N_EC(J)-2)/6
          ENDIF
          BIN_EC(J)=(X_EC(2,J)-X_EC(1,J))/FLOAT(N_EC(J))
        ENDDO
        IF(STATUS.EQ.0)CALL EZGETA_iarr('CONST_TRD_EC',0,0,0,NCTRD_EC
     &       ,STATUS)
        IF(STATUS.EQ.0)CALL EZGETA('CONST_TRD_EC',1,NCTRD_EC,1,CTRD_EC,
     &      STATUS)
        IF (STATUS.NE.0) CALL ERRMSG('ELIKE_RCP','ELIKE',
     &      'error reading RCP','F')
        CALL EZRSET
      ENDIF ! FIRST

      ELIKE=999.
      IER=0

      IF(LPELC.LE.0) THEN
        CALL ERRMSG('NO PELC BANK','ELIKE',' ','W')
        IER = -1
        GOTO 999
C      ELSE IF(Q(LPELC-4).NE.4HPELC) THEN
C        CALL ERRMSG('NOT A PELC BANK','ELIKE',' ','W')
C        IER = -1
C        GOTO 999
      ENDIF

      IETA = INT(ABS(Q(LPELC+19)))
      CALL CLEANEM(LPELC,1,OK,ISTATUS)
      CALL CLEANEM_CQUANS(IVAR,CQUAN)
      CALL CLEANEM_TQUANS(IVAR,TQUAN)

      PE=1.
      PH=1.
      PEE=1.
      NOTRD = .FALSE.

      LZTRK=LQ(LPELC-3)
      IF(LZTRK.GT.0) THEN
        LDTRK=LQ(LZTRK-7)
        LFDCT=LQ(LZTRK-8)
        LVTXT=LQ(LZTRK-6)
      ELSE
        LDTRK=0
        LFDCT=0
        LVTXT=0
        CALL ERRMSG('No ZTRK associated with electron - skip ',
     &    'ELIKE',' ','W')
        IER = -10
        GOTO 999
      ENDIF
      LHMTE=LQ(LPELC-1)
C
C       Flag  Hard Failure if
C         1) CHISQ or EMF =0, should not happen!
C         2) if DTRK and VTXT missing for CC clusters
C
      IF ((Q(LHMTE+7).LE.0..AND.(QVAR_CC(2).OR.QVAR_EC(2))).OR.
     &    (CQUAN(9).LE.0.  .AND.(QVAR_CC(4).OR.QVAR_EC(4)))) THEN
        CALL ERRMSG('Chisq or EMfraction is zero - skip','ELIKE',' ',
     &    'W')
        IER = -3
        GOTO 999
      ENDIF
      VTXONLY = .FALSE.
      IF(IETA.LE.12) THEN  ! CC
        IF ((LDTRK.LE.0).AND.(QVAR_CC(1).OR.QVAR_CC(3))) THEN
          IF ((LVTXT.LE.0).AND.(QVAR_CC(1).OR.QVAR_CC(3))) THEN
            CALL ERRMSG('cluster in CC, but DTRK and VTXT missing-skip',
     &        'ELIKE',' ','W')
            IER = -4
            GOTO 999
          ELSE
            VTXONLY = .TRUE.
            IER = 20
          ENDIF
        ENDIF
      ELSE IF (IETA.GE.15.AND.IETA.LE.25) THEN ! EC
        IF (QVAR_EC(1).OR.QVAR_EC(3)) THEN
          IF ((LFDCT.LE.0).AND.(LDTRK.LE.0)) THEN
            IF (LVTXT.LE.0) THEN
              CALL ERRMSG('EC cluster - FDCT/DTRK/VTXT missing ',
     &        'ELIKE',' ','W')
              IER = -5
              GOTO 999
            ELSE
              IER = 20
              VTXONLY = .TRUE.
            ENDIF
          ENDIF
        ENDIF
      ENDIF
C
C ****  Compute electron likelihood
C
      IF(IETA.LE.12) THEN  ! CC

        DO I=1,NVAR_CC
          IF(QVAR_CC(I))THEN
            IF(I.EQ.1)THEN
              IF (VTXONLY) THEN
                VAR = Q(LVTXT+20)
                IVTX = 1
              ELSE
                VAR = Q(LDTRK+20)             ! CDC dE/dx
              ENDIF
            ELSEIF(I.EQ.2)THEN
              VAR = Q(LHMTE+7)              ! H-matrix chi**2
            ELSEIF(I.EQ.3)THEN
              VAR = TQUAN(12)               ! track match significance
              IF (VTXONLY) IVTX = 2
            ELSEIF(I.EQ.4)THEN
              VAR = CQUAN(9)                ! EM fraction
            ELSEIF(I.EQ.5)THEN
              IF(TQUAN(22).EQ.0) THEN ! TRD acceptance
                CALL ERRMSG(
     &          'TRD acceptance failed - get likelihood without TRD',
     &          'ELIKE',' ','W')
                IER = 10
                NOTRD = .TRUE.
                VAR = 0.
              ELSE
                VAR = TQUAN(23)               ! TRD epsilon
              ENDIF
            ENDIF
            IF (NOTRD) THEN
              GOTO 111
            ENDIF
            IF (VTXONLY.AND.(I.EQ.1.OR.I.EQ.3)) THEN
              IBIN=INT((VAR-X_VTX(1,IVTX))/BIN_VTX(IVTX))+1
              IF(IBIN.LT.1)IBIN=1
              IF(IBIN.GT.N_VTX(I))IBIN=N_VTX(IVTX)
              PE  = PE* P_VTX(IBIN,IVTX)
C             set PH and PEE as same for VTX only tracks
              PH  = PH* P_VTX(N_VTX(IVTX)+IBIN,IVTX)
              PEE = PEE*P_VTX(N_VTX(IVTX)+IBIN,IVTX)
            ELSE
              IBIN=INT((VAR-X_CC(1,I))/BIN_CC(I))+1
              IF(IBIN.LT.1)IBIN=1
              IF(IBIN.GT.N_CC(I))IBIN=N_CC(I)
              PE  = PE*P_CC(IBIN,I)
              PH  = PH*P_CC(N_CC(I)+IBIN,I)
              PEE = PEE*P_CC(2*N_CC(I)+IBIN,I)
            ENDIF
  111       CONTINUE
          ENDIF
        ENDDO

        IF(PE.GT.0) THEN
          ELIKE=(FHAD*PH+(1.-FHAD)*PEE)/PE
        ELSE
          ELIKE=999.
        ENDIF

      ELSE IF (IETA.GE.15.AND.IETA.LE.25) THEN ! EC

        DO I=1,NVAR_EC
          IF(QVAR_EC(I))THEN
            IF(I.EQ.1)THEN
              IF (VTXONLY) THEN
                VAR = Q(LVTXT+20)
                IVTX = 1
              ELSE IF (LFDCT.GT.0) THEN
                VAR = TQUAN(14)               ! FDC dE/dx
              ELSE
                VAR = Q(LDTRK+20)
              ENDIF
            ELSEIF(I.EQ.2)THEN
              VAR = Q(LHMTE+7)              ! H-matrix chi**2
            ELSEIF(I.EQ.3)THEN
              VAR =  TQUAN(12)              ! track match significance
              IF (VTXONLY) IVTX = 3
            ELSEIF(I.EQ.4)THEN
              VAR = CQUAN(9)                ! EM fraction
            ELSEIF(I.EQ.5)THEN
              IF(TQUAN(22).EQ.0) THEN       ! not in TRD acceptance
                CALL ERRMSG(
     &          'TRD acceptance failed - get likelihood without TRD',
     &          'ELIKE',' ','W')
                IF (VTXONLY) THEN
                  IER = 21
                ELSE
                  IER = 10
                ENDIF
                NOTRD = .TRUE.
                VAR = 0.
              ELSE                          ! in TRD acceptance
                VAR = TQUAN(23)             ! TRD epsilon
                CALL TRD_NUM_LAYERS(LPELC,GEOM,BADTRK,                  
     &              TRD_CROSSED_LAYERS)
                IF (TRD_CROSSED_LAYERS.EQ.0) THEN
                  CALL ERRMSG('num. layers and acceptance disagree',
     &                'ELIKE',' ','F')
                  GOTO 999
                ENDIF
                FDCVTX_TRK = .FALSE.
                IF (LFDCT.GT.0.AND.LVTXT.GT.0) THEN
                  IF (.NOT.BTEST(IQ(LZTRK),0)) FDCVTX_TRK = .TRUE.
                ELSEIF (VTXONLY) THEN
                  IVTX = 4
                ENDIF
              ENDIF
            ENDIF
            IF (NOTRD) THEN
              GOTO 222
            ENDIF
            IF ( (I.EQ.1.AND.(LFDCT.LE.0.AND.LDTRK.GT.0)).OR.
     &        (I.EQ.5.AND.TRD_CROSSED_LAYERS.EQ.3) ) THEN
              IBIN=INT((VAR-X_CC(1,I))/BIN_CC(I))+1
              IF(IBIN.LT.1)IBIN=1
              IF(IBIN.GT.N_CC(I))IBIN=N_CC(I)
              PE  = PE*P_CC(IBIN,I)
              PH  = PH*P_CC(N_CC(I)+IBIN,I)
              PEE = PEE*P_CC(2*N_CC(I)+IBIN,I)
            ELSEIF (VTXONLY.AND.(I.EQ.1.OR.I.EQ.3.OR.I.EQ.5)) THEN
              IBIN=INT((VAR-X_VTX(1,IVTX))/BIN_VTX(IVTX))+1
              IF(IBIN.LT.1)IBIN=1
              IF(IBIN.GT.N_VTX(IVTX))IBIN=N_VTX(IVTX)
              IVOFF = 1
              IF (I.EQ.5) THEN
                IBIN= IBIN+N_VTX(IVTX)*(TRD_CROSSED_LAYERS-1)
                IVOFF = 2
              ENDIF
              PE  = PE* P_VTX(IBIN,IVTX)
C             set PH and PEE as same for VTX only tracks
              PH  = PH* P_VTX(IVOFF*N_VTX(IVTX)+IBIN,IVTX)
              PEE = PEE*P_VTX(IVOFF*N_VTX(IVTX)+IBIN,IVTX)
            ELSE IF (I.EQ.5.AND.TRD_CROSSED_LAYERS.GT.0) THEN
              IFOFF = 0
              IBLAST = 0
              IBIN=INT((VAR-X_EC(1,I))/BIN_EC(I))+1
              IF(IBIN.LT.1)IBIN=1
              IF(IBIN.GE.N_EC(I)) THEN
                IBIN=N_EC(I)
                IBLAST = 1
              ENDIF
              IBIN= IBIN+N_EC(I)*(TRD_CROSSED_LAYERS-1)
              IF (FDCVTX_TRK) IFOFF = 1
              PXH_IBLAST  = CTRD_EC(TRD_CROSSED_LAYERS+2*IFOFF)
              PXEE_IBLAST = CTRD_EC(4+TRD_CROSSED_LAYERS+2*IFOFF)
C             now get probabilities depending on 1 or 2 lyr tracks, and
C             FDC/VTX or FDC-only track
              IF (IBLAST.EQ.1) THEN
                PXH  = PXH_IBLAST
                PXEE = PXEE_IBLAST
              ELSE
                PXH  = P_EC(2*N_EC(I)+IBIN,I)*(1-PXH_IBLAST)
     &            /(1-P_EC(N_EC(I)*(TRD_CROSSED_LAYERS+2),I))
                PXEE = P_EC(4*N_EC(I)+IBIN,I)*(1-PXEE_IBLAST)
     &            /(1-P_EC(N_EC(I)*(TRD_CROSSED_LAYERS+4),I))
              ENDIF
              PE  = PE*P_EC(IBIN,I)
              PH  = PH*PXH
              PEE = PEE*PXEE
            ELSE
              IBIN=INT((VAR-X_EC(1,I))/BIN_EC(I))+1
              IF(IBIN.LT.1)IBIN=1
              IF(IBIN.GT.N_EC(I))IBIN=N_EC(I)
              PE  = PE*P_EC(IBIN,I)
              PH  = PH*P_EC(N_EC(I)+IBIN,I)
              PEE = PEE*P_EC(2*N_EC(I)+IBIN,I)
            ENDIF
  222       CONTINUE
          ENDIF
        ENDDO

        IF(PE.GT.0) THEN
          ELIKE=(FHAD*PH+(1.-FHAD)*PEE)/PE
        ELSE
          ELIKE=999.
        ENDIF

      ELSE

        CALL ERRMSG('CAL FIDUCIAL FAILED','ELIKE',' ','W')
        IER = -2
        GOTO 999

      ENDIF !CC OR EC

  999 RETURN

      ENTRY ELIKE_SET_MASK_CC(MASK_CC)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : set the mask that determines which electron ID
C-                         variables are used to compute likelihoods.
C-
C-   Inputs  : MASK - bit pattern to determine variables, 1 turns a variable on,
C-                    0 turns it off
C-                    bit 1 - CDC dE/dx
C-                    bit 2 - H-matrix chisquared
C-                    bit 3 - track match
C-                    bit 4 - EM fraction
C-                    bit 5 - TRD epsilon
C-
C----------------------------------------------------------------------
      DO I=1,NVAR_CC
        IF(JBIT(MASK_CC,I).GT.0)THEN
          QVAR_CC(I)=.TRUE.
        ELSE
          QVAR_CC(I)=.FALSE.
        ENDIF
      ENDDO
      ELIKE = 999.
C----------------------------------------------------------------------
      RETURN

      ENTRY ELIKE_SET_MASK_EC(MASK_EC)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : set the mask that determines which electron ID
C-                         variables are used to compute likelihoods.
C-
C-   Inputs  : MASK - bit pattern to determine variables, 1 turns a variable on,
C-                    0 turns it off
C-                    bit 1 - FDC dE/dx
C-                    bit 2 - H-matrix chisquared
C-                    bit 3 - track match
C-                    bit 4 - EM fraction
C-                    bit 5 - TRD epsilon (1, 2, and 3-layer tracks)
C-
C----------------------------------------------------------------------
      DO I=1,NVAR_EC
        IF(JBIT(MASK_EC,I).GT.0)THEN
          QVAR_EC(I)=.TRUE.
        ELSE
          QVAR_EC(I)=.FALSE.
        ENDIF
      ENDDO
      ELIKE = 999.
C----------------------------------------------------------------------
      RETURN

      END
