      SUBROUTINE L2JETS_CALIB
C- -----------------------------------------------------------------------
C-
C-   Purpose and Methods : Multiply Jet ET by a calibration factor
C-
C-   Inputs  :
C-   Outputs :
C-   Controls: Calibration parameters are held in L2JETS_CONT.INC
C-
C-   Created  14-AUG-1990   Richard V. Astur
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:L1PHP.PARAMS'
      INCLUDE 'D0$PARAMS:L2JETS.PARAMS'
      INCLUDE 'D0$PARAMS:JAUX.PARAMS'
      INCLUDE 'D0$PARAMS:ESUM.PARAMS'
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:L2LINK.INC'               ! L2 link common
      INCLUDE 'D0$INC:L2JETS_CONT.INC'
      INCLUDE 'D0$INC:L2JETS_CHARS.INC'         ! Character variables
      INCLUDE 'D0$INC:L2JETS_HOT.INC'
      INCLUDE 'D0$INC:L2JETS_PAR.INC'
      INTEGER ICAND,IP, IETA_USED, IPHI_USED, ILAY_USED, IER, ICONE
      INTEGER BITFLAG
      INCLUDE 'D0$INC:TTEDGE.INC'       ! Trigger to Readout eta array
      INTEGER ESUM_CONESIZE_TO_JET
C&IF VAXVMS,VAXELN
      INTEGER IBSET
C&ELSE
C&      EXTERNAL IBSET
C&ENDIF
      REAL ABSETA,L2CALIB(3),ETATT,PHITT, L2_VERT
      REAL CL2_ET_CORR, ETCOR, ETA_DET
      REAL R_USED(3), THETA, ZV_USED
C----------------------------------------------------------------------

      IF ( USE_CL2 ) THEN
        L2CALIB(1) = L2JCAL_CEN_CL2
        L2CALIB(2) = L2JCAL_CRA_CL2
        L2CALIB(3) = L2JCAL_END_CL2
      ELSE
        L2CALIB(1) = L2JCAL_CEN
        L2CALIB(2) = L2JCAL_CRA
        L2CALIB(3) = L2JCAL_END
      END IF
C
C---Cycle through the candidates: choose those with correct bit set and
C---that have flag = L2J_ENG
C
      DO 500, ICAND = 1 , NJTHOT
      IP = LJAUX + (NOW_IND_PARAM - 1)*NREP_JAUX*NJTHOT + (ICAND - 1)*
     &  NREP_JAUX
      IF ( IQ( IP + PJEVT) .NE. L2J_ENG) GOTO 500    ! only do analyzed jets
C
C---Determine and use calibration factors:
C
      ABSETA = ABS( Q( IP + PJETA ) )            ! Get absolute value of
C                                                ! eta
      IF (ABSETA .LE. .7) THEN
        Q(IP + PJET) = Q(IP+PJET)*L2CALIB(1)
        Q(IP + PJEMFR) = Q(IP+PJEMFR)*L2CALIB(1)
        Q(IP + PJETASIZ) = Q(IP+PJETASIZ)*L2CALIB(1)
        Q(IP + PJPHISIZ) = Q(IP+PJPHISIZ)*L2CALIB(1)
      ELSE IF (ABSETA .LE. 1.3) THEN
        Q(IP + PJET) = Q(IP+PJET)*L2CALIB(2)
        Q(IP + PJEMFR) = Q(IP+PJEMFR)*L2CALIB(2)
        Q(IP + PJETASIZ) = Q(IP+PJETASIZ)*L2CALIB(2)
        Q(IP + PJPHISIZ) = Q(IP+PJPHISIZ)*L2CALIB(2)
      ELSE
        Q(IP + PJET) = Q(IP+PJET)*L2CALIB(3)
        Q(IP + PJEMFR) = Q(IP+PJEMFR)*L2CALIB(3)
        Q(IP + PJETASIZ) = Q(IP+PJETASIZ)*L2CALIB(3)
        Q(IP + PJPHISIZ) = Q(IP+PJPHISIZ)*L2CALIB(3)
      END IF
C
C---Correct ET and EMET due to vertex
C
C      CALL L2JETS_JAUX_ET_CORR(IP)
C
C      IETA_USED = TTEDGE( IQ(IP+PJIETA) )   ! Smallest RO tower in this TT
      IETA_USED = INT( Q(IP+PJETA)/.1 ) + SIGN( 1., Q(IP+PJETA) )
      IETA_USED = MAX( -NETAL, MIN( NETAL, IETA_USED ) )
      IPHI_USED = 1                         ! Always exists
      ILAY_USED = 11
      IF ( ABS(IETA_USED) .EQ. NETAL ) ILAY_USED = 13 ! No layer 11 here
C
      ETCOR = 1.
      IF ( IETA_USED .NE. 0) ETCOR = CL2_ET_CORR( IETA_USED )
      CALL CELXYZ( IETA_USED, IPHI_USED, ILAY_USED, R_USED(1),
     &  R_USED(2), R_USED(3), IER )
      ZV_USED = L2_VERT()
C
C: Get correct PHYSICS eta
C
      ETA_DET = Q(IP+PJETA)
      IF ( IER .EQ. 0 ) CALL ETA_ZCORR( R_USED, ZV_USED, Q(IP+PJETA),
     &  THETA )
C
C: Make ET correction
C
      Q( IP + PJET ) = Q( IP + PJET )*ETCOR
      Q( IP + PJEMFR)= Q( IP + PJEMFR)*ETCOR
      Q( IP + PJETASIZ ) = Q( IP + PJETASIZ )*ETCOR
      Q( IP + PJPHISIZ)= Q( IP + PJPHISIZ)*ETCOR
C
C---Enter in the ESUM bank for 'general' jet. Pass integerized cone_size*10
C---position is at center of trigger tower
C---12/15/92: Now we will pass the cone size as a bit mask. For example:
C---cone of  R=.7 --> set bit 7, R=.3 --> set bit 3. The ESUM package will
C---OR these results so that we can tell what went into an  ESUM object
C
      ICONE = 1 + ICON_ENG( NOWPARAM )*2
      IF ( ICONE .LT. 1 .OR. ICONE .GT. 15 ) THEN
        ICONE = MAX( 1, MIN( ICONE, 15 ) )
        CALL ERRMSG('Unexpected cone size','L2JETS_CALIB',
     &    ' Cone size stored as .1 or 1.5','W')
      ENDIF
      BITFLAG = 0
      BITFLAG =IBSET( BITFLAG, ICONE )
C
      ETATT = (IQ( IP + PJIETA)-.5)*.2
      IF (ETATT.LT.0) ETATT = ETATT + .2
      PHITT = (IQ( IP + PJIPHI)-.5)*(TWOPI/32.)
      CALL ESUMFL('FILT',ID_JET, Q( IP + PJET),
     &  Q(IP+PJETA),ETATT,PHITT, BITFLAG)
C---Enter in the ESUM bank for 'specific cone sized' jet
      CALL ESUMFL('FILT',ID_JET_1-1+NOW_IND_PARAM, Q( IP + PJET),
     &  Q(IP+PJETA),ETATT,PHITT, BITFLAG)

C---Now we must flag this jet as having been calibrated:
      IQ(IP + PJEVT) = L2J_CALIB

  500 CONTINUE
  999 RETURN
      END
