      SUBROUTINE CORRECT_JETS_WIDTH(CONE_SIZE_IN, JET_ET, DET_ETA,
     &  JET_WIDTH, JET_EMF, CORRECTION_FACTOR )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Determine relative correction based on jet width
C-
C-   Inputs  :CONE_SIZE_IN [R]: cone size used, -1 for NN
C-             JET_ET    [R]  : jet transverse energy
C-             DET_ETA   [R]  : Detector eta
C-             JET_WIDTH [R]  : jet RMS width
C-             JET_EMF   [R]  : jet emfraction
C-
C-   Outputs :
C-    CORRECTION_FACTOR  [R]  : relative correction factor found
C-   Controls:
C-
C-   Created   9-MAY-1995   Richard V. Astur
C-   Updated  Sep-02-1995   Bob Kehoe   -- change data statements to RCP
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL ETA_SEC1, ETA_SEC2
      SAVE ETA_SEC1, ETA_SEC2
      REAL CONE_SIZE_IN
      REAL CONE_SIZE, DET_ETA, JET_WIDTH, JET_EMF, JET_ET
      REAL CORRECTION_FACTOR, CORRECTION_FACTOR2
      REAL WIDTH_USED
      INTEGER IPARA, I, ISEC, ISEC1, ISEC2, icone
      INTEGER LRCP, IER
      LOGICAL OK
C: Width correction parametrization
      REAL CORRSEC(2)
      REAL WIDTH_AVER(5,2), WIDTH_CUTOFF(5,2), WIDTH_A0(5,2)
      REAL WIDTH_A1(5,2), WIDTH_A2(5,2)
      SAVE WIDTH_AVER, WIDTH_CUTOFF, WIDTH_A0, WIDTH_A1, WIDTH_A2
      REAL ETPARA(5,3)
      SAVE ETPARA
      LOGICAL FIRST
      DATA FIRST /.TRUE./
C----------------------------------------------------------------------
C: Statement function
      INTEGER IETBIN, IDET
      REAL CORFAC
      LOGICAL OVER
      CORFAC(IETBIN,IDET)  = (WIDTH_A0(IETBIN,IDET) +
     &  WIDTH_USED*WIDTH_A1(IETBIN,IDET)
     &  + WIDTH_A2(IETBIN,IDET)*WIDTH_USED**2)/WIDTH_AVER(IETBIN,IDET)
C----------------------------------------------------------------------
      CORRECTION_FACTOR = 1.00
      CONE_SIZE = MAX(CONE_SIZE_IN, 0.3)

C
C: Initialize
C
      IF (first) THEN
        CALL ezloc('QCD_JET_CORRECTION_RCP',lrcp)
        ok = lrcp .GT. 0
        IF (.NOT. ok) THEN
          CALL inrcp('QCD_JET_CORRECTION_RCP',ier)
          IF (ier.EQ.0) CALL ezpick('QCD_JET_CORRECTION_RCP')
          IF (ier.EQ.0) CALL ezerr(ier)
          IF (ier.NE.0) THEN
            CALL errmsg('RCP not found','correct_jets_width',
     &        'QCD_JET_CORRECTION_RCP','F')
          ENDIF
          CALL ezrset
        ENDIF
        CALL ezpick('QCD_JET_CORRECTION_RCP')
        CALL ezerr(ier)
        IF (ier.EQ.0) THEN    ! *** read in RCP parameters ***
          IF (IER.EQ.0) CALL ezget('ETA_SEC1',eta_sec1,ier)
          IF (IER.EQ.0) CALL ezget('ETA_SEC2',eta_sec2,ier)
          IF (ier.EQ.0) CALL ezget('ETPARA',etpara,ier)
          if (ier.EQ.0) CALL ezget('WIDTH_AVER',width_aver,ier)
          if (ier.EQ.0) CALL ezget('WIDTH_CUTOFF',width_cutoff,ier)
          if (ier.EQ.0) CALL ezget('WIDTH_A0',width_a0,ier)
          if (ier.EQ.0) CALL ezget('WIDTH_A1',width_a1,ier)
          if (ier.EQ.0) CALL ezget('WIDTH_A2',width_a2,ier)
          CALL ezrset
          IF (ier.NE.0) THEN
            CALL errmsg('RCP error','correct_jets_width',
     &        'Read error:abort ','F')
            ier = -3
            GOTO 999
          ENDIF
        ELSE
          CALL errmsg('NO QCD_JET_CORRECTION_RCP',
     &      'correct_jets_width','NO RCP file to work with','F')
          ier = -2
          GOTO 999
        ENDIF
        first = .false.
      ENDIF

C
C: Set algorithm parameter
C
      ICONE = 1
      if (cone_size .le. .6 ) icone = 2
      if (cone_size .le. .4 ) icone = 3
C
C: Determine ET bin
C
      IPARA = 5
      OVER  = .FALSE.
      DO I= 4, 1, -1
        IF ( JET_ET .LE. ETPARA(I,icone) ) IPARA = I
      ENDDO
      IF ( IPARA .GE. 5 ) THEN
        IPARA = 4
        OVER = .TRUE.
      ENDIF
C
C: Which section of the calorimeter
C
      IF ( ABS(DET_ETA) .LE. ETA_SEC1 ) THEN
        ISEC1 = 1
        ISEC2 = 1
      ELSEIF ( ABS(DET_ETA) .LE. ETA_SEC2 ) THEN
        ISEC1 = 1
        ISEC2 = 2
      ELSE
        ISEC1 = 2
        ISEC2 = 2
      ENDIF
C
C: Interpolate between forward and central for region in between (if needed)
C
      DO ISEC = ISEC1, ISEC2

        WIDTH_USED  = MIN( WIDTH_CUTOFF(IPARA,ISEC),
     &                   MAX( .1, JET_WIDTH*.7/CONE_SIZE ))

        CORRECTION_FACTOR  = CORFAC(IPARA,ISEC)

C
C: Interpolate in ET if necessary  (we never use the 5th bin even though its 
C:                                  there)
C
        IF ( IPARA .GT. 1 .AND. ( .NOT. OVER)  ) THEN
          WIDTH_USED  = MIN( WIDTH_CUTOFF(IPARA-1,ISEC), 
     &      MAX( .1,JET_WIDTH*.7/cone_size) )

          CORRECTION_FACTOR2  = CORFAC(IPARA-1,ISEC)
C
C: Interpolate
C
          CORRECTION_FACTOR = CORRECTION_FACTOR +
     &      (CORRECTION_FACTOR2-CORRECTION_FACTOR)*
     &      (JET_ET-ETPARA(IPARA,icone))
     &      /(ETPARA(IPARA-1,icone)-ETPARA(IPARA,icone))
        ENDIF
        CORRSEC(ISEC) = CORRECTION_FACTOR
      ENDDO
C
C: Final interpolation
C
      IF ( ISEC1 .EQ. ISEC2 ) THEN
      ELSE
        CORRECTION_FACTOR = CORRSEC(1) + (CORRSEC(2)-CORRSEC(1)) *
     &    (ABS(DET_ETA)-ETA_SEC1)/(ETA_SEC2-ETA_SEC1)
      ENDIF
      CORRECTION_FACTOR = 1./CORRECTION_FACTOR

  999 RETURN
      END
