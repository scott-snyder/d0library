      LOGICAL FUNCTION NP_LQ_2EM ()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Selects 2 high Et electrons/photons
C-
C-   Returned value: .TRUE. if the event satisfies the ELQ cuts
C-   Inputs  : none
C-   Outputs : none
C-
C-   Controls: none
C-
C-   ENTRY  NP_LQ_2EM_EOJ performs and end-of-job summary (dummy for now).
C-
C-   Created   9-OCT-1992   K. Wyatt Merritt
C-   Updated  18-DEC-1992   Marc Paterno  Made to conform with the standard
C-                                        (18 Dec 1992) format.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      LOGICAL  NP_LQ_2EM_EOJ
C
C
      LOGICAL FIRST
C
      INTEGER I, IER
      INTEGER IOS
      INTEGER NUM_ELC, IE, NUM_TRKS, NUM_PHO
      INTEGER N_ELE_ET_CUT
      REAL    SUMMARY(20)
C
      REAL E_ELE, SIG_ELE, TH_ELE, ETA_ELE, PHI_ELE, ET_ELE (10)
      REAL CONE_NRG (5), DIST
      REAL E_PHO, SIG_PHO, TH_PHO, ETA_PHO, PHI_PHO, ET_PHO (10)
      REAL ELE_ET_CUT
C
      DATA FIRST /.TRUE./
      SAVE FIRST
C----------------------------------------------------------------------
C
      IF ( FIRST ) THEN
        FIRST=.FALSE.
C
        CALL INRCP ('NP_LQ_2EM_RCP', IER)

        IF (IER.EQ.0) THEN
          CALL EZPICK ('NP_LQ_2EM_RCP')
          CALL EZGET ('ELE_ET_CUT', ELE_ET_CUT, IER)
          CALL EZRSET
        ELSE
          CALL ERRMSG ('No NP_LQ_2EM_RCP', 'NP_LQ_2EM_EVENT',
     &        'Could not find NP_LQ_2EM_RCP', 'F')
        ENDIF                           ! if ier .eq. 0
      ENDIF                             ! if first
C------------------------------------------------------------
      NP_LQ_2EM = .FALSE.
      IF (LHEAD.EQ.0) GOTO 999                ! no filtering
C
      IOS=MOD (IQ (LHEAD+1), 1000)
      IF (IOS.LT.5) GOTO 999     ! not an event record
C
      N_ELE_ET_CUT = 0
      DO IE = 1, 10
        ET_ELE (IE) = 0.
      ENDDO
      CALL GTPELC_TOTAL (NUM_ELC, IER)
      IF (IER.NE.0 .OR. NUM_ELC.EQ.0) GO TO 5
      DO IE = 1, MIN0 (10, NUM_ELC)
        CALL GTPELC (IE, E_ELE, ET_ELE (IE), SIG_ELE, TH_ELE, ETA_ELE,
     &    PHI_ELE, CONE_NRG, DIST, NUM_TRKS, IER)
        IF (ET_ELE (IE) .GE. ELE_ET_CUT) N_ELE_ET_CUT = N_ELE_ET_CUT + 1
      ENDDO
C
    5 CALL GTPPHO_TOTAL (NUM_PHO, IER)
      IF (IER.NE.0 .OR. NUM_PHO.EQ.0) GO TO 6
      DO IE = 1, MIN0 (10, NUM_PHO)
        CALL GTPPHO (IE, E_PHO, ET_PHO (IE), SIG_PHO, TH_PHO, ETA_PHO,
     &    PHI_PHO, CONE_NRG, IER)
        IF (ET_PHO (IE) .GE. ELE_ET_CUT) N_ELE_ET_CUT = N_ELE_ET_CUT + 1
      ENDDO
C
    6 NP_LQ_2EM = N_ELE_ET_CUT .GE. 2
C
  999 RETURN
C----------------------------------------------------------------------
C#######################################################################
      ENTRY NP_LQ_2EM_EOJ ( SUMMARY )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : End of job summary for NP_LQ_2EM filter.  The array
C-   SUMMARY (20 reals) may contain whatever accounting information is of
C-   interest.
C-
C-   Inputs  : none
C-   Outputs : SUMMARY [R(20)] real array of summary results (dummy)
C-   Controls: none
C-
C-   Created  18-DEC-1992   Marc Paterno
C-
C----------------------------------------------------------------------
      NP_LQ_2EM_EOJ = .TRUE.
      CALL VZERO (SUMMARY, 20)
      RETURN
      END
