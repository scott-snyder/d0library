      SUBROUTINE DUMP_MASS_EVENT
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : PRINT OUT INTERESTING EVENT QUANTITIES
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created   9-AUG-1993   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:KINEQ.INC'
      INCLUDE 'D0$INC:EVENT_QUAN1.INC'
      INCLUDE 'D0$INC:BTAG_ISAJ.INC'
      INTEGER PRUNIT,SSUNIT
      LOGICAL MONTE_CARLO_DATA
C
      INTEGER K
      LOGICAL first
      SAVE first
      DATA first / .true. /
C
      LOGICAL READ_RCP
      INTEGER IER
C----------------------------------------------------------------------
      IF( first ) THEN
        first = .false.
        CALL EZPICK('TOP_MASS_RCP')
        CALL EZGET('READ_EVENT_FROM_RCP',READ_RCP,IER)
        CALL EZRSET
      ENDIF
C
      IF(READ_RCP)RETURN  !ONLY PRINT IF NOT RCP QUANS
C
      PRUNIT=SSUNIT()
C
      WRITE(PRUNIT,1)
    1 format(//,' Particles used for mass fit ')
      CALL VEC_PRINT(PRUNIT,'ELECTRONS',P24_ELECTRON,NE,NELEC)
      CALL VEC_PRINT(PRUNIT,'PHOTONS',P18_PHOTON,NP,NPHO)
      CALL VEC_PRINT(PRUNIT,'MUONS',P23_MUON,NM,NMUO)
      CALL VEC_PRINT(PRUNIT,'JETS',P25_JETS,NJ,NJETS)
      WRITE(PRUNIT,2)P2_NEUT
    2 FORMAT(1X,'  NEUTRINO',2F10.3)
C
      IF ( MONTE_CARLO_DATA() ) THEN
C WRITE BTAG INFO
        WRITE(PRUNIT,3)NTAG,(JET_TAGGED(K),K=1,NTAG)
    3   FORMAT(' NUMBER OF JETS BTAGGED ',I6,' TAGGED JETS ',2I)
C
        WRITE(PRUNIT,4)(DIF_R(K),DIF_ET(K),NAME_TAG(K),K=1,NTAG)
    4   FORMAT(' DIFF_R,DIFF_ET,NAME_TAG ',2F12.3,2X,A2/)
C
      ENDIF
C
  999 RETURN
      END
