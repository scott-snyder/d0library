      SUBROUTINE L2JETSFL
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fill JETS bank with JETS that passed the cuts
C-                         of the current parameter set. Hang the JTSH
C-                         bank under the first JETS bank.
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  22-NOV-1989   Richard V. Astur
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'       ! main ZEBRA store
      INCLUDE 'D0$PARAMS:L1PHP.PARAMS'
      INCLUDE 'D0$PARAMS:L2JETS.PARAMS'
      INCLUDE 'D0$PARAMS:JAUX.PARAMS'
      INCLUDE 'D0$INC:L2JETS_CONT.INC'  ! JETS control common
      INCLUDE 'D0$INC:L2JETS_CHARS.INC'         ! Character variables
      INCLUDE 'D0$INC:L2JETS_HOT.INC'   ! # of hot towers
      INCLUDE 'D0$INC:L2JETS_PAR.INC'   ! parameter 
      INCLUDE 'D0$INC:L2LINK.INC'       ! L2 link common
      INTEGER ICAND,IP,I
C----------------------------------------------------------------------
C
C---BKJETS will make either the FIRST or simply another JETS bank in the
C---linear structure. It then returns the link to us.
      CALL BKJETS(LJETS)

C---Under the first JETS bank, we will hang the JTSH bank which describes
C---some algorithm and parameter dependent information.
      CALL BKJTSH(LJETS,LJTSH)

C---Fill the first JETS bank and the first JTSH bank.
      DO ICAND = 1,NJTHOT
        IP = LJAUX + (NOW_IND_PARAM-1)*NREP_JAUX*NJTHOT + (ICAND-1)*
     &    NREP_JAUX
        IF ( IQ( IP + PJEVT ) .GT. 0 ) THEN    ! this is the first
C---First JETS:
          Q( LJETS + 9) = Q(IP + PJETA)         ! eta
          Q( LJETS + 7) = Q(IP + PJPHI)         ! phi
          Q( LJETS + 6) = Q(IP + PJET )         ! ET
C---Now JTSH:
          IQ( LJTSH + 2 ) = 4           ! Algorithm number: L2JETS
          Q( LJTSH + 11) = ICON_ENG( NOWPARAM )*.2 + .1
          Q( LJTSH + 12) = ETMIN( NOWPARAM )
          Q( LJTSH + 13) = ICON_CEN( NOWPARAM )*.2 + .1
          Q( LJTSH + 14) = FLOAT( NJET_COUNT( NOWPARAM ) )

          GOTO 500                      ! kick out of do loop: only want
C                                       ! 1
        END IF
      END DO
  500 CONTINUE                      ! Now we have 1. Get rest.
C---Well, we THINK we have one. It might be that none was found. In that
C---case...
      IF (ICAND .GT. NJTHOT) THEN
        CALL ERRMSG('No jets? tool passed!','L2JETSFL',
     &        ' No JETS to write even though Tool passed!!!!','W')
        RETURN
      END IF

C---Write the rest.
      DO I = ICAND+1,NJTHOT
        CALL BKJETS( LJETS )
        IP = LJAUX + (NOW_IND_PARAM-1)*NREP_JAUX*NJTHOT + (ICAND-1)*
     &    NREP_JAUX
        IF ( IQ( IP + PJEVT ) .GT.  0 ) THEN    ! this is the first
          Q( LJETS + 9) = Q(IP + PJETA)         ! eta
          Q( LJETS + 7) = Q(IP + PJPHI)         ! phi
          Q( LJETS + 6) = Q(IP + PJET )         ! ET
        END IF
      END DO

C
C  Bank Name : JTSH
C  Author    : NICHOLAS HADLEY
C  Date      : 06-JULY-1989
C  Tree description : PROC_ZEBANKS
C
C  Bank description : Jet shape parameters for a given jet
C
C     LQ     Q/IQ
C-----------------------------------------------------------------------
C      0          Next   link to none
C     +1          Up     link to JETS
C     +2          Origin link to JETS
C.......................................................................
C             -5         Bank number
C             -4         Bank name, 'JTSH'
C             -3         NL = 0
C             -2         NS = 0
C             -1         ND = 20
C              0         Status
C             +1     I   Bank version (=1)
C             +2     I   ALGORITHM NUMBER, 1=FIXED CONE, 2=NEAREST NEIGHBOR
C                                          3=BOOSTED SPHERICITY 4=L2JETS
C             +3     F   RMS ETA WIDTH
C             +4     F   RMS PHI WIDTH
C             +5     F   FRACTION OF EM ET = EM_ET/TOTAL_ET
C             +6     F   SPARE
C             +7     F   SPARE
C             +8     F   SPARE
C             +9     F   SPARE
C       ALGORITHM DEPENDENT QUANTITIES
C             +10    I   FOR FIXED CONE,ENERGY SHARED (0=NO, ANYTHING ELSE YES)
C             +11    F                  CONE RADIUS -->ICON_ENG
C             +12    F                  MIN JET ET  -->ET cut
C             +13    F   SPARE          CORE RADIUS -->ICON_CEN
C             +14    F   SPARE          JET COUNT CUT
C             +15    F   SPARE
C             +16    F   SPARE
C             +17    F   SPARE
C             +18    F   SPARE
C             +19    F   SPARE
C             +20    F   SPARE
C Note:- 1-9 ARE ALGORITHM INDEPENDENT QUANTITIES IE THESE SHOULD BE THE
C SAME FOR ALL ALGORITHMS
C        10-20 ARE RESERVED FOR ALGORITHM DEPENDENT QUANTITIES
C        REPEAT THESE FOR DIFFERENT ALGROITHMS
  999 RETURN
      END
