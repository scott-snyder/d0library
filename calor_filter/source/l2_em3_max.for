      SUBROUTINE L2_EM3_MAX(TGETA,TGPHI,ETA3,PHI3,LYR3)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : From Hot trigger tower to find Hot EM3
C-
C-   Inputs  : TGETA,TGPHI Trigger Tower eta, phi of candidate
C-
C-   Outputs : ETA3,PHI3,LYR3   offline coordinates of hottest EM3 cell
C-                              if ETA3 = 0, no energy was found in EM3
C-   Controls: None
C-
C-   Created 30-DEC-1989   Yi  Xia
C-
C-   Updated  29-JAN-1992   James T. Linnemann  use ring routines
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'       ! zebra main store /ZEBCOM/
      INCLUDE 'D0$INC:ZLINKC.INC'       ! zebra link area to store links
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$CALOR_FILTER$SOURCE:PTCAEP2.DEF'
      INTEGER TGETA,TGPHI,LYR3,ETA3,PHI3,ETA,PHI,LYR,NR
      INTEGER IPOINT,L2CAEP,GZCAEP,TGETA1,TGPHI1,LYMAX
      INTEGER ETALO,ETAHI,PHILO(2),PHIHI(2),NPHI
      REAL ET,EM3MAX
      CHARACTER*80 MSG
C----------------------------------------------------------------------
C
C...unpack in ring size = 1TT (i.e. 3X3 TT) around candidate
      CALL CL2_RING12(TGETA,TGPHI,1,ETALO,ETAHI,PHILO,PHIHI,NPHI)
      CALL CL2_ROTOW_ETNOM(ETALO,ETAHI,PHILO(1),PHIHI(1))
      IF (NPHI.GT.1)
     &    CALL CL2_ROTOW_ETNOM(ETALO,ETAHI,PHILO(2),PHIHI(2)) !wraparound
C
C...look for max EM3 in a trigger tower (2x2 or 1x1 Readout Towers) (ringsize 0)
      CALL CL2_RING12(TGETA,TGPHI,0,ETALO,ETAHI,PHILO,PHIHI,NPHI)
C
      EM3MAX = -999999.
      ETA3 = 0  ! initialize in case this was L1 noise and zero suppressed
      PHI3 = PHILO(1)
      LYR3 = LYEM3A
      LYMAX = 6
      IF(IABS(ETALO).GE.27) LYMAX = 3  !no EM3 subdivision
      L2CAEP = GZCAEP()
      IF (L2CAEP.GT.0) THEN
        NR = IQ(L2CAEP+2)
        DO ETA = ETALO,ETAHI
          DO PHI = PHILO(1),PHIHI(1)  !no wraparound in a single TTower
            DO LYR = LYEM3A,LYMAX
              IF (PTCAEP2(LYR,PHI,ETA).GT.0) THEN
                IPOINT = (PTCAEP2(LYR,PHI,ETA)-1)*NR
                ET = Q(L2CAEP+IPOINT+5)
                IF (ET .GT. EM3MAX) THEN
                  EM3MAX = ET
                  ETA3 = ETA
                  PHI3 = PHI
                  LYR3 = LYR
                ENDIF
              ENDIF
            ENDDO
          ENDDO
        ENDDO
      ENDIF
      IF (EM3MAX .EQ. -999999.) THEN
C        WRITE(MSG,100) TGETA,TGPHI
C  100   FORMAT('No Data found at L1 ETA =',I6,' L1 PHI =',I6)
C        CALL ERRMSG('NO_EM_E','L2_EM3_MAX',MSG,'W')
      ELSEIF (EM3MAX .LE. 0) THEN
C        WRITE(MSG,200) TGETA,TGPHI,ETA3,PHI3
C  200   FORMAT('Only - E at L1ETA,PHI=',2I3,
C     &    ' L2=',2I3)
C        CALL ERRMSG('NEG_EM_E','L2_EM3_MAX',MSG,'W')
        ETA3 = 0  ! signal unacceptable candidate
      ENDIF
  999 RETURN
      END
