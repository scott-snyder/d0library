      SUBROUTINE CL2TEST_ETSUM(PATH,ETALO,ETAHI,PHILO,PHIHI,NPHI,
     &  MXLYR,ETSUM)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Sum the ET in a square region
C-
C-   Inputs  : PATH             [C]     'RECO' or 'FILT' where to get data
C-             ETALO,ETAHI      [I]     eta boundaries (IETAC)
C-             PHILO,PHIHI      [I]     phi boundaries
C-             NPHI             [I]     number of phi boundaries
C-             MXLYR            [I]     last layer to sum over
C-      link areas for L2CAEP, and offline CAEH
C-   Outputs : ETSUM            [F]     sum of ET(i) in region
C-   Controls:
C-
C-   Created  14-JUN-1991   James T. Linnemann
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      CHARACTER*4 PATH                  ! where to look for data
      INTEGER ETALO,ETAHI,PHILO(2),PHIHI(2),NPHI,MXLYR ! region definition
      REAL    ETSUM                     ! the sum of ET there
      INCLUDE 'D0$INC:CL2_LINK.INC'     ! pointers to CL2xxx banks
      INCLUDE 'D0$INC:CL2TEST_LINK.INC' ! CAHITS links
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$PARAMS:L1PHP.PARAMS'
      INCLUDE 'D0$INC:PTCAEP.INC'
      INCLUDE 'D0$CALOR_FILTER$SOURCE:PTCAEP2.DEF'
      INTEGER LYR,IBOUND,PHI,ETA,IPOINT
C----------------------------------------------------------------------
      CALL PATHST(PATH)
      ETSUM = 0
      IF (PATH.EQ.'RECO') THEN
        IF (LCAEH.GT.0) THEN
          DO LYR = 1,MXLYR
            DO IBOUND = 1,NPHI
              DO PHI = PHILO(IBOUND),PHIHI(IBOUND)
                DO ETA = ETALO,ETAHI
                  IF (PTCAEP(ETA,PHI,LYR).GT.0) THEN
                    IPOINT = (PTCAEP(ETA,PHI,LYR)-1)*IQ(LCAEH+2)
                    ETSUM = ETSUM + Q(LCAEH+IPOINT+8)
                  ENDIF
                ENDDO
              ENDDO
            ENDDO
          ENDDO
        ENDIF
      ELSE
        IF (L2CAEP.GT.0) THEN
          DO ETA = ETALO,ETAHI
            DO IBOUND = 1,NPHI
              DO PHI = PHILO(IBOUND),PHIHI(IBOUND)
                DO LYR = 1,MXLYR
                  IF (PTCAEP2(LYR,PHI,ETA).GT.0) THEN
                    IPOINT = (PTCAEP2(LYR,PHI,ETA)-1)*IQ(L2CAEP+2)
                    ETSUM = ETSUM + Q(L2CAEP+IPOINT+5)
                  ENDIF
                ENDDO
              ENDDO
            ENDDO
          ENDDO
        ENDIF
      ENDIF
  999 RETURN
      END
