C----------------------------------------------------------------------
      FUNCTION QCD_UPK_PHO()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-
C-   Returned value  :
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  28-DEC-1992   Lars Rasmussen (based on Jaehoon Yu's
C-                          QCD_PHOTONS)
C-   Updated   24-SEP-1993  Andrew G. Brandt save new words
C-   Updated  31-MAR-1995   Bob Hirosky  fix nphot bug (nphot .le. max_pho)
C-
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:QCD_PHO.INC/LIST'
      LOGICAL QCD_UPK_PHO
C
      INTEGER   I,IER,IPH
      REAL      E(4),ET,SIG(4),TET,ETA,PHI,CONF(5),DIST
      INTEGER   TNUM
C----------------------------------------------------------------------
      QCD_UPK_PHO = .TRUE.
C
C- Initialize
C
      NPHOT = -999
      DO I = 1,3
         PH_ET(I)    = -999.
         PH_ETA(I)   = -999.
         PH_PHI(I)   = -999.
         PH_COR(I)  = -999.
         PH_ISO(I)  = -999.
         PH_HMAT(I)  = -999.
      END DO
C
C- Get number of Photon banks
C
      CALL GTPPHO_TOTAL( NPHOT, IER )
      IF ( IER .NE. 0 .OR. NPHOT .LE. 0 ) RETURN
      NPHOT = MIN ( NPHOT, MAX_PHO )
C
C- Fill the QCD_PHO common block ( take the three higest ET photons )
C
      DO 10 IPH = 1,NPHOT
         CALL GTPPHO ( IPH,E,ET,SIG,TET,ETA,PHI,CONF,DIST,TNUM,IER )
         IF ( IER .LT. 0 ) GOTO 10
         DO I = 1,3
            IF ( ET .GT. PH_ET( I ) ) THEN
               IF ( I .LT. 3 ) THEN
                  PH_ET(I+1)    = PH_ET(I)
                  PH_ETA(I+1)   = PH_ETA(I)
                  PH_PHI(I+1)   = PH_PHI(I)
                  PH_ISO(I+1)   = PH_ISO(I)
                  PH_COR(I+1)   = PH_COR(I)
                  PH_HMAT(I+1)  = PH_HMAT(I)
               END IF
               PH_ET(I)   = ET
               PH_ETA(I)  = ETA
               PH_PHI(I)  = PHI
               PH_COR(I) = CONF(2)
               PH_ISO(I) = CONF(3)
               PH_HMAT(I) = SIG(4)
               GOTO 10
            END IF
         END DO
10    CONTINUE
  999 RETURN
      END
