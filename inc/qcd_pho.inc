C----------------------------------------------------------------------
C-
C-   Created   28-DEC-1992  Lars Rasmussen (based on QCD_PHO_NTUP.INC)
C-   Updated   24-SEP-1993  Andrew G. Brandt save new words
C-   Updated   31-OCT-1994  Andrew G. Brandt convert to columnwise
C----------------------------------------------------------------------
      INTEGER MAX_PHO
      PARAMETER (MAX_PHO=3)
      COMMON /QCD_PHO/ NPHOT,PH_ET(MAX_PHO),PH_ETA(MAX_PHO),
     &                    PH_PHI(MAX_PHO),PH_COR(MAX_PHO),
     &                    PH_ISO(MAX_PHO),PH_HMAT(MAX_PHO)
C
      REAL    PH_ET,PH_ETA,PH_PHI,PH_COR,PH_ISO,PH_HMAT
      INTEGER NPHOT
      CHARACTER*255 QCD_PHO_TAG
      DATA QCD_PHO_TAG /'NPHOT[0,3],PH_ET(NPHOT):R,
     &    PH_ETA(NPHOT):R,PH_PHI(NPHOT):R,PH_COR(NPHOT):R,
     &    PH_ISO(NPHOT):R,PH_HMAT(NPHOT):R'/
