      REAL FUNCTION NOISY_HIS (ETA,PHI,LYR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Noise generator according to pedestal
C-   distributions.
C-
C-   Returned value  : A random number distributed according to the
C-                     contens of a the pedestal distribution for a
C-                     given channel (eta, phi, lyr)
C-
C-   Inputs  :         ETA    Eta of channel
C-                     PHI    Phi of channel
C-                     LYR    Layer of channel
C-                     requires that the logical PEDS_HIS_FILE point
C-                     to an HBOOK file with noise distributions
C-
C-   Outputs :         none
C-   Controls:         none
C-
C-   Created  14-OCT-1991   Lars Rasmussen
C-   Modified  9-JUL-1993   Ian Adam
C-                          add check on HEXIST before HRNDM1
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL HEXIST
      EXTERNAL HEXIST
      REAL HRNDM1
      EXTERNAL HRNDM1
      INTEGER NOI_HIST_ID
C
      INTEGER ETA, PHI, LYR
      LOGICAL FIRST /.TRUE./
      INTEGER HISID
C----------------------------------------------------------------------
      IF (FIRST) THEN
        CALL HCDIR ('//PAWC',' ')
        CALL HMDIR ('PEDS',' ')
        CALL HCDIR ('PEDS',' ')
        CALL HRGET (0,'PEDS_HIS_FILE',' ')
        FIRST = .FALSE.
      END IF
C
      CALL HCDIR ('//PAWC/PEDS',' ')
      HISID = NOI_HIST_ID(ETA,PHI,LYR)
      IF (HEXIST(HISID)) THEN
        NOISY_HIS=HRNDM1(HISID)
      ELSE
        NOISY_HIS=0.0
      ENDIF
      CALL HCDIR (CHAR(92),' ')
  999 RETURN
      END
