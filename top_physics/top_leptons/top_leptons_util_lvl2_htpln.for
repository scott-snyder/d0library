      SUBROUTINE TOP_LEPTONS_UTIL_LVL2_HTPLN(WAM_HIT,SAM_HIT,IOK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Test available EF plane information
C-                         against muon level 2 plane cuts. Returns
C-                         IOK=1/-1 for pass/fail
C-
C-   Inputs  : 
C-             WAM_HIT(6) : Wamus hit plane information - planes 
C-                          available to fit (not necessarily used).
C-             SAM_HIT(3) : Samus hit plane info.
C-   Outputs : 
C-   Controls: 
C-
C-   Created  28-JAN-1993   Stephen J. Wimpenny
C-   Modified 22-Mar-1993   Routine name changed for library compatibility
C-                          (was Muon_Ef_Lvl2_Htpln)
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER WAM_HIT(6),SAM_HIT(3),NA,NB,NC,NTOT,IOK
C
      IOK=1
C
      NA=WAM_HIT(1)+WAM_HIT(5)+SAM_HIT(1)
      NB=WAM_HIT(2)+WAM_HIT(6)+SAM_HIT(2)
      NC=WAM_HIT(3)+WAM_HIT(4)+SAM_HIT(3)
      NTOT=NA+NB+NC
C
      IF(NA.EQ.3.AND.NB.LE.1.AND.NC.LE.1) GO TO 10
      IF(NA.EQ.2.AND.NB.LE.1.AND.NC.LE.1) GO TO 10
      IF(NA.EQ.1.AND.NB.LE.1.AND.NC.LE.1) GO TO 10
      IF(NTOT.LE.5.AND.NA.LE.3) GO TO 10
      RETURN
   10 CONTINUE
C
C *** Failed Cuts
C
      IOK=-1
C----------------------------------------------------------------------
  999 RETURN
      END
