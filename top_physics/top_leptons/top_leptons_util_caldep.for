      SUBROUTINE TOP_LEPTONS_UTIL_CALDEP(MU_TRK_PT,MU_ETA,MU_PHI,NCELLS,
     &  EM_E,TOT_E,EM_E_90,TOT_E_90,EM_E_180,TOT_E_180,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-
C-   Inputs  : MU_TRK_PT(3) = Space point on track in CD volume
C-             MU_ETA         Pseudorapidity
C-             MU_PHI         Phi in radians
C-             NCELLS         (INTEGER) number of nearset neighbors to
C-                                      cal cells hit by muon track
C-                                      must be less than or equal to 9
C-   Outputs : EM_E           EM energy in cells requested
C-             TOT_E          Totle energy in cells requested
C-             EM_E_90        EM energy in cells rotated 90deg in phi
C-             TOT_E_90       Totle energy in cells rotated 90 in phi
C-             EM_E_180       EM energy in cells back-to-back with track
C-             TOT_E_180      Totle energy in cells back-to-back
C-             IER            Return Flag :
C-                                   0 - OK
C-                                  -1 - illegal no of n.neighbor cells requested
C-                                  -2 - input track point is outside of CD volume
C-                                  -3 - problem in accessing calorimeter cell
C-                                       data with CAL_ECELL
C-   Controls: 
C-
C-   Created  12-MAY-1993   joey thompson
C-   Modified 14-May-1993   Minor code cleanup
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
C
      INTEGER  NCELLS,NCELLS_MAX
      INTEGER  IER,ITEMP1
C
      REAL     MU_TRK_PT(3),MU_ETA,MU_PHI
      REAL     EM_E,TOT_E,EM_E_90,TOT_E_90,EM_E_180,TOT_E_180
      REAL     PI,ZMAX,RMAX,RTEMP1
      REAL     MU_PHI_90,MU_PHI_180,MU_ETA_NEG
C
      DATA     PI/3.14159/
      DATA     ZMAX/100./
      DATA     RMAX/72./
      DATA     NCELLS_MAX/9/
C
C----------------------------------------------------------------------
C
      EM_E=999.0
      TOT_E=999.0
      EM_E_90=999.0
      TOT_E_90=999.0
      EM_E_180=999.0
      TOT_E_180=999.0
      IER = 0
C
C First check input to see if it qualifies...
C
      IF(NCELLS.GT.NCELLS_MAX) THEN
        IER=-1
        CALL ERRMSG('CALDEP not supported for NCELLS GT 9',
     1              'TOP_LEPTONS_UTIL_CALDEP',' ','W')
        GOTO 999
      ENDIF
      IF (ABS(MU_TRK_PT(3)).GT.ZMAX .OR.
     1    SQRT( (MU_TRK_PT(1))**2 + (MU_TRK_PT(2))**2 ) .GT. RMAX) THEN
        IER=-2
        CALL ERRMSG('Space point for Track not within CD volume',
     1              'TOP_LEPTONS_UTIL_CALDEP',' ','W')
        GOTO 999
      ENDIF
C
C Now get angles
C
      MU_ETA_NEG = -1. * MU_ETA
      MU_PHI_90  = MU_PHI + (PI*0.5)
      MU_PHI_180 = MU_PHI + 180
C
C Now get energy dep info
C
      ITEMP1 = 0
      CALL CAL_ECELL(MU_TRK_PT,MU_ETA,MU_PHI,NCELLS,
     1               EM_E,TOT_E,RTEMP1,ITEMP1)
      IF (ITEMP1.NE.0) IER = -3
C      
      ITEMP1 = 0
      CALL CAL_ECELL(MU_TRK_PT,MU_ETA,MU_PHI_90,NCELLS,
     1               EM_E_90,TOT_E_90,RTEMP1,ITEMP1)
      IF (ITEMP1.NE.0) IER = -3
C
      ITEMP1 = 0
      CALL CAL_ECELL(MU_TRK_PT,MU_ETA_NEG,MU_PHI_180,NCELLS,
     1               EM_E_180,TOT_E_180,RTEMP1,ITEMP1)
      IF (ITEMP1.NE.0) IER = -3
  999 RETURN
      END
