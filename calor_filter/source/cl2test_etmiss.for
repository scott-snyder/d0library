      FUNCTION CL2TEST_ETMISS()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : hist ETMISS, SUMET to test filters
C-              make package with this before and after vms_filter to verify
C-              effect of cuts
C-   Returned value  :
C-   Inputs  : PNUT1 from RECO path
C-   Outputs :
C-   Controls:
C-
C-   Created  30-JUN-1991   James T. Linnemann
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL CL2TEST_ETMISS
      INCLUDE 'D0$INC:ZEBCOM.INC'
      REAL    MPTCL2,SUMETCL2
      INTEGER LPNUT,GZPNUT
      CHARACTER*4 OLD_PATH
C----------------------------------------------------------------------
      LOGICAL FIRST
      SAVE FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
      IF (FIRST) THEN
        CALL HCDIR('//PAWC',' ')          ! go to top directory
        CALL HMDIR('CL2TEST_ETMISS','S')       ! create CL2TEST directory
        CALL HBOOK1(1,'ETMISS',50,0.,50.,0.)
        CALL HBOOK1(2,'SUMET',50,0.,500.,0.)
        FIRST = .FALSE.
      ENDIF
      CALL HCDIR('//PAWC/CL2TEST_ETMISS',' ') ! Get correct PAW directory
      CALL PATHGT(OLD_PATH)
      CALL PATHST('FILT')
      LPNUT = GZPNUT(1)
      MPTCL2 = 0.000001
      SUMETCL2 = 0.0000001
      IF (LPNUT.GT.0) THEN
        MPTCL2 = Q(LPNUT+7)+.000001        ! so can div afely
        SUMETCL2 = Q(LPNUT+14)+.0000001
      ENDIF
      CALL HFILL(1,MPTCL2,0.,1.)
      CALL HFILL(2,SUMETCL2,0.,1.)
      CALL PATHST(OLD_PATH)
      CL2TEST_ETMISS = .TRUE.
  999 RETURN
      END
