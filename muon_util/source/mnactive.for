      LOGICAL FUNCTION MNACTIVE(IMOD)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : To answer the question: does this muon chamber
C-                         have active scintillators for tracking?
C-
C-   Returned value:  .TRUE. or .FALSE.
C-   Inputs:          IMOD   [I]   Muon PDT Module number
C-   Outputs:         None
C-   Controls:
C-
C-   Created 3-MAR-1995 R. Markeloff. Based on MU_SCINT_MOD by B.S.Acharya,
C-                                    replaces obsolete function with same name
C-   Modified 21-APR-95 R. Markeloff. Added entry MNACTIVE_RUN
C-            24-Apr 1995  J. Wilcox  Reworked.  Removed MNACTIVE_RUN (which 
C-                                    didn't work for ENDTSK) and added a call
C-                                    to MU_SCINT_MOD.
C-             3-MAT-1995  M. Fortner Use MNMDAT as lookup table
C-             16-oct-1995 D. Wood  New start runs for octs 4 & 7 (from A.Ito)
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
C
      INTEGER  IMOD,ISTART,IRUN,JRUN(10)
      INTEGER  IFLG,NCEL,NSCT
C  Starting run for octants used in tracking
      DATA JRUN/3*74562,75039,86693,2*999999,89787,2*999999/
C
C--------------------------------------------------------------------
      MNACTIVE = .FALSE.
C
C  MC does not simulate scintillators at this time
      IF (IQ(LHEAD+1) .GT. 1000) RETURN
C
C  Check for tracking scintillator on this chamber
C
      CALL MNMDAT(IMOD,IFLG,NCEL,NSCT)
      IF (IFLG.EQ.0) THEN
        MNACTIVE = .TRUE.
        ISTART = JRUN(MOD(IMOD,10)+1)
        IRUN = IQ(LHEAD+6)
        IF (IRUN .LT. ISTART) MNACTIVE = .FALSE.
      END IF
C
      RETURN
      END
