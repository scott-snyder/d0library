      SUBROUTINE MNADD( NMOD, NWIR, NSCNT, NPM, IADC, IER )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return muon scintillator address 
C-
C-   Inputs  : NMOD  : Muon module ID
C-             NWIR  : wire ID
C-   Outputs : NSCNT : scintillator address
C-             NPM   : 1st or 2nd P.M. 
C-             IER   : 0 - no error
C-   Controls: None
C-
C-   Created   4-MAR-1992   Atsushi Taketani
C-   Updated  16-MAR-1994   Susumu Igarashi   
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER  NMOD, NWIR, NSCNT(*), NPM(*), IADC(*), IER
C----------------------------------------------------------------------
      IER = -1

      IF(NMOD.GE.200.AND.NMOD.LE.247.AND.NWIR.GE.1.AND.NWIR.LE.16)THEN
        NSCNT(1)=(NWIR-1)/2+1
        NSCNT(2)=(NWIR-1)/2+1
        NPM(1)=1
        NPM(2)=2
        IADC(1)=1
        IADC(2)=2
        IER = 0
      ELSE
        NSCNT(1)=0 
        NSCNT(2)=0 
        NPM(1)=0
        NPM(2)=0
        IADC(1)=0
        IADC(2)=0
        IER = 0
      ENDIF
C
  999 RETURN
      END
