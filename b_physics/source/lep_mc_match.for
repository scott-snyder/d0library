      SUBROUTINE LEP_MC_MATCH(NLEP,ISLEP,DELMOM,DELPHI,
     +                        DELTHE,MATCH)             
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Match reference(reconstructed)tracks with
C-                         corresponding ISAJET tracks        
C-   Inputs  : ISLEP
C-   Outputs : DELMOM,DELPHI,DELTHE,MATCH
C-   Controls: 
C-
C-   Created  14-NOV-1991   C.MURPHY
C-   Updated  24-MAR-1992   c.r.murphy - Added veekin.inc and dropped   
C-   some arguments from call list.    
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:VEEKIN.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZISAL.LINK'
      REAL PI,TWOPI,HALFPI,RAD
      INCLUDE 'D0$INC:PI.INC'
      REAL MOMREF,PHIREF,THEREF
      REAL ISLEP(3,2)
      REAL DELMOM,DELPHI,DELTHE,DIFFCHECK
      REAL DIFFMOM(2),DIFFPHI(2),DIFFTHE(2)
      REAL THEDIFF_SMALL,PHIDIFF_SMALL
      INTEGER NLEP,I
      LOGICAL MATCH
      DATA THEDIFF_SMALL,PHIDIFF_SMALL/0.1,0.1/
C
      MOMREF=STR(1,NLEP)
      THEREF=STR(2,NLEP)
      PHIREF=STR(3,NLEP)
      DO 100 I=1,2
        DIFFMOM(I)=ABS(MOMREF-ISLEP(1,I))
        DIFFTHE(I)=ABS(THEREF-ISLEP(2,I))
        DIFFPHI(I)=ABS(PHIREF-ISLEP(3,I))
        IF (DIFFPHI(I).GT.PI) THEN
          DIFFPHI(I)=TWOPI-DIFFPHI(I)
        ENDIF  
  100 CONTINUE
C
      IF (DIFFPHI(1).LT.DIFFPHI(2)) THEN
        DELPHI=PHIREF-ISLEP(3,1)
        IF (ABS(DELPHI).GT.PI) THEN
          DIFFCHECK=TWOPI-ABS(DELPHI)
          IF (DELPHI.LT.0.) THEN
            DELPHI=-DIFFCHECK
          ELSE
            DELPHI=DIFFCHECK
          ENDIF
        ENDIF
        IF (DIFFMOM(1).LT.0.40*ISLEP(1,1).AND.DIFFTHE(1).LT.
     +      THEDIFF_SMALL.AND.DIFFPHI(1).LT.PHIDIFF_SMALL) THEN
          MATCH=.TRUE.
          DELTHE=THEREF-ISLEP(2,1)
          DELMOM=(MOMREF-ISLEP(1,1))/ISLEP(1,1)
        ELSE 
          GO TO 200
        ENDIF
C  
      ELSE
C
200     DELPHI=PHIREF-ISLEP(3,2)
        IF (ABS(DELPHI).GT.PI) THEN
          DIFFCHECK=TWOPI-ABS(DELPHI)
          IF (DELPHI.LT.0.) THEN
            DELPHI=-DIFFCHECK
          ELSE
            DELPHI=DIFFCHECK
          ENDIF
        ENDIF
        IF (DIFFMOM(2).LT.0.40*ISLEP(1,2).AND.DIFFTHE(2).LT.
     +      THEDIFF_SMALL.AND.DIFFPHI(2).LT.PHIDIFF_SMALL) THEN
          MATCH=.TRUE.
          DELTHE=THEREF-ISLEP(2,2)
          DELMOM=(MOMREF-ISLEP(1,2))/ISLEP(1,2)
        ELSE
          MATCH=.FALSE.
          GO TO 999
        ENDIF
      ENDIF  
C
  999 RETURN
      END
