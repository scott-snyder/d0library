      SUBROUTINE VRDSAVE(ZVTX,PHIMIN,PHIMAX,THEMIN,THEMAX)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Interface routine for passing road information from
C-               VTROAD to low level tracking routines (VPOINT,FTVTXT).  Find 
C-               and save central value. 
C-      
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created   5-FEB-1993   Ed Oltman
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL ZVTX,PHIMIN,PHIMAX,THEMIN,THEMAX,PHI,DZDR
      REAL S_ZVTX,S_PHI,THETA,S_DZDR
      REAL PHI1,PHI2
      REAL PI2
      DATA PI2/6.283185/
      DATA S_ZVTX/-99999./
C----------------------------------------------------------------------
      S_ZVTX = ZVTX
      PHI1 = PHIMIN
      PHI2 = PHIMAX
      IF (PHI1 .GT. PHI2) THEN
        IF (PHI2 .LT. 0.) THEN
          PHI2 = PHI2 + PI2
        ELSE
          PHI1 = PHI1 - PI2
        ENDIF
      ENDIF
      S_PHI = 0.5*(PHI1+PHI2)
      THETA = 0.5*(THEMIN+THEMAX)
      S_DZDR = COS(THETA)/SIN(THETA)
      GO TO 999
      ENTRY VRDGET(ZVTX,PHI,DZDR)
      ZVTX = S_ZVTX
      PHI = S_PHI
      DZDR = S_DZDR
  999 RETURN
      END
