      SUBROUTINE MAKE_W(EVECT,ETMISS,ETPHI,WVECT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Reconstructs W bosons from electron and 
C-                         missing Et vectors 
C-
C-   Inputs  :  EVECT   electron four vector
C-              ETMISS  missing Et 
C-              ETPHI   phi of missing Et
C-              
C-   Outputs :  WVECT   reconstructed W four vector
C-   Controls: 
C-
C-   Created  14-AUG-1990   Norman A. Graf
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL EVECT(4),ETMISS,ETPHI,WVECT(4),W_MASS
      REAL M2,E2,PX2,PY2,PZ
      DATA W_MASS /80.5 /
C----------------------------------------------------------------------
      E2 = EVECT(4)*ETMISS
      PX2 = EVECT(1)*ETMISS*COS(ETPHI)
      PY2 = EVECT(2)*ETMISS*SIN(ETPHI)
      M2 = W_MASS*W_MASS/2
      IF(EVECT(3) .EQ. 0) THEN
        PZ = 0.
      ELSE
        PZ = (E2-PX2-PY2-M2)/EVECT(3)
      ENDIF
      WVECT(1) = EVECT(1)+ETMISS*COS(ETPHI)
      WVECT(2) = EVECT(2)+ETMISS*SIN(ETPHI)
      WVECT(3) = EVECT(3)+PZ
      WVECT(4) = WVECT(1)*WVECT(1)+WVECT(2)*WVECT(2)+WVECT(3)*
     &  WVECT(3)   
      WVECT(4) = SQRT(WVECT(4) + 2*M2)
  999 RETURN
      END
