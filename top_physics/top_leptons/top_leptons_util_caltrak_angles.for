      SUBROUTINE TOP_LEPTONS_UTIL_CALTRAK_ANGLES(LZTRK,THETA,PHI,ETA)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns Theta,Phi,Eta for track specified
C-                         by LZTRK
C-
C-   Inputs  : LZTRK Bank pointer
C-
C-   Outputs : Best values of each of the following :
C-              Theta, phi, eta
C-
C-   Controls: 
C-
C-   Created   6-SEP-1992   Stephen J. Wimpenny
C-   Modified 22-Mar-1993   Routine name changed for library compatibility
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKA.INC'
C
      INTEGER LZTRK,LZFIT,LDTRK,LFDCT
      REAL THETA,PHI,ETA
C
      LDTRK=0
      LFDCT=0
      THETA=-1.570
      PHI=-1.570
      ETA=0.
      IF(LZTRK.GT.0) THEN
C
C *** Look for track Banks - Use LZFIT if present,otherwise use
C *** either CDC or FDC track angles
C
        LZFIT=LQ(LZTRK-1)
        LDTRK=LQ(LZTRK-7)
        LFDCT=LQ(LZTRK-8)
        IF(LZFIT.GT.0) THEN
C
C *** Global ZTRAK fit
C
          THETA=Q(LZFIT+13)
          ETA=0.0
          IF(THETA.GT.1.0E-5) ETA=-LOG(TAN(THETA/2.))
          PHI=Q(LZFIT+10)
        ELSE
          IF(LDTRK.GT.0) THEN
C
C *** CDC Track segment
C
            THETA=Q(LDTRK+9)
            ETA=0.0
            IF(THETA.GT.1.0E-5) ETA=-LOG(TAN(THETA/2.))
            PHI=Q(LDTRK+6)
          ELSEIF(LFDCT.GT.0) THEN
C
C *** FDC Track segment
C
            THETA=Q(LFDCT+22)
            ETA=0.0
            IF(THETA.GT.1.0E-5) ETA=-LOG(TAN(THETA/2.))
            PHI=Q(LFDCT+6)
          ENDIF
        ENDIF
      ENDIF
C------------------------------------------------------------------
  999 RETURN
      END
