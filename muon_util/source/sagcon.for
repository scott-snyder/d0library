      INTEGER FUNCTION SAGCON (RINN, ROUT, ENDCAP, T_MEDIUM, V_MEDIUM)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : get SAMUS geometry constants
C-
C-   Inputs  : None
C-   Outputs : RINN - inner tubes radius
C-             ROUT - out tubes radius
C-             ENDCAP - endcap length
C-             T_MEDIUM - tracking medium number of tube wall      
C-             V_MEDIUM - tracking medium number of tube gas
C-   Controls: 
C-
C-   Created  27-SEP-1990   A. Efimov
C-   Updated  30-APR-1991   Andrei Kiryunin: geometry from banks SSTH, etc 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      REAL RINN, ROUT, ENDCAP
      INTEGER LSSTH,GZSSTH, T_MEDIUM,V_MEDIUM
C----------------------------------------------------------------------
C
      SAGCON = -1
      LSSTH = GZSSTH()
      IF (LSSTH.LT.0) GOTO 999
      RINN = C(LSSTH+9) * 0.5
      ROUT = RINN + C(LSSTH+10)
      ENDCAP = C(LSSTH+14)
      T_MEDIUM = IC(LSSTH+11)
      V_MEDIUM = IC(LSSTH+12)
      SAGCON = +1
C
  999 RETURN
      END
