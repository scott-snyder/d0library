C----------------------------------------------------------------------
      REAL FUNCTION D3UXH (KT1,KT2)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Will subtract, the DBL3 packed times,
C-    KT2 from KT1 and return the difference in hours.
C-
C-   Returned value  : KT1 - KT2 (in hours)
C-                     if = -999.0, probaly something went wrong
C-
C-   Inputs  : KT1 (I*4)   DBL3 packed time
C-   Outputs : KT2 (I*4)   DBL3 packed time
C-   Controls: 
C-
C-   Created  23-OCT-1992   Lars Rasmussen
C-   Mod      24-AUG-1994   Will save last convertions, l0r
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL D3UUT,LIB$DAY
C
      INTEGER KT1,KT2
      INTEGER KT1_S, KT2_S
      REAL    XH_S
      INTEGER VT1(2),VT2(2),ND1,ND2,NS1,NS2
      LOGICAL LRET
      DATA KT1_S /0/
      DATA KT2_S /0/
      DATA XH_S /-999.0/
C----------------------------------------------------------------------
      D3UXH = -999.0
      IF ( KT1 .EQ. KT1_S .AND. KT2 .EQ. KT2_S ) THEN
         D3UXH = XH_S
         RETURN
      ELSE IF ( KT1 .EQ. KT2_S .AND. KT2 .EQ. KT1_S ) THEN
         D3UXH = -XH_S
         RETURN
      END IF
C
      IF (.NOT. D3UUT (VT1,KT1)) RETURN
      IF (.NOT. D3UUT (VT2,KT2)) RETURN
      IF (.NOT. LIB$DAY(ND1,VT1,NS1)) RETURN
      IF (.NOT. LIB$DAY(ND2,VT2,NS2)) RETURN
C
      D3UXH = 24.*FLOAT((ND1-ND2)) + (FLOAT(NS1-NS2))/360000.
      KT1_S = KT1
      KT2_S = KT2
      XH_S = D3UXH
C
      RETURN
      END
