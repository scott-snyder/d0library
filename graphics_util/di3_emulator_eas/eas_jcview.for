      SUBROUTINE JCVIEW(XVRP, YVRP, ZVRP, XEYE, YEYE, ZEYE, VANG)
C
C    Purpose:
CD   This module provides for the changing of the viewing transfomation 
CD   with one call.  The parameters passed in are the (X, Y, Z) view
CD   reference point (XVRP, YVRP, ZVRP), the eyepoint(XEYE, YEYE, ZEYE),
CD   and a viewing angle (VANGLE). All parameters are Reals and input.
C
C    A. Virgo
C    Fermi National Accelerator Laboratory
C    RD/Computing section -- Graphics Support Group
C    Batavia, IL  60510
C
CM   Module Date: 09-Jan-1989
CH   History:
CH      27-MAR-89  SA   Formula for PERSP corrected.
CH      26-MAR-89  SA   NORML was correctly normalized to 1.
CH      14-MAR-89  SA   The third component of vupnt and norml were corrected.
CH      09-MAR-89  SA   Direction of normal vector was correctly set.
CH      09-JAN-88  ATV  Correct window scaling problem.
CH      19-DEC-88  ATV  Initial entry.
C
      IMPLICIT NONE
C
C    Common blocks:
CB      GRFPAR-W, SEGINF-R
C
C    Calls:
CC      ERROR.
C
      EXTERNAL ERRHND
C
C    Next is the declaration of parameters passed to the subroutine/function.
C
      REAL XVRP, YVRP, ZVRP, XEYE, YEYE, ZEYE, VANG
C
C    Then local declarations of variables (non-common variables).
C
      REAL EPS, DX, DY, DZ, RHO, WSF
      INTEGER I
      DATA EPS /1.0E-9/
C
C    Then common block declarations.
C
      INCLUDE 'D0$INC:GRFPAR.INC/LIST'
      INCLUDE 'D0$INC:SEGINF.INC/LIST'
C
C    Then executable code follows
C
      IF (SEGOPN) THEN 
         CALL ERROR('JCVIEW: A SEGMENT IS OPEN')
      ENDIF
      IF (VANG .GT. 179.0) THEN
         CALL ERROR('JCVIEW: VANGLE NOT IN RANGE (0.0..179.0)')
      ENDIF
      DX = XVRP - XEYE
      DY = YVRP - YEYE
      DZ = ZVRP - ZEYE
      RHO = SQRT(DX * DX + DY * DY + DZ * DZ)
      IF (RHO .LT. EPS) THEN
         CALL ERROR('JCVIEW: THE VRP AND EYEPOINT ARE COINCIDENT')
      ENDIF
      PRJTYP   = PTPERS
      PERSP    = RHO
      VUPNT(1) = XVRP
      VUPNT(2) = YVRP
      VUPNT(3) = ZVRP
      NORML(1) = DX / RHO
      NORML(2) = DY / RHO
      NORML(3) = (DZ / RHO)
      IF (VANG .GT. 0.0) THEN
         WSF = RHO * TAND(VANG / 2.0)
         DO 10 I=1,3,2
            UWIND(I)     = -WSF
            UWIND(I + 1) =  WSF
   10    CONTINUE
      ENDIF
      RETURN
      END
