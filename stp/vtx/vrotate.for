      SUBROUTINE VROTATE(RZS,RYPS,RZPS,ROT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : MAke a rotation matrix ROT to rotate a vector about
C-               the Euler-like angles RZ,RYP,RZP
C-
C-   Inputs  : RZS  -- rotation about z-axis
C-             RYPS --     "      "   y'-axis
C-             RZPS --     "      "   z'-axis
C-   Outputs : ROT
C-   Controls: 
C-
C-   Created  12-AUG-1992   Ed Oltman
C-   Updated  12-FEB-1993   Ed Oltman  FOR D0 SOFTWARE 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
c I/O:
      REAL   RZS,RYPS,RZPS
      DOUBLE PRECISION ROT(3,3)
      LOGICAL FIRST/.TRUE./
c Locals:
      DOUBLE PRECISION RZ,RYP,RZP
      DOUBLE PRECISION SN,CS,R1(3,3),R2(3,3),R3(3,3),TMP(3,3)
      INTEGER I,J,K
C----------------------------------------------------------------------
      IF (FIRST) THEN
        FIRST = .FALSE.
        DO I = 1,3
          DO J = 1,3
            R1(I,J) = 0.D0
            R2(I,J) = 0.D0
            R3(I,J) = 0.D0
          ENDDO
        ENDDO
        R1(3,3) = 1.D0
        R2(2,2) = 1.D0
        R3(3,3) = 1.D0
      ENDIF
      RZ = RZS
      RYP= RYPS
      RZP= RZPS
c..Rotation about z-axis
      CS = DCOS(RZ)
      SN = DSIN(RZ)
      R1(1,1) = CS
      R1(1,2) =-SN
      R1(2,1) = SN
      R1(2,2) = CS
c..Rotation about y'-axis
      CS = DCOS(RYP)
      SN = DSIN(RYP)
      R2(1,1) = CS
      R2(1,3) = SN
      R2(3,1) =-SN
      R2(3,3) = CS
c..Rotation about z'-axis
      CS = dCOS(RZP)
      SN = dSIN(RZP)
      R3(1,1) = CS
      R3(1,2) =-SN
      R3(2,1) = SN
      R3(2,2) = CS
c..Now multiply everything together: first ROT = Ryp.Rz
      DO I = 1,3
        DO K = 1,3
          TMP(I,K) = 0.
          DO J = 1,3
            TMP(I,K) = TMP(I,K) + R2(I,J)*R1(J,K)
          ENDDO
        ENDDO
      ENDDO
c..And finally, ROT = Rzp.Ryp.Rz
      DO I = 1,3
        DO K = 1,3
          ROT(I,K) = 0.
          DO J = 1,3
            ROT(I,K) = ROT(I,K) + R3(I,J)*TMP(J,K)
          ENDDO
        ENDDO
      ENDDO
  999 RETURN
      END
