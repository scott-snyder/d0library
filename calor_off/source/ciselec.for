      SUBROUTINE CISELEC
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Gets ISAJET electron parameters 
C-
C-   Inputs  : NONE
C-   Outputs : 
C-   Controls: 
C-
C-   Created   2-MAY-1990   Norman Graf
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:CTRAK.INC'
      INCLUDE 'D0$INC:CISALEC.INC'
C
      INTEGER I,J
      REAL PHI_ISA,THETA_ISA,ETA_ISA
C----------------------------------------------------------------------
C
C ****  Get ISAJET electron parameters
C     
      CALL CISKIN(NPART,VERT_ISALEC,IDPART,PART)
      NISALEC = 0
      DO 10 I = 1,NPART
        IF (ABS(IDPART(I)) .NE. 12) GOTO 10 ! only electrons
        NISALEC = NISALEC + 1
        DO 20 J = 1,5
   20   P_ISALEC(J,NISALEC) = PART(J,I)
        UVEC_ISALEC(1,NISALEC) = PART(1,I)/PART(4,I)
        UVEC_ISALEC(2,NISALEC) = PART(2,I)/PART(4,I)
        UVEC_ISALEC(3,NISALEC) = PART(3,I)/PART(4,I)     ! Unit vector along
C                                           ! ISAJET trajectory
        CALL ETOETA(PART(1,I),PHI_ISA,THETA_ISA,ETA_ISA)
        THETA_ISALEC(NISALEC) = THETA_ISA
        PHI_ISALEC(NISALEC) = PHI_ISA
        ETA_ISALEC(NISALEC) = ETA_ISA            ! rapidity 
   10 CONTINUE
  999 RETURN
      END
