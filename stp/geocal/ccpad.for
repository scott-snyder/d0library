      SUBROUTINE CCPAD(LCLAY, TITLE)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : TO FILL PAD INFORMATION  FROM SRCP INTO 
C-       CLAY BANK.
C-
C-   Inputs  :     LCLAY     pointer to CLAY bank
C-                 TITLE     identifier of SRCP array
C-   Outputs : 
C-   Controls: 
C-   Zebra Banks Altered:    CLAY
C-
C-   Created   4-FEB-1989   Stephen Kahn
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:CLAY.DEF'
      INCLUDE 'D0$INC:SCPARR.INC'
      INCLUDE 'D0$INC:PI.DEF'
      INCLUDE 'D0$INC:SCCCH_FLOOR.INC'
C
      CHARACTER TITLE*(*)
      INTEGER LCLAY, NPHI, NETA, I
      REAL CM_PER_INCH
      PARAMETER (CM_PER_INCH=2.54)
C
      CALL GTSRCP(TITLE, IVAL, 1)      ! get SRCP array
C
      NPHI = IVAL(3) - 1
      NETA = IVAL(NPHI + 5) - 1
      IC(LCLAY + ILNETA) = NETA        ! numbr of eta's
      IC(LCLAY + ILNPHI) = NPHI        ! numbr of phi's
      CALL UCOPY(RVAL(NPHI+6), C(LCLAY + ILZPD0), NETA+1)  ! z position
C                                        ! of eta boundaries
      C(LCLAY + ILPHIC) = RVAL(2)*RADIAN    ! module angular offset
      C(LCLAY + ILDPHI) = TWOPI/(IVAL(1)*NPHI)     ! module angular extent
      C(LCLAY + ILPHI0) = C(LCLAY+ILPHIC) - 0.25*IC(LCLAY+ILNPHI)*
     &  C(LCLAY+ILDPHI)                ! angular position of 1st pad
C
      DO 100 I = 0, NPHI
  100 RVAL(I + 4) = RVAL(I + 4)*RADIAN      ! convert to radians
      CALL UCOPY(RVAL(4), C(LCLAY+ILZPD1+NETA), NPHI+1)    ! phi
C                                        ! boundaries in radians
      C(LCLAY + ILWDTH) = RVAL(NPHI+4) - RVAL(4)      ! active azi width
      IF(TITLE(1:4) .EQ. 'CCCH') THEN   ! CCCH edge element
        C(LCLAY + ILZPD1 + NETA + NPHI + 1) = 0.5 * CM_PER_INCH *
     &    CCCH_FLOOR_OUTER_LENGTH 
      END IF
C----------------------------------------------------------------------
  999 RETURN
      END
