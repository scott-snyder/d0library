      SUBROUTINE C_SETUP_ISAJET
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : SETUP ISAJET TRACK PARAMETERS
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  28-MAR-1992   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:PI.DEF'
      INCLUDE 'D0$INC:CTRAK.INC'
C----------------------------------------------------------------------
      CALL CISKIN(NPART,VERT,IDPART,PART)
C
      IF(NPART.LE.0.OR.NPART.GT.1)THEN
        CALL ERRMSG('CALORIMETER','C_SETUP_ISAJET',
     &    'WRONG NUMBER OF PARTICLES ','W')
        GO TO 999
      ENDIF
C
C ****  WE NEED TO PICK UP PRIMARY ELECTRON PARAMETERS
C
      UVEC(1,1) = PART(1,1)/PART(4,1)
      UVEC(2,1) = PART(2,1)/PART(4,1)
      UVEC(3,1) = PART(3,1)/PART(4,1)     ! UNIT VECTOR ALONG
C                                        ! ISAJET TRAJECTORY
      PHI(1) = ATAN2(UVEC(2,1),UVEC(1,1))/RADIAN     ! IN DEGREES
      RAP(1) = ATAN2(SQRT(UVEC(2,1)**2+UVEC(1,1)**2),UVEC(3,1))
      RAP(1) = -ALOG(SIN(RAP(1)/2.)/COS(RAP(1)/2.))    ! RAPIDITY OF TRACK
C
  999 RETURN
      END
