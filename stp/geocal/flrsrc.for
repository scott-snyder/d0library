      SUBROUTINE FLRSRC( LCLAY, TITLE, MOTHER, GRANDM)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Obtain SRCP position data translated to
C-                 Master coordinate system for CLAY bank
C-
C-   Inputs  :     LCLAY     pointer to CLAY
C-                 TITLE     SRCP array for daughter volume
C-                 MOTHER    SRCP array for mother volume
C-                 GRANDM    SRCP array for grandmother volume
C-   Outputs : 
C-   Controls: 
C-
C-   Created   7-FEB-1989   Stephen Kahn
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:SCPARR.INC'
      INCLUDE 'D0$PARAMS:CLGA.PARAMS'
      INCLUDE 'D0$INC:CLAY.DEF'
C
      CHARACTER TITLE*(*), MOTHER*(*), GRANDM*(*), RMATRX*15, SHAPE*4
      INTEGER LCLAY, IROT, I, NMTX, INDX, ICOORD
      REAL XD(3), X(3), CYLIND
      REAL*8 ROTM(9)
      REAL RAD
      PARAMETER (RAD=0.017453293)
C
      IF(TITLE(1:4) .EQ. 'NONE') THEN
        CALL VZERO(X,3)
        ICOORD = 0
      ELSE
      CALL GTSRCP( TITLE, IVAL, 1)     ! get SRCP array of daughter
C
      IC(LCLAY + ILMATE) = IVAL(3)     ! material type associate to floor
      ICOORD = IC( LCLAY + ILCOOR)
      CALL UCOPY(RVAL(8), X, 3)     ! transfer daughter coordinates
      WRITE (SHAPE,'(A4)') IVAL(2)
      IF(SHAPE .EQ. 'TUBE') THEN
        IC(LCLAY + ILCOOR) = 123    ! tube should be in Cartesian coord
        ICOORD = 123
        C(LCLAY + ILDELZ) = 2.*RVAL(14)   ! shape param 3
      ELSE IF(SHAPE .EQ. 'TRD1' .AND. TITLE(1:2) .EQ. 'CC') THEN
        C(LCLAY + ILDELR) = 2.*RVAL(15)   ! shape param 4
      ELSE IF(SHAPE .EQ. 'TRD1' .AND. TITLE(1:2) .EQ. 'EC') THEN
        C(LCLAY + ILDELZ) = 2.*RVAL(14)   ! shape param 3
      ELSE IF(SHAPE .EQ. 'TRAP') THEN
        C(LCLAY + ILDELZ) = 2.*RVAL(15) * COS(RVAL(13)*RAD)
      END IF
      END IF
C
      CALL GTSRCP( MOTHER, IVAL, 1)    ! get SRCP array of mother
      IF(IC(LCLAY+ILMATE) .EQ. 0) IC(LCLAY+ILMATE) = IVAL(3)  ! use
C                         ! mother material type if daughter not  available
      CALL UCOPY(X,XD,3)               ! move X to XD
      CALL UCOPY(RVAL(8), X, 3)        ! copy mother positions
      IROT = IVAL(6)                   ! rotation identifier
      IF(MOTHER(1:2) .EQ. 'CC') THEN
        RMATRX = 'CC_ROT_MATRICES'
      ELSE
        RMATRX = 'EC_ROT_MATRICES'
      END IF
      IF( ICOORD .EQ. 0) THEN
        WRITE (SHAPE,'(A4)') IVAL(2)
        IF(SHAPE .EQ. 'TUBE' ) THEN
          IC(LCLAY + ILCOOR) = 123
          ICOORD = 123
          C(LCLAY + ILDELZ) = 2.*RVAL(14) ! shape param 3
        ELSE IF (SHAPE .EQ. 'TRD1') THEN
          IC(LCLAY + ILCOOR) = 123
          ICOORD = 123
          C(LCLAY + ILDELZ) = 2.*RVAL(14) ! shape param 3
        ELSE IF (SHAPE .EQ. 'TRAP') THEN
          IC(LCLAY + ILCOOR) = 123
          ICOORD = 123
          C(LCLAY + ILDELZ) = 2.*RVAL(15) ! shape param 3
        END IF
      END IF
C
      CALL GTSRCP( RMATRX, IVAL, 1)  ! rotation matrix array
      NMTX = IVAL(1)
      INDX = 1
C
      DO 200 I = 1, NMTX             ! loop thru rotation angle sets
        IF(IROT .EQ. IVAL(INDX+1)) THEN    ! try to match rotation flag
           ROTM(1) = SIN(RVAL(INDX+2)*RAD)*COS(RVAL(INDX+3)*RAD)
           ROTM(2) = SIN(RVAL(INDX+2)*RAD)*SIN(RVAL(INDX+3)*RAD)
           ROTM(3) = COS(RVAL(INDX+2)*RAD)
           ROTM(4) = SIN(RVAL(INDX+4)*RAD)*COS(RVAL(INDX+5)*RAD)
           ROTM(5) = SIN(RVAL(INDX+4)*RAD)*SIN(RVAL(INDX+5)*RAD)
           ROTM(6) = COS(RVAL(INDX+4)*RAD)
           ROTM(7) = SIN(RVAL(INDX+6)*RAD)*COS(RVAL(INDX+7)*RAD)
           ROTM(8) = SIN(RVAL(INDX+6)*RAD)*SIN(RVAL(INDX+7)*RAD)
           ROTM(9) = COS(RVAL(INDX+6)*RAD)
           GO TO 210
        ELSE
           INDX = INDX + 7
        END IF
  200  CONTINUE
  210  CONTINUE
C
C ... INVERSE ROTATION AND ADD TRANSLATION TO GET GLOBAL SYSTEM
C 
       DO 300 I = 1, 3
  300  X(I) = X(I) + XD(1)*ROTM(I) + XD(2)*ROTM(I+3) + XD(3)*ROTM(I+6)
C
C ... ADD GRANDMOTHER COORDINATES
C
      IF(GRANDM(1:4) .NE. 'NONE') THEN
      CALL GTSRCP(GRANDM, IVAL, 1)
C
      DO 400 I = 1, 3
  400 X(I) = X(I) + RVAL(I+7)
      END IF
C
C ... PUT INTO PROPER COORD SYSTEM
C
      IF(ICOORD .EQ. 123) THEN         ! if cartesian coord wanted
        CALL UCOPY(X, C(LCLAY+ILRCEN),3)
      ELSE IF (ICOORD .EQ. 345) THEN   ! if cylindrical coord wanted
        DO 450 I = 1,3
  450   C(LCLAY+ILRCEN+I-1) = CYLIND(X,I,123)
      ELSE 
        WRITE(6,*) ' NOT CARTESIAN OR CYLINDRICAL: ',TITLE,MOTHER
      END IF
C
C ... PUT IN PRINCIPAL AXIS ANGLES
C
      CALL ROTPAX( ROTM, C(LCLAY+ILTHTE))   ! convert ROTM to PAX
C----------------------------------------------------------------------
  999 RETURN
      END
