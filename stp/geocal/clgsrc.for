      SUBROUTINE CLGSRC( LCLGA, TITLE, MOTHER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Obtain SRCP position data translated to
C-                 Master coordinate system
C-
C-   Inputs  :     LCLGA     pointer to CLGA or CLGI bank  
C-                 TITLE     SRCP array for daughter volume
C-                 MOTHER    SRCP array for mother volume
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
C
      CHARACTER TITLE*(*), MOTHER*(*), RMATRX*15
      INTEGER LCLGA, IROT, I, NMTX, INDX, ICOORD
      REAL  XD(3), X(3), CYLIND
      REAL*8 ROTM(9)
      REAL RAD
      PARAMETER (RAD=0.017453293)
C
      CALL GTSRCP( TITLE, IVAL, 1)     ! get SRCP array of daughter
C
      IC( LCLGA + IGSHAP) = IVAL(2)    ! shape name
      IC( LCLGA + IGMATE) = IVAL(3)    ! material type
C     IC( LCLGA + IGVOL) =IVAL(1)      ! geant vol name
      IROT = IVAL(6)                   ! rotation identifier
      ICOORD = IC( LCLGA + IGCOOR)
C
      IF( MOTHER(1:4) .EQ. 'NONE') THEN     ! mother volume already
        IF( ICOORD .EQ. 123) THEN      !
           CALL UCOPY( RVAL(8), C(LCLGA + IGXCEN), 3)
        ELSE IF( ICOORD .EQ. 345) THEN 
           DO 100 I = 1, 3
  100      C(LCLGA+IGRCEN+I-1) = CYLIND(RVAL(8),I,123)
        ELSE
           WRITE( 6, *) 'NOT CARTESIAN:', TITLE
        END IF
        RETURN
      ELSE         ! not mother volume
        CALL UCOPY(RVAL(8), XD, 3)     ! transfer daughter coordinates
C
        IF(TITLE(1:2) .EQ. 'CC') THEN
          RMATRX = 'CC_ROT_MATRICES'
        ELSE
          RMATRX = 'EC_ROT_MATRICES'
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
C      DO 300 I = 1, 3
C 300  X(I) = XD(1)*ROTM(I) + XD(2)*ROTM(I+3) + XD(3)*ROTM(I+6)
C
      CALL UCOPY(XD,X,3)    ! previous transformation not needed
C
C ... ADD MOTHER COORDINATES
C
      CALL GTSRCP(MOTHER, IVAL, 1)
C
      DO 400 I = 1, 3
  400 X(I) = X(I) + RVAL(I+7)
C
C ... PUT INTO PROPER COORD SYSTEM
C
      IF(ICOORD .EQ. 123) THEN         ! if cartesian coord wanted
        CALL UCOPY(X, C(LCLGA+IGXCEN),3)
      ELSE IF (ICOORD .EQ. 345) THEN   ! if cylindrical coord wanted
        DO 450 I = 1,3
  450   C(LCLGA+IGRCEN+I-1) = CYLIND(X,I,123)
      ELSE 
        WRITE(6,*) ' NOT CARTESIAN OR CYLINDRICAL: ',TITLE,MOTHER
      END IF
      END IF
      CALL ROTPAX( ROTM, C(LCLGA+IGOMGE))   ! rot mtx to pax angles
C----------------------------------------------------------------------
  999 RETURN
      END
