      SUBROUTINE PRFDTX
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Prints out table of theta sector positions
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  27-APR-1989   Jeffrey Bantly
C-   Updated  14-FEB-1992   Susan K. Blessing  Remove machine block for 
C-    ATAND2, SIND, COSD.  Use radians instead.
C-   Updated   1-MAR-1993   Susan K. Blessing  Remove PRODUC. 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:PI.DEF'
C
      INTEGER QUAD,SECTOR
      INTEGER PRUNIT,ICORNR
      INTEGER USUNIT
C
      REAL    DIMENS(6),CORNRX(4),CORNRY(4),ANGLE,RADIUS
C
C----------------------------------------------------------------------
      PRUNIT=USUNIT()
      IF(PRUNIT.LE.0) GOTO 999
      WRITE(PRUNIT,*) ' Printout of the Sizes and Positions of Thetas'
      DO 20 QUAD = 0,7
        WRITE(PRUNIT,*) ' QUAD =',QUAD
        DO 30 SECTOR = 0,5
          WRITE(PRUNIT,*) ' SECTOR =',SECTOR
          CALL GTFDTX(QUAD,SECTOR,DIMENS)
          CORNRX(1) = DIMENS(1)-DIMENS(4)    !
          CORNRX(2) = DIMENS(1)+DIMENS(4)    !   (3)+-------+(4)
          CORNRX(3) = DIMENS(1)-DIMENS(4)    !      |       |
          CORNRX(4) = DIMENS(1)+DIMENS(4)    !      |   +   |
          CORNRY(1) = DIMENS(2)-DIMENS(5)    !      |       |
          CORNRY(2) = DIMENS(2)-DIMENS(5)    !   (1)+-------+(2)
          CORNRY(3) = DIMENS(2)+DIMENS(5)    !
          CORNRY(4) = DIMENS(2)+DIMENS(5)    !
          DO 40 ICORNR = 1,4
            IF( QUAD .LE. 3 ) THEN    ! rotate by 45 degrees
              ANGLE = ATAN2(CORNRY(ICORNR),CORNRX(ICORNR))
              ANGLE = ANGLE + PI/4.
              RADIUS = (CORNRX(ICORNR)**2 + CORNRY(ICORNR)**2)**.5
              CORNRX(ICORNR) = RADIUS*COS(ANGLE)
              CORNRY(ICORNR) = RADIUS*SIN(ANGLE)
            ENDIF
            WRITE(PRUNIT,*) ' CORNR=',ICORNR,' X,Y=',
     &                      CORNRX(ICORNR),CORNRY(ICORNR)
   40     CONTINUE
   30   CONTINUE
   20 CONTINUE
C----------------------------------------------------------------------
  999 RETURN
      END
