      SUBROUTINE DRSAMHIT(station,section,tube,iview,iflag)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : draw  SAMUS tube
C-
C-   Inputs  : station
C-             section
C-             tube
C-             IFLAG- = -1  To draw -Z (NORTH) A,B,&C stations tubes hit
C-             IFLAG- = +1  To draw +Z (SOUTH) A,B,&C stations tubes hit
C-                      +2  To draw -Z (NORTH) A station tubes hit
C-                      +3  To draw -Z (NORTH) B station tubes hit
C-                      +4  To draw -Z (NORTH) C station tubes hit
C-                      +5  To draw +Z (SOUTH) A station tubes hit
C-                      +6  To draw +Z (SOUTH) B station tubes hit
C-                      +7  To draw +Z (SOUTH) C station tubes hit
C-   Outputs : none
C-   Controls: none
C-
C-   Created  31-OCT-1994   Andrei Mayorov
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER  station,section,tube,itube,iview
      REAL SPAR1(3),SPAR2(3),XPAR1(3),XPAR2(3)
      INTEGER numt,iflag
      REAL XDCEN,YDCEN,ZDCEN,XDMIN,XDMAX,YDMIN,YDMAX,DX,DY,DZ
      REAL RAD,LEN,ANGLE
      REAL X1,Y1,X2,Y2,X3,Y3,X4,Y4
      DATA ANGLE/0.7071068/             ! = COS(45)
C----------------------------------------------------------------------

      IF(iview.EQ.2) THEN
        IF(IFLAG.EQ.-1.AND.STation.LT.4) GOTO 80
        IF(IFLAG.EQ.+1.AND.STation.GT.3) GOTO 80
        IF(IFLAG.GT.1.AND.IFLAG.LT.8) THEN
          IF((IFLAG-1).EQ.STation) GOTO 80
        END IF
        GOTO 999
   80   CALL SACELL2(station,section,tube,NUMT,SPAR1,XPAR1,SPAR2,XPAR2)
        IF (.NOT.(NUMT.EQ.1 .OR. NUMT.EQ.2)) THEN
          WRITE(6,*)'NUMT ERROR IN PSAMUD1_EV'
          GOTO 999
        ENDIF
        DO ITUBE=1,NUMT
          IF (ITUBE.EQ.1) THEN
            XDCEN=XPAR1(1)
            YDCEN=XPAR1(2)
          ELSE
            XDCEN=XPAR2(1)
            YDCEN=XPAR2(2)
          ENDIF
          IF (SECTION.EQ.1 .OR. SECTION.EQ.4) THEN
            CALL PXCOLR('RED')
            IF (ITUBE.EQ.1) THEN
              DX=SPAR1(2)
              DY=SPAR1(3)
            ELSE
              DX=SPAR2(2)
              DY=SPAR2(3)
            ENDIF
            XDMIN=XDCEN-DX
            XDMAX=XDCEN+DX
            YDMIN=YDCEN-DY
            YDMAX=YDCEN+DY
            CALL JMOVE(XDMIN,YDMIN)
            CALL JDRAW(XDMIN,YDMAX)
            CALL JDRAW(XDMAX,YDMAX)
            CALL JDRAW(XDMAX,YDMIN)
            CALL JDRAW(XDMIN,YDMIN)
          ENDIF
          IF (SECTION.EQ.3 .OR. SECTION.EQ.6) THEN
            CALL PXCOLR('RED')
            IF (ITUBE.EQ.1) THEN
              DX=SPAR1(3)
              DY=SPAR1(2)
            ELSE
              DX=SPAR2(3)
              DY=SPAR2(2)
            ENDIF
            XDMIN=XDCEN-DX
            XDMAX=XDCEN+DX
            YDMIN=YDCEN-DY
            YDMAX=YDCEN+DY
            CALL JMOVE(XDMIN,YDMIN)
            CALL JDRAW(XDMIN,YDMAX)
            CALL JDRAW(XDMAX,YDMAX)
            CALL JDRAW(XDMAX,YDMIN)
            CALL JDRAW(XDMIN,YDMIN)
          ENDIF
          IF (SECTION.EQ.2 .OR. SECTION.EQ.5) THEN
            CALL PXCOLR('MAG')
            IF (ITUBE.EQ.1) THEN
              RAD=SPAR1(2)
              LEN=SPAR1(3)
            ELSE
              RAD=SPAR2(2)
              LEN=SPAR2(3)
            ENDIF
            X1=XDCEN-LEN*ANGLE-RAD*ANGLE
            Y1=YDCEN-LEN*ANGLE+RAD*ANGLE
            X2=XDCEN+LEN*ANGLE-RAD*ANGLE
            Y2=YDCEN+LEN*ANGLE+RAD*ANGLE
            X3=XDCEN+LEN*ANGLE+RAD*ANGLE
            Y3=YDCEN+LEN*ANGLE-RAD*ANGLE
            X4=XDCEN-LEN*ANGLE+RAD*ANGLE
            Y4=YDCEN-LEN*ANGLE-RAD*ANGLE
            IF(STATION.EQ.1.OR.STATION.EQ.3.OR.STATION.EQ.5)THEN
              Y1=YDCEN+LEN*ANGLE-RAD*ANGLE
              Y2=YDCEN-LEN*ANGLE-RAD*ANGLE
              Y3=YDCEN-LEN*ANGLE+RAD*ANGLE
              Y4=YDCEN+LEN*ANGLE+RAD*ANGLE
            ENDIF
C
            CALL JMOVE(X1,Y1)
            CALL JDRAW(X2,Y2)
            CALL JDRAW(X3,Y3)
            CALL JDRAW(X4,Y4)
            CALL JDRAW(X1,Y1)
          END IF
        ENDDO
      ELSE
        CALL pxcolr('MAG')
        CALL sacell2(station,section,tube,numt,spar1,xpar1,spar2,xpar2)
        DO itube=1,numt
          IF (iview.EQ.1) THEN            ! SIDE VIEW
            IF(itube.EQ.1) THEN
              ydcen=xpar1(2)
            ELSE
              ydcen=xpar2(2)
            END IF
          ELSEIF (iview.EQ.3) THEN        ! TOP VIEW
            IF(itube.EQ.1) THEN
              ydcen=xpar1(1)
            ELSE
              ydcen=xpar2(1)
            END IF
          ENDIF
          IF (section.EQ.3 .OR. section.EQ.6.OR.section.EQ.1 .OR.
     &      section.EQ.4) THEN     ! X,Y - mark=+
            CALL pxmark('MAG',2,xpar1(3),ydcen,0)
          ELSEIF (section.EQ.2 .OR. section.EQ.5) THEN ! U,V - mark=0
            CALL pscell(iview,xpar1)
          ENDIF
        END DO
      END IF
  999 RETURN
      END
