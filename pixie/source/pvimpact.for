      SUBROUTINE PVIMPACT
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Draw blown up view of XY vertex from
C-                extrapolated VTX tracks 
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  28-FEB-1993   M. Pang
C-   Updated  16-JUL-1993   Liang-ping Chen, M. Pang  
C-                          use VERTXMC, VERTYMC for MC data  
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INTEGER LVTRH,GZVTRH,N_VTX_TRAK
      INTEGER LVTXT,GZVTXT,I_TRAK,I
      INTEGER IER
      REAL FULL_SCALE,AL,XG,YG,RAY1,RAY2,A1,B1,C1
      REAL X1(2),Y1(2),XT(2),YT(2),DIS1,DIS2
      REAL X2(2),Y2(2),A2,B2,C2,XP1(1),XP2(2)
      REAL YP1(2),YP2(2),BEAM(3),VERTX,VERTY
      REAL VERTXMC, VERTYMC
      REAL RADIUS_IN,VALUE,EPS1,EPS2
      CHARACTER*3 CTRK
      CHARACTER*4 SCALE
      LOGICAL FIRST
      DATA FIRST /.TRUE./
C----------------------------------------------------------------------

      IF ( FIRST ) THEN
        FIRST = .FALSE.
        CALL EZPICK( 'PX_VTXDIS_RCP' )
        CALL EZGET( 'VERTX', VERTX, IER )
        CALL EZGET( 'VERTY', VERTY, IER )
        CALL EZGET( 'VERTXMC', VERTXMC, IER )
        CALL EZGET( 'VERTYMC', VERTYMC, IER )
        CALL EZGET( 'RADIUS_IN', RADIUS_IN, IER )
        CALL EZRSET
        IF ( IQ(LHEAD + 1) .GT. 1000 ) THEN  ! it is MC data     
          VERTX = VERTXMC 
          VERTY = VERTYMC                                       
        ENDIF                                                   
      END IF

C
C Draw axis
C
      CALL PUOPEN

      CALL PXCOLR('FOR')
      FULL_SCALE = 1.5
      EPS1 = 0.05
      EPS2 = 0.02
      CALL JMOVE(-FULL_SCALE,0.)
      CALL JDRAW(FULL_SCALE,0.)
      CALL JMOVE(0.,-FULL_SCALE)
      CALL JDRAW(0.,FULL_SCALE)
      DO I = -3,3
	VALUE = FLOAT(I)/2.
        CALL JMOVE(VALUE,-EPS1)
        CALL JDRAW(VALUE,EPS1)
        CALL JMOVE(-EPS1,VALUE)
        CALL JDRAW(EPS1,VALUE)
	IF ( I .NE. 0 ) THEN
          WRITE(SCALE,'(F4.1)') VALUE
          CALL PUVSTR(VALUE,-0.12,1.,2.,SCALE)
          CALL PUVSTR(0.12,VALUE,1.,2.,SCALE)
	END IF	
      END DO
      CALL PUVSTR(1.8,-0.2,1.,2.,'cm')
      DO I = -15,15
	VALUE = FLOAT(I)/10.
        CALL JMOVE(VALUE,-EPS2)
        CALL JDRAW(VALUE,EPS2)
        CALL JMOVE(-EPS2,VALUE)
        CALL JDRAW(EPS2,VALUE)
      END DO

      BEAM(1) = VERTX
      BEAM(2) = VERTY
      BEAM(3) = 0.

C
C Draw Tracks
C
      CALL PXCOLR('RED')
      LVTRH=GZVTRH()
      IF (LVTRH .EQ. 0) GOTO 1000
      N_VTX_TRAK = IQ(LVTRH+2)          ! # OF TRACKS
      LVTXT = GZVTXT(0)


      
      RAY1 = RADIUS_IN
      RAY2 = 1.7
      DO WHILE ( LVTXT .NE. 0 ) 
        I_TRAK = IQ(LVTXT-5)
        AL =  Q(LVTXT+6)
        XG = Q(LVTXT+7)
        YG = Q(LVTXT+8)
        A1 = TAN(AL)**2 + 1.
        B1 = - BEAM(1) + TAN(AL)*(YG - TAN(AL)*XG - BEAM(2))
        C1 =  B1**2 -
     &    A1*(BEAM(1)**2+(YG - TAN(AL)*XG - BEAM(2))**2 -RAY1**2)
        if ( C1 .lt. 0. ) goto 10
C TWO POINT CROSSING INNER CIRCLE CENTERED AT BEAM
        X1(1) = ( -B1 + SQRT(C1)  ) / A1
        X1(2) = ( -B1 - SQRT(C1)  ) / A1
        Y1(1) = TAN(AL)*X1(1) + YG - TAN(AL)*XG
        Y1(2) = TAN(AL)*X1(2) + YG - TAN(AL)*XG
        DIS1 = SQRT((XG-X1(1))**2 + (YG-Y1(1))**2)
        DIS2 = SQRT((XG-X1(2))**2 + (YG-Y1(2))**2)
        IF ( DIS1 .GT. DIS2 ) THEN
          XT(1) = X1(1)
          YT(1) = Y1(1)
        ELSE
          XT(1) = X1(2)
          YT(1) = Y1(2)
        END IF
        A2 = TAN(AL)
        B2 = YG - TAN(AL)*XG
        C2 =  A2**2*B2**2-(1.+A2**2)*(B2**2-RAY2**2)
        if ( C2 .lt. 0. ) goto 10
C TWO POINT CROSSING OUTER CIRCLE CENTERED THE CENTER OF THE CHAMBER
        X2(1) = ( -A2*B2 + SQRT(C2)  ) / ( 1+A2**2 )
        X2(2) = ( -A2*B2 - SQRT(C2)  ) / ( 1+A2**2 )
        Y2(1) = A2*X2(1) + B2
        Y2(2) = A2*X2(2) + B2
        XP1(1) = X2(1)
        XP2(1) = X2(2)
        YP1(1) = Y2(1)
        YP2(1) = Y2(2)
        DIS1 = SQRT((XG-X2(1))**2 + (YG-Y2(1))**2)
        DIS2 = SQRT((XG-X2(2))**2 + (YG-Y2(2))**2)
        IF ( DIS1 .LT. DIS2 ) THEN
          XT(2) = X2(1)
          YT(2) = Y2(1)
        ELSE
          XT(2) = X2(2)
          YT(2) = Y2(2)
        END IF
        CALL JMOVE(XT(1),YT(1))
        CALL JDRAW(XT(2),YT(2))
        WRITE(CTRK,113) I_TRAK
        CALL PUVSTR(XT(2)+0.1*COS(AL),YT(2)+0.1*SIN(AL)
     &                ,1.,2.,CTRK)
   10   CONTINUE
        LVTXT = LQ(LVTXT)
      END DO

113   FORMAT(I3)

1000  CONTINUE
      CALL JRCLOS

  999 RETURN
      END
