      SUBROUTINE PLSETV(NX,NY,UMIN,UMAX,VMIN,VMAX)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Sets viewing paramters for LEGO plots automatically
C-                         Can be overridden by interactive parameters
C-                          for window and viewpoint set by user
C-
C-   Inputs  : NX - Number of grid lines in x
C-             NY - Number of grid lines in y
C-   Outputs : UMIN - min. u of window in world. coord
C-             UMAX - max. value of window in world coord.
C-             VMIN - min. v of window in world coord.
C              VMAX = max. v of window in world coord.
C-   Controls: 
C-
C-   Modified 12-AUG-1993   N. Oshima - Fix the view direction setting
C-                                      problem.
C-   Created  20-FEB-1989   Sharon Hagopian
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER NX,NY
      REAL UMIN,UMAX,VMIN,VMAX ! WINDOW LIMITS IN WORLD COORD.
C----------------------------------------------------------------------
C Local variables:
      REAL XN0,YN0,ZN0 ! DEFAULT NORMAL VECTOR      
      REAL XDIR,YDIR,ZDIR ! VECTOR NORMAL TO VIEWING PLANE
      REAL U0,V0 ! DEFAULT WINDOW SIZE
      REAL U(4),V(4) ! CORNERS OF GRID IN VIRTUAL COORD.
      REAL U14,V14,U24,V24 ! SIN,COS OF ANGLES OF DIAMOND GRID
      REAL VV
C-----------------------------------------------------------------------
C Data Statements:
      DATA U0,V0/1000.,1000./
      DATA XN0,YN0,ZN0/0.,0.,-1./
C----------------------------------------------------------------------
      CALL J3RGET(8,XDIR,YDIR,ZDIR)
      CALL JPARAL
      IF(XDIR.EQ.XN0.AND.YDIR.EQ.YN0.AND.ZDIR.EQ.ZN0)THEN
        XDIR=15.
        YDIR=-15.
        ZDIR=-3.
      ENDIF
C  SET VIEW DIRECTION
      CALL JVUPNT(0.,0.,0.)
      CALL JNORML(XDIR,YDIR,ZDIR)
      CALL JUPVEC(0.,0.,1.)
C  DETERMINE THE WINDOW SIZE
      IF(NX.LT.60.OR.NX.GT.80)GO TO 10
      IF(NY.LT.60.OR.NY.GT.80)GO TO 10
      VMIN=-NY*.12-NX*.12
      VMAX=NX*.22
      UMIN=1-NY*.8-NX*.9
      UMIN=1.05*UMIN
      UMAX=4.2
      GO TO 50
C CALCULATE WINDOW AUTOMATICALLY
   10 CALL J4RGET(1,UMIN,UMAX,VMIN,VMAX)
      IF(UMAX.EQ.U0.AND.VMAX.EQ.V0)THEN
        CALL JCONWV(FLOAT(NX+1),FLOAT(NY+1),0.,U(1),V(1))
        CALL JCONWV(1.,FLOAT(NY+1),0.,U(2),V(2))
        CALL JCONWV(FLOAT(NX+1),1.,0.,U(3),V(3))
        CALL JCONWV(1.,1.,0.,U(4),V(4))
        U24=(U(2)-U(4))/SQRT((U(2)-U(4))**2+(V(2)-V(4))**2)
        U14=(U(1)-U(4))/SQRT((U(1)-U(4))**2+(V(1)-V(4))**2)
        V24=(V(2)-V(4))/SQRT((U(2)-U(4))**2+(V(2)-V(4))**2)
        V14=(V(1)-V(4))/SQRT((U(1)-U(4))**2+(V(1)-V(4))**2)
        UMAX=4.
        UMIN=U24*NY +U14*NX
        VMAX=-.10*UMIN
        VV=AMIN1(FLOAT(NX),FLOAT(NY))
        IF(VMAX.LT.UMAX)THEN
          VMAX=UMAX
        ENDIF        
        VMIN=-1.*VMAX
      ENDIF 
   50 CALL JWINDO(UMIN,UMAX,VMIN,VMAX)
  999 RETURN
      END
