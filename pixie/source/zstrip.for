       SUBROUTINE ZSTRIP 
C---------------------------------------------------------------------
C
C   Description:  Z-Strip Plot  
C   ------------ 
C          
C
C   Preconditions necessary before call:
C     NONE
C
C   Author:
C   ------- 
C       Lupe Rosas  May 29 1989
C
C---------------------------------------------------------------------
      IMPLICIT NONE
c      INCLUDE 'D0$INC:GRAPHF77.INC'
c      INCLUDE 'D0$INC:TRHITW.INC/LIST'
C---------------------------------------------------------------------
      CHARACTER*10 IMESS
      CHARACTER*3  CLAY
      CHARACTER*24 MESS
      INTEGER  ILAY, ICELL, I, J
      REAL XMIN,XMAX(3),YMIN,YMAX,ECUT,R(3),ACTLEN,ANGLE(3),
     X     UMIN, UMAX, VMIN, VMAX, PI
C---------------------------------------------------------------------
      DATA IMESS/'ZST LAY='/ 
      DATA R         /26.70,37.25,47.80/     ! Radious of layers
      DATA ACTLEN    /166.6/                 ! Active Length
      DATA XMIN,YMIN /0.,0./                 
      DATA YMAX      /255./
      DATA ANGLE     /23.7,29.0,47.0/        ! Helical pad pitch angles
      DATA PI        /3.14159/
C---------------------------------------------------------------------
      CALL PUGETV('ECUT',ECUT)
C
C Calculating XMAX for each display
      DO 10 I=1, 3
        XMAX(I) = (YMAX*ACTLEN)/(2 * PI * R(I))
   10 CONTINUE

      UMIN=-40.
      VMAX=296.
      VMIN=-40.
C Making Grid
      DO 20 ILAY=1,3
         UMAX = XMAX(ILAY)+ 40.
         CALL JWINDO(UMIN,UMAX,VMIN,VMAX)
         CALL PXITOC(ILAY,3,CLAY)
         MESS=IMESS//CLAY
         CALL PUMESS(MESS)
         CALL ZSGRID(XMIN,XMAX(ILAY),YMIN,YMAX,ANGLE(ILAY),ILAY,ECUT)
         CALL JPAUSE(1)
         IF (ILAY.NE.3)
     X    CALL PUHEAD('ZSEVEN')
   20 CONTINUE
      GO TO 999
  700 CALL PUMESS(' ERROR IN LAYER NUMBER')
  999 RETURN
      END
