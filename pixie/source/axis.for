      SUBROUTINE AXIS
C======================================================================
C
C  Description: SHOW DIRECTION OF AXIS
C  ============
C
C
C  Input Arguments:
C  ================
C   NONE
C
C  Output Arguments:
C  =================
C  None
C
C  Preconditions necessary before call:
C  ====================================
C SEGMENT MUST BE OPEN
C
C  Author:
C  =======
C Lupe Rosas (from CDF routine by A. Beretvas)
C
C  Revision History:
C  =================
C April 3, 1989
C
C======================================================================

      IMPLICIT NONE

C  Local Declarations:
C  ===================
      INTEGER MGTSEG,MCLAOS,MCLACS    ! FUNCTION NAMES
      REAL XMIN,XMAX,YMIN,YMAX
      REAL SCALEX,SCALEY              !FULL SIZE OF AXIS IN CM
      CHARACTER*8 HOR,VER             !WRITE SIZE IN CM
      REAL XUP,YUP,ZUP
      REAL XNORM,YNORM,ZNORM
      REAL XI,XF,XF0,XF1,XF2,XF3
      REAL ZI,ZF,ZF0,ZF1,ZF2,ZF3

      REAL YI,YF,YF0,YF1              !COORDINATES USED IN DRAWING
      REAL XXI,XXF,XXF0,XXF1
      REAL DELX,DELY                  !SCALE SIZE 1/40 OF FULL SCALE
      CHARACTER*3 COLOR
C  EXTERNAL Declarations:
C  ======================


C  Data Statements:
C  ================

C  Executable Code:
C  ================

      CALL J4RGET(1,XMIN,XMAX,YMIN,YMAX)
      SCALEX=XMAX-XMIN
      SCALEY=YMAX-YMIN
      CALL J3RGET(9,XUP,YUP,ZUP)      !GET UP VECTOR
      CALL J3RGET(8,XNORM,YNORM,ZNORM) !GET NORMAL VECTOR
C
      DELX=.010*SCALEX
      DELY=.015*SCALEY
      CALL JSIZE(DELX,DELY)
      XI=XMAX-23.*DELX  ! X val for origen
      XF=XI+4.*DELX     ! X val for the end of the x-axis 
      XF1=XI+3.*DELX    ! X val for SCALERX and Y label
      YI=YMAX-7.*DELY   ! Y val for origen
      YF=YI+4.*DELY     ! Y val for the end of the y-axis
      YF0=YI-1.*DELY    ! Y val for XDIV label
      YF1=YF0-1.*DELY   ! Y val for YDIV label
      IF(YUP.EQ.1.AND. ((ZNORM.EQ.1) .OR. (ZNORM.EQ.-1)) ) THEN   !X-Y AXIS
        COLOR = 'RED'
        CALL PXCOLR(COLOR)                    ! X-AXIS=RED
        CALL JBASE(1.,0.,0.)
        CALL JPLANE(0.,1.,0.)
        CALL J3MOVE(XI,YI,0.)
        CALL J3DRAW(XF,YI,0.)
        CALL J3MOVE(XF,YI,0.)
        CALL JHSTRG(' > X')
        CALL J3MOVE(XI,YF0,0.)
        CALL J3STRG(' XDIV=')
        CALL J3MOVE(XF1,YF0,0.)
        WRITE(HOR,900) SCALEX
  900   FORMAT(F8.1)
        CALL J3STRG(HOR)
        CALL J3MOVE(XI,YF1,0.)
        COLOR = 'BLU'
        CALL PXCOLR(COLOR)                      !Y-AXIS BLUE
        CALL J3STRG(' YDIV=')
        CALL J3MOVE(XF1,YF1,0.)
        WRITE(VER,900) SCALEY
        CALL J3STRG(VER)
        CALL JBASE(0.,1.,0.)
        CALL JPLANE(-1.,0.,0.)

        CALL J3MOVE(XI,YI,0.)
        CALL J3DRAW(XI,YF,0.)
        CALL JSIZE(DELY,DELX)
        CALL J3MOVE(XI,YF,0.)
        CALL JHSTRG(' > ')
        CALL JBASE(1.,0.,0.)
        CALL JPLANE(0.,1.,0.)
        CALL J3MOVE(XI,YF+2.,0.)
        CALL JHSTRG('Y')
        COLOR = 'GRE'
        CALL PXCOLR(COLOR)                   ! ARROW Z GREEN 
        CALL JCMARK(4)                          !SYMBOL CIRCLE
        CALL J3MARK(XI,YI,0.)
C
      ELSEIF(YUP.EQ.1.AND.XNORM.EQ.-1) THEN   !Y-Z AXIS
        ZI=XI
        ZF=XF
        ZF1=XF1
        COLOR = 'GRE'
        CALL PXCOLR(COLOR)                      ! Z-AXIS GREEN 
        CALL JBASE(0.,0.,1.)
        CALL JPLANE(0.,1.,0.)
        CALL J3MOVE(0.,YI,ZI)
        CALL J3DRAW(0.,YI,ZF)
        CALL J3MOVE(0.,YI,ZI)
        CALL JHSTRG(' > Z')
        CALL J3MOVE(0.,YF0,ZI)
        CALL J3STRG('ZDIV=')
        CALL J3MOVE(0.,YF0,ZF1)
        WRITE(HOR,900)SCALEX
        CALL J3STRG(HOR)
        COLOR = 'BLU'
        CALL PXCOLR(COLOR)              !Y-AXIS BLUE        
        CALL J3MOVE(0.,YF1,ZI)
        CALL J3STRG('YDIV=')
        CALL J3MOVE(0.,YF1,ZF1)
        WRITE(VER,900)SCALEY
        CALL J3STRG(VER)
        CALL JBASE(0.,1.,0.)
        CALL JPLANE(0.,0.,-1.)
        CALL J3MOVE(0.,YI,ZI)
        CALL J3DRAW(0.,YF,ZI)
        CALL JSIZE(DELY,DELX)
        CALL JHSTRG(' > ')
C
        CALL JBASE(0.,0.,1.)
        CALL JPLANE(0.,1.,0.)
        CALL J3DRAW(0.,YF+2.,ZI)
        CALL JHSTRG('Y')
        COLOR = 'RED'
        CALL PXCOLR(COLOR)              ! ARROW X RED 
        CALL JCMARK(5)                  ! SYMBOL CIRCLE
        CALL J3MARK(0.,YI,ZI)
C
      ELSEIF(XUP.EQ.1.AND.YNORM.EQ.1) THEN    !Z-X AXIS
        ZI=XI
        ZF=XF
        ZF1=XF1
        XXI=YI
        XXF=YF
        XXF0=YF0
        XXF1=YF1
        COLOR = 'GRE'
        CALL PXCOLR(COLOR)              !  Z -AXIS GREEN 
        CALL JBASE(0.,0.,1.)
        CALL JPLANE(1.,0.,0.)
        CALL J3MOVE(XXI,0.,ZI)
        CALL J3DRAW(XXI,0.,ZF)
        CALL J3MOVE(XXI,0.,ZI)
        CALL JHSTRG(' > Z')
        CALL J3MOVE(XXF0,0.,ZI)
        CALL J3STRG('ZDIV=')
        CALL J3MOVE(XXF0,ZF1)
        WRITE(HOR,900)SCALEX
        CALL J3STRG(HOR)
        COLOR ='RED'
        CALL PXCOLR(COLOR)                      !X-AXIS RED 
        CALL J3MOVE(XXF1,0.,ZI)
        CALL J3STRG('XDIV=')
        CALL J3MOVE(XXF1,0.,ZF1)
        WRITE(VER,900)SCALEX
        CALL J3STRG(VER)
        CALL JBASE(1.,0.,0.)
        CALL JPLANE(0.,0.,-1.)
        CALL J3MOVE(XXI,0.,ZI)
        CALL J3DRAW(XXF,0.,ZI)
        CALL J3MOVE(XXI,0.,ZI)
        CALL JSIZE(DELY,DELX)
        CALL JHSTRG(' > ')
        CALL JBASE(0.,0.,1.)
        CALL JPLANE(1.,0.,0.)
        CALL J3MOVE(XXF+2.,0.,ZI)
        CALL JHSTRG('X')
C
        COLOR = 'BLU'
        CALL PXCOLR(COLOR)              !  ARROW Y BLUE 
        CALL JCMARK(4)                          !SYMBOL CIRCLE
        CALL J3MARK(XXI,0.,ZI)
      END IF
  999 CONTINUE      ! finished
C
      RETURN
      END

