      SUBROUTINE J_PROJS
C  SET UP ALL VIEWING PROJECTIONS AND TRANSFORMATIONS
      INCLUDE 'fgl.h'
      INCLUDE 'D0$GRAPHICS_UTIL$SGIDI3:DI3GL.INC'
      LOGICAL FIRST
      SAVE FIRST,RADTOD
      DATA FIRST/.TRUE./
C
C  TENTHS OF A DEGREE
      IF(FIRST) THEN
        FIRST=.FALSE.
        RADTOD=1800./3.1415927
        WNEAR=-999999.
        WFAR=999999.
      ENDIF
C
C  FIND THE U AND V UNIT VECTORS FROM THE CURRENT N VECTOR (NORM.)
C  AND UP VECTOR (NOT NECESSARILY NORM).
      IF(IRIGHT.EQ.0) THEN
        CALL J_CROSS(UPVEC,NVEC,UVEC)
      ELSE
        CALL J_CROSS(NVEC,UPVEC,UVEC)
      ENDIF
      RU=UVECX**2+UVECY**2+UVECZ**2
      IF(RU.LE.0.) RETURN
      RU=SQRT(RU)
      UVECX=UVECX/RU
      UVECY=UVECY/RU
      UVECZ=UVECZ/RU
      IF(IRIGHT.EQ.0) THEN
        CALL J_CROSS(NVEC,UVEC,VVEC)
      ELSE
        CALL J_CROSS(UVEC,NVEC,VVEC)
      ENDIF
C********************
C!!!IMPLEMENT MODELLING AND PERSPECTIVE TRANSFORMATIONS!!!
C  THIS IS A PERSPECTIVE TRANSFORMATION.
C  (OBLIQUE TRANSFORMATIONS NOT IMPLEMENTED YET)
C      IF(IPROJ.EQ.3) THEN
C        SFACTR=VDIST/(VDIST+ZP)
C        XP=SFACTR*XP
C        YP=SFACTR*YP
C      ENDIF
C!!!  PERSPECTIVE PROJECTION
C  100 FOVY=ATAN(.5*DV/ABS(VDIST))*RADTOD
C      ASPECT=1./RASP
C      WNEAR=0.
C      WFAR=999999.
C      CALL PERSPE(FOVY,ASPECT,WNEAR,WFAR)
C********************
C
C  TRANSLATE TO ORIGIN OF UVN SYSTEM (THE VIEWPOINT)
      CALL D_MATUNI(TTRAN)
      TTRAN(1,4)=-XVW
      TTRAN(2,4)=-YVW
      TTRAN(3,4)=-ZVW
C
C  ROTATE TO UVN AXES
      CALL D_MATUNI(TUVN)
      TUVN(1,1)=UVECX
      TUVN(1,2)=UVECY
      TUVN(1,3)=UVECZ
      TUVN(2,1)=VVECX
      TUVN(2,2)=VVECY
      TUVN(2,3)=VVECZ
      TUVN(3,1)=VNORMX
      TUVN(3,2)=VNORMY
      TUVN(3,3)=VNORMZ
C
C  FORM VIEWING TRANSFORMATION MATRIX
      CALL D_MATCPY(TTRAN,TVIEW)
      CALL D_MATMUL(TUVN,TVIEW)
      call d_dmpmat('J_PROJS-TTRAN:',TTRAN)
      call d_dmpmat('J_PROJS-TUVN:' ,TUVN )
      call d_dmpmat('J_PROJS-TVIEW:',TVIEW)
C
      IF(HCPY) THEN
C  COMPLETE TOTALH FOR HARDCOPY BY APPLYING DEVICE TRANSFORMATION.
        CALL D_MATCPY(TVIEW,TTOTAH)
        CALL D_MATMUL(TDEVIC,TTOTAH)
        call d_dmpmat('J_PROJS-TDEVIC:',TDEVIC)
        call d_dmpmat('J_PROJS-TTOTAH:',TTOTAH)
        RETURN
      ENDIF
C
C  CALCULATE DEVICE TRANSFORMATION MATRIX
      CALL J_DEVTRN
C
C  COMPLETE TTOTAL FOR WORKSTATION BY APPLYING DEVICE TRANSFORMATION.
      CALL D_MATCPY(TVIEW,TTOTAL)
      CALL D_MATMUL(TSCREE,TTOTAL)
      call d_dmpmat('J_PROJS-TSCREE:',TSCREE)
      call d_dmpmat('J_PROJS-TTOTAL:',TTOTAL)
C  SET THE NEAR AND FAR PLANES UNTIL FURTHER NOTICE
      CALL ORTHO(-1.,1.,-1.,1.,WNEAR,WFAR)
C  LOAD UP THE COMPLETE TRANSFORMATION
      CALL MULTMA(TTOTAL)
      RETURN
      END
