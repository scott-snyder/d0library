      SUBROUTINE PUROTATE(XCAM,UPVEC,XP)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Handle 3D Rotation for standard DI3000.
C-
C-   Input   : None
C-                       
C-   Outputs : XCAM(1)  - x of camera position after rotating
C-             XCAM(2)  - y of           "            "
C-             XCAM(3)  - z of           "            "
C-             UPVEC(1) - x of up vector after rotating
C-             UPVEC(2) - y of       "          "
C-             UPVEC(3) - z of       "          "
C-             XP(1..3) - Window coordinates of selected point.
C-
C-   Warning!  : DON'T call this routine between JROPEN and JRCLOS.
C-
C-   Created  28-MAY-1991   Nobuaki Oshima & Michael Peters(DI3000 part)
C-   Updated  30-MAY-1991   LUPE HOWELL    - Use INTMSG etc. 
C-   Modified 27-DEC-1991   Nobuaki Oshima 
C-                 Improve ENTRY PU_DISCONN_RSEG part.
C-   Modified 06-JAN-1992   Nobuaki Oshima 
C-                 Fix the cylinder size problem. 
C-   Updated  13-MAY-1992   Lupe Howell and Harrison B. Prosper
C-                 Now only for standard DI3000 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C-
      REAL XCAM(3),UPVEC(3),XP(3)
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBPIX.INC'
      INCLUDE 'D0$INC:PXPARA.INC'
      INCLUDE 'D0$INC:PI.DEF'
C----------------------------------------------------------------------
      INTEGER I,ISEG
      INTEGER IRESP,IR
      INTEGER IDUM,NSTR,NSTRT,INSTR,NSTRE
      INTEGER NSEG
C- 
      INTEGER NS,IER
      INTEGER NCLMAX,NLINE
      PARAMETER (NCLMAX=100,NLINE=10)
      REAL AZCYL
      REAL XMIN,XMAX,YMIN,YMAX
      REAL XC(3),VC(3),VLEN
      REAL RCAM,SMIN
      REAL VMOD,VN(3),VP(3),S(3),PHL,PHH,PHI,XCYL,YCYL,ZCYL,RCYL,SCL
      REAL XVIN,YVIN
C-
C----------------------------------------------------------------------
C-
C--- Get 2D viewplane window boundaries (world coordinates)
C-
      CALL J4RGET(1,XMIN,XMAX,YMIN,YMAX)
      RCYL  = (YMAX-YMIN)*.32
      AZCYL = (XMAX-XMIN)*.38
C-
C--- Get the current view point
C-
      CALL J3RGET(7,VP(1),VP(2),VP(3))
C-
C--- Get the current viewplane normal vector
C-
      CALL J3RGET(8,VN(1),VN(2),VN(3))
C-
      DO I=1,3
        XC(I) = VP(I) - VN(I)
      ENDDO
      IF(XC(1).NE.0. .OR. XC(2).NE.0. ) THEN
        PHI=ATAN2(XC(2),XC(1))
      ELSE
        PHI=0.
      ENDIF
      PHL=PHI-HALFPI
      PHH=PHI+HALFPI
C-
C--- Draw a partial cylinder to guide the user
C-
      CALL JOPEN
      IF(XC(3).GT.0.) THEN
        ZCYL=AZCYL
      ELSE
        ZCYL=-AZCYL
      ENDIF
      CALL PXCOLR('FOR')
      CALL JCIRCL(0.,0., ZCYL,RCYL,0)
      CALL JARC(0.,0.,-ZCYL,RCYL,0,PHL/RADIAN,PHH/RADIAN)
      DO I=1,NLINE
        PHI=PHL+(I-1)*PI/(NLINE-1)
        XCYL=RCYL*COS(PHI)
        YCYL=RCYL*SIN(PHI)
        CALL J3MOVE(XCYL,YCYL,ZCYL)
        CALL J3DRAW(XCYL,YCYL,-ZCYL)
      ENDDO
      CALL JCLOSE
C-
C--- Determine desired rotation angles
C--- Let user select a point in the viewplane
C-
      CALL STAMSG(' Click to specify rotation',.TRUE.)
      CALL PULOC3 (0., 0., XP(1), XP(2), XP(3))
      IF ( IER .NE. 0 ) THEN
        CALL INTMSG(' **** Point outside viewport ****')
        GO TO 999
      ENDIF
      VLEN=0.
C-
C--- Calculate the origin and direction of the ray through the selected point
C-
      DO I=1,3
        VC(I) = VN(I)
        XC(I) = XP(I) - VN(I)
        VLEN  = VLEN + VC(I)**2
      ENDDO
      VLEN=SQRT(VLEN)
      DO I=1,3
        VC(I)=VC(I)/VLEN
      ENDDO
C-
C--- Find intersection of line of sight with cylinder
C-
      SMIN=1.E6
      CALL CLNRD(XC,VC,RCYL,NS,S)
      IF(NS.GT.0) THEN
        DO I=1,NS
          XCAM(3)=XC(3)+S(I)*VC(3)
          IF(S(I).GT.0..AND.ABS(XCAM(3)).LT.AZCYL.AND.S(I).LT.SMIN)
     &       SMIN=S(I)
        ENDDO
      ENDIF
C-
C--- Find intersecton of line of sight with near end of cyl
C-
      CALL CLNZD(XC,VC,ZCYL,NS,S)
      IF(NS.GT.0)THEN
        XCAM(1)=XC(1)+S(1)*VC(1)
        XCAM(2)=XC(2)+S(1)*VC(2)
        RCAM=SQRT(XCAM(1)**2+XCAM(2)**2)
        IF(S(1).GT.0..AND.RCAM.LT.RCYL.AND.S(1).LT.SMIN) SMIN=S(1)
      ENDIF
      IF(SMIN.GT.1E5) THEN
        CALL INTMSG(' Line of sight misses cylinder')
        GO TO 999
      ENDIF
      RCAM=0.
      DO I=1,3
        XCAM(I)=XC(I)+SMIN*VC(I)
        RCAM=RCAM+XCAM(I)**2
      ENDDO
      RCAM=SQRT(RCAM)
      SCL=VMOD(XCAM(1),3)/RCAM
C-
C--- Determine new camera position in cartesian coords
C-
      DO I=1,3
        XCAM(I)=SCL*XCAM(I)
      ENDDO
C-
C--- Update PIXIE up vector so that z-axis points to right
C--- ( UPVEC = CROSS(NORML,ZAXIS) )
C-
      VLEN=SQRT(XCAM(2)**2+XCAM(1)**2)
      IF(VLEN.GT.0.) THEN
        UPVEC(1) =  XCAM(2)/VLEN
        UPVEC(2) = -XCAM(1)/VLEN
        UPVEC(3) = 0.
      ELSE
        UPVEC(1) = 0.
        UPVEC(2) = 1.
        UPVEC(3) = 0.
      ENDIF
  999 RETURN
      END
