      SUBROUTINE PU3ROT
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Handle 3D Rotation on EVANS & SUTHERLAND and
C-                         the standard DI3000 device.
C-
C-   Warning  : DON'T call this routine between JROPEN and JRCLOS.
C-
C-   Created   3-NOV-1990   Nobuaki Oshima
C-   Modified  8-MAR-1991   Nobuaki Oshima ( Fixed a bug in DI3000 part
C-                                           and added ENTRY PU3DIS.)
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBPIX.INC'
      INCLUDE 'D0$INC:PXPARA.INC'
      INCLUDE 'D0$INC:PI.DEF'
      INCLUDE 'D0$PARAMS:ESCAPE_CODE_NAMES.DEF'
C-
      INTEGER I,ISEG,ILISTS(1024)
      INTEGER IRESP,IR
      INTEGER IDUM,NREAL,NSTR,NSTRT,INSTR,NSTRE
      INTEGER LISSTN(10),NSTREA(10)
      INTEGER NSEG,RSEGTYP(10)
      REAL    RDUM(1),RLEVEL, RLIST(10)
      LOGICAL LVIEW3D
C- for PU3RTN
      INTEGER NCLMAX,NLINE
      PARAMETER (NCLMAX=100,NLINE=10)
      REAL AZCYL
      PARAMETER (AZCYL=300.)
      INTEGER ICHAR,NS
      INTEGER IDX,IER,NPORT
      REAL XC(3),VC(3),VLEN,XP(3),PHICAM
      REAL XCAM(3),UPVEC(3),RCAM,SMIN
      REAL VMOD,VN(3),VP(3),S(3),PHL,PHH,PHI,XCYL,YCYL,ZCYL,RCYL,SCL
      REAL XVIN,YVIN
      REAL VPORTXMIN,VPORTXMAX,VPORTYMIN,VPORTYMAX
      CHARACTER*40 TXTITM
      CHARACTER*32 ACTPAC,COMMAND
      CHARACTER*1  CHOICE
C-
      DATA NREAL,RLIST(1)  /1, 1.0E5/
      DATA TXTITM/'Enter R for Rotate : C for Continue >'/
C----------------------------------------------------------------------
C-
C--- If device was NOT EAS(UIS), jump to DI3000 part...
      CALL JIQDIL(RLEVEL)
      IF (RLEVEL .GE. 0.)   GO TO 500
C-
C--- Setup all retain segments
      CALL VZERO(ILISTS,1024)
C--- To setup the transfomation network.
      CALL JIQDEV(1,17,IRESP)
      IF(IRESP.NE.1) THEN
        CALL PUMESS(' THIS DEVICE DOES NOT HAVE 3D CAPABILITY')
        GOTO 999
      END IF
      CALL JDD3D(.TRUE.)
      CALL JESCAP(SETUP_3D, 0, 0,IDUM,RDUM)
C-
C--- Connect segments for rotating( RLEVEL=-2 only )
C--- Apply all retained segments(18-DEC-1990)
C-
      IF (RLEVEL .EQ. -2.)   THEN
        ISEG = 0
        CALL PU_GET_SEGMENT_TYPE(NSEG,RSEGTYP)
        DO I=1,NSEG
          IF (RSEGTYP(I) .EQ. -1) THEN
            ISEG = ISEG + 1
            ILISTS(ISEG) = I
          ENDIF
        ENDDO
C-
C--- To connect the segments to the transformation network.
        NSTR = 0
C-
        DO I=1,NSEG
          NSTR = NSTR+1
          LISSTN(NSTR) = ILISTS(I)
        ENDDO
C-
        NSTRT=NSTR/10+1
        DO 200 IR=1,NSTRT
          INSTR =1 + (IR-1)*10
          IF(IR.EQ.NSTRT) THEN
            NSTRE=10 + NSTR- 10*NSTRT
          ELSE
            NSTRE=10
          END IF
          NSTREA(IR)=NSTRE
          CALL JESCAP(CONNECT_3D,NSTRE,NREAL,LISSTN(INSTR),RLIST)
  200   CONTINUE
      ENDIF
C-
      GOTO 999
C-
C---    Rotation with the standard DI3000(was SUBROUTINE PU3RTN)
C-
  500 CONTINUE
C-
C
C ****  Getting Input from the user to rotate or not...
C
      CALL OUTMSG('1')
      CALL GETPAR(1,TXTITM,'U',CHOICE)
C
      IF(CHOICE .EQ. 'C') GOTO 999
C
      CALL PU_GET_ACTIVE_COMMAND(COMMAND)
      CALL PU_GET_SCREEN_INDEX(COMMAND, IDX, IER)
C
C ****  Get the current view point
C
      CALL J3RGET(7,VP(1),VP(2),VP(3))
C ***
C ***   Get the current viewplane normal vector
C ***
      CALL J3RGET(8,VN(1),VN(2),VN(3))
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
C
C ****  Draw a partial cylinder to guide the user
C
      CALL JOPEN
      RCYL=250.
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
C
C ****   Determine desired rotation angles
C ****  Let user select a point in the viewplane
      CALL PUMESS('Click to specify rotation')
      CALL PULOC3 (0., 0., XP(1), XP(2), XP(3))
      IF ( IER .NE. 0 ) THEN
        CALL PUMESS('**** Point outside viewport ****')
        GO TO 999
      ENDIF
C
      CALL PU_GET_SCREEN_PARAM(IDX,'VIEW3D',LVIEW3D,IER)
      IF (.NOT. LVIEW3D ) THEN
        CALL PUMESS('This is not a 3D view')
        GO TO 999
      ENDIF
      VLEN=0.
C
C ****    Calculate the origin and direction of the ray through
C ****   the selected point
C
      DO I=1,3
        VC(I) = VN(I)
        XC(I) = XP(I) - VN(I)
        VLEN  = VLEN + VC(I)**2
      ENDDO
      VLEN=SQRT(VLEN)
      DO I=1,3
        VC(I)=VC(I)/VLEN
      ENDDO
C
C ****  Find intersection of line of sight with cylinder
      SMIN=1.E6
      CALL CLNRD(XC,VC,RCYL,NS,S)
      IF(NS.GT.0) THEN
        DO I=1,NS
          XCAM(3)=XC(3)+S(I)*VC(3)
          IF(S(I).GT.0..AND.ABS(XCAM(3)).LT.AZCYL.AND.S(I).LT.SMIN)
     &       SMIN=S(I)
        ENDDO
      ENDIF
C
C ****  Find intersecton of line of sight with near end of cyl
C
      CALL CLNZD(XC,VC,ZCYL,NS,S)
      IF(NS.GT.0)THEN
        XCAM(1)=XC(1)+S(1)*VC(1)
        XCAM(2)=XC(2)+S(1)*VC(2)
        RCAM=SQRT(XCAM(1)**2+XCAM(2)**2)
        IF(S(1).GT.0..AND.RCAM.LT.RCYL.AND.S(1).LT.SMIN) SMIN=S(1)
      ENDIF
      IF(SMIN.GT.1E5) THEN
        CALL PUMESS('Line of sight misses cyl')
        GO TO 999
      ENDIF
      RCAM=0.
      DO I=1,3
        XCAM(I)=XC(I)+SMIN*VC(I)
        RCAM=RCAM+XCAM(I)**2
      ENDDO
      RCAM=SQRT(RCAM)
      SCL=VMOD(XCAM(1),3)/RCAM
C
C ****  Determine new camera position in cartesian coords
C
      DO I=1,3
        XCAM(I)=SCL*XCAM(I)
      ENDDO
C
C ****  Modify PIXIE camera position
C
      CALL PU_SET_SCREEN_PARAM(IDX,'CAMX',XCAM(1),IER)
      CALL PU_SET_SCREEN_PARAM(IDX,'CAMY',XCAM(2),IER)
      CALL PU_SET_SCREEN_PARAM(IDX,'CAMZ',XCAM(3),IER)
C
C ****  Modify PIXIE up vector so that z-axis points to right
C ****  UPVEC = CROSS(NORML,ZAXIS)
C
      VLEN=SQRT(XCAM(2)**2+XCAM(1)**2)
      IF(VLEN.GT.0.) THEN
        UPVEC(1) =  XCAM(2)/VLEN
        UPVEC(2) = -XCAM(1)/VLEN
        UPVEC(3) = 0.
        CALL PU_SET_SCREEN_PARAM(IDX,'UPVECX',UPVEC(1),IER)
        CALL PU_SET_SCREEN_PARAM(IDX,'UPVECY',UPVEC(2),IER)
        CALL PU_SET_SCREEN_PARAM(IDX,'UPVECZ',UPVEC(3),IER)
      ELSE
        UPVEC(1) = 0.
        UPVEC(2) = 1.
        UPVEC(3) = 0.
      ENDIF
      RETURN
C-
C--- Disconnect retained segments
C-
      ENTRY PU3DIS
C-
      CALL JESCAP(DISCONNECT_3D,NSTRE,NREAL,LISSTN(INSTR),RLIST)
  999 RETURN
      END
