      SUBROUTINE CRYPLT (LUN)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Plot cryostat using standard SRCP geometry
C-                         file
C-
C-   Inputs  : LUN         Unit number of input stream (CRYOSTAT)
C-   Outputs : None
C-   Controls: None
C-
C-   Created  10-OCT-1988   Harrison B. Prosper
C-   Updated   8-DEC-1989   Harrison B. Prosper  
C-      Made compatible with new RCP 
C-   Updated  13-Feb-1992   Herbert Greenlee
C-      Changed OPEN to D0OPEN
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL ACTIVE
      INTEGER      NSEC,IB,NPAR
      PARAMETER( NSEC = 200 )
      CHARACTER*1  COMMAND
      CHARACTER*4  SHAPE
      CHARACTER*32 NAME,CRYOST(NSEC)
      CHARACTER*40 STRING
      REAL         RAW(NSEC),Z(2000),Y(2000),Z0,DZ
      REAL         RMIN1,RMAX1,RMIN2,RMAX2,VALUE,CMINX,CMAXX
      REAL         RATIO,CMIN,CMAX,UMIN,UMAX,VMIN,VMAX,SIZE
      INTEGER      II,NN,I,J,K,L,M,N,IER,ISEC,LUN,IRAW(NSEC),NPT,NZ,IC
      INTEGER      KMIN,KMAX,IQUAD
      INTEGER      COLOR1,DEVICE
      LOGICAL OK
      PARAMETER( COLOR1 = 1 )
      PARAMETER( DEVICE = 1 )
      EQUIVALENCE (IRAW(1),RAW(1))
C----------------------------------------------------------------------
C
C ****  Get graphics setup parameters
C
      RATIO = 0.98275
      CMIN  = 0.0
      CMAX  = 400.0
      SIZE  = 0.85
C
      WRITE(6,10)
   10 FORMAT(1X,'Number of quadrants to draw (1,2) [1] > ',$)
      READ (5,20) IQUAD
   20 FORMAT(I10)
      IF ( IQUAD .LE. 1 ) THEN
        CMINX = CMIN
        CMAXX = CMAX
      ELSE
        CMINX =-CMAX
        CMAXX = CMAX
        CMAX = 2.0*CMAX
      ENDIF
C
C ****  Read in parameters from Parameter file and store
C       data in the SRCP bank which hangs off the bank SCPH.
C
C ****  Get section names
C
      CALL D0OPEN (LUN,'CRYOSTAT','I',OK)
      IF(.NOT.OK)GO TO 950
      CALL EZPAR  (LUN,'ARRAY',CRYOST,NN)
      CLOSE(UNIT=LUN)
      DO II =  1,NN
        WRITE(6,FMT='(1X,A32)') CRYOST(II)
      ENDDO
C
C ****  Read and decode parameter file
C
      WRITE(6,FMT='('' Decode file...'')')
      CALL D0OPEN (LUN,'CRYOSTAT','I',OK)
      IF(.NOT.OK)GO TO 950
      CALL EZRDF  (LUN)
      CLOSE(UNIT=LUN)
      WRITE(6,FMT='('' Done !!!'')')
C
C ****  Initialize DI3000
C
      CALL JBEGIN
      CALL JDINIT (DEVICE)              ! Initialize output device
      CALL JDEVON (DEVICE)
      CALL JVPORT (-SIZE,SIZE,-SIZE,SIZE)       ! Define DI3000 viewport
C
C ****  Map window into viewport
C
      UMIN = CMINX
      UMAX = CMAXX
      VMIN = CMIN*RATIO
      VMAX = CMAX*RATIO
      CALL JWINDO (UMIN,UMAX,VMIN,VMAX)
      CALL JWCLIP (.TRUE.) ! Enable clipping
C
C ****  Draw Border
C
      CALL JFRAME (DEVICE)
      CALL JOPEN
      CALL JCOLOR (COLOR1)
      CALL JMOVE (UMIN,VMIN)
      CALL JDRAW (UMAX,VMIN)
      CALL JDRAW (UMAX,VMAX)
      CALL JDRAW (UMIN,VMAX)
      CALL JDRAW (UMIN,VMIN)
      CALL JCLOSE
C
C ****  Get points from SRCP bank
C
      IB = 11
      DO 800 II =1,NN
        CALL WORD (CRYOST(II),I,J,L)
        CALL EZGET  (CRYOST(II)(1:L),IRAW(1),IER)
        IF ( IER .NE. 0 ) GOTO 900
C
C ****  Get shape name
C
        CALL UHTOC (IRAW(2),4,SHAPE,4)
        Z0   = RAW(IB-1) ! Z displacement
        NPAR = IRAW(IB)
C
C ****  Plot shape
C
        IF ( SHAPE .EQ. 'LINE' ) THEN
          NPAR = IRAW(1)
          IC = 8
          CALL JOPEN
          CALL JCOLOR (COLOR1)
          CALL JMOVE (RAW(IC+1),RAW(IC+2))
          DO 710 I =  2,NPAR
            IC = IC + 3
          CALL JDRAW (RAW(IC+1),RAW(IC+2))
  710     CONTINUE
          CALL JCLOSE
C
        ELSEIF ( SHAPE .EQ. 'TUBE' ) THEN
          RMIN1 = RAW(IB+1)
          RMAX1 = RAW(IB+2)
          DZ   = RAW(IB+3)
          Z(1) = Z0 - DZ
          Y(1) = RMIN1
          Z(2) = Z(1)
          Y(2) = RMAX1
          Z(3) = Z0 + DZ
          Y(3) = RMAX1
          Z(4) = Z(3)
          Y(4) = RMIN1
          CALL JOPEN
          CALL JCOLOR (COLOR1)
          CALL JPOLGN (Z,Y,4)
          CALL JCLOSE
C
        ELSEIF ( SHAPE .EQ. 'TRAP' ) THEN
C
C ****  Determine corners of trapezoid
C
          Z(1) = RAW(IB+1) + Z0
          Y(1) = RAW(IB+2)
          Z(2) = RAW(IB+3) + Z0
          Y(2) = RAW(IB+4)
          Z(3) = RAW(IB+5) + Z0
          Y(3) = RAW(IB+6)
          Z(4) = RAW(IB+7) + Z0
          Y(4) = RAW(IB+8)
          CALL JOPEN
          CALL JCOLOR (COLOR1)
          CALL JPOLGN (Z,Y,4)
          CALL JCLOSE
C
        ELSEIF ( SHAPE .EQ. 'POLY' ) THEN
C
          IC = IB
          N = NPAR/4
          NPT = 2*N
          DO 700 I =  1,N
            Z(I) = RAW(IC+1) + Z0
            Y(I) = RAW(IC+2)
            Z(NPT-I+1) = RAW(IC+3) + Z0
            Y(NPT-I+1) = RAW(IC+4)
            IC = IC + 4
  700     CONTINUE
          CALL JOPEN
          CALL JCOLOR (COLOR1)
          CALL JPOLGN (Z,Y,NPT)
          CALL JCLOSE
C
        ELSEIF ( SHAPE .EQ. 'PCON' ) THEN
          NZ = RAW(IB+3)
          NPT = 2*NZ
          IC = IB
          DO I =  1,NZ
            KMIN = NPT-I+1
            KMAX = I
            IC = IC + 3
            Z(KMIN) = RAW(IC+1) + Z0
            Y(KMIN) = RAW(IC+2)
            Y(KMAX) = RAW(IC+3)
            Z(KMAX) = Z(KMIN)
          ENDDO
          CALL JOPEN
          CALL JCOLOR (COLOR1)
          CALL JPOLGN (Z,Y,NPT)
          CALL JCLOSE
        ENDIF
  800 CONTINUE
C
  900 CONTINUE
      CALL JPAUSE (DEVICE)
      CALL JDEVOF (DEVICE)
      CALL JDEND  (DEVICE)
      CALL JEND
      GOTO 999
C
  950 CONTINUE
      WRITE (6,FMT='('' ERROR OPENING FILE CRYOSTAT '')')
C
  999 RETURN
      END
