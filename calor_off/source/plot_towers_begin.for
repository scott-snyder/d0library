      LOGICAL FUNCTION PLOT_TOWERS_BEGIN
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created   4-OCT-1989   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INTEGER ERROR,I,J,K,L,N,II,JJ,KK
      INTEGER IETA,IPHI,LAYER(32),NLAYER,POINTS,JETA,JPHI
      INTEGER ETA1,ETA2,IETA_OFFSET,IETA_STEP,IETA_SKIP
      INTEGER PHI1,PHI2,IPHI_OFFSET,IPHI_STEP,IPHI_SKIP,DEV
C
      INTEGER DEVICE
C
C ****  Get range of ETA, PHI and LAYER indices
C
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
C
      REAL    UMIN,UMAX,VMIN,VMAX,PHIMIN,PHIMAX,ETAMIN,ETAMAX
      REAL    SIZE,SCALE,DX,DY,XT,YT,CHARX,CHARY,XETA,XPHI
      REAL    E(7),X,Y,Z,ETA,PHI,THETA,XX,YY,ZZ,DPHI,DETA,DETA1,DPHI1
      REAL    PHI_TICK,PHI_STEP,ETA_TICK,SCAL,XTT,YTT,ETMIN,ETMIN1
      REAL    RADIUS,RADIUS1,RATIO,SIZEY,SIZEX
C
      LOGICAL ACTIVE, PLOT_TOWERS_VALUES, PAUSE, PAUS
      CHARACTER*80 STRING
C
      INTEGER LINE_STYLE,LINE_WIDTH,LL
      INTEGER NORMAL,RED,GREEN,YELLOW,BLUE,MAGENTA,CYAN,WHITE,BLACK
      INTEGER COMPLEMENT,COLOR,MAX_INTENSITY,MIN_INTENSITY
      INTEGER POINT,PLUS,ASTERISK,BIGO,BIGX
C
      PARAMETER( LINE_STYLE = 0 )
      PARAMETER( LINE_WIDTH = 0 )
      PARAMETER( POINT = 1 )
      PARAMETER( PLUS = 2 )
      PARAMETER( ASTERISK = 3 )
      PARAMETER( BIGO = 4 )
      PARAMETER( BIGX = 5 )
C
      PARAMETER( NORMAL = 0 )
      PARAMETER( RED    = 1 )
      PARAMETER( GREEN  = 2 )
      PARAMETER( YELLOW = 3 )
      PARAMETER( BLUE   = 4 )
      PARAMETER( MAGENTA= 5 )
      PARAMETER( CYAN   = 6 )
      PARAMETER( WHITE  = 7 )
      PARAMETER( BLACK  = 8 )
      PARAMETER( COMPLEMENT  = 9 )
      PARAMETER( MIN_INTENSITY = 1 )
      PARAMETER( MAX_INTENSITY = 32767 )
C
      INTEGER ICOLOR,GRID_COLOR,IER,SWITCH,SWITCH1
      CHARACTER*3 COLOUR(0:9),ACOLOR
      DATA COLOUR
     &  /'NOR','RED','GRE','YEL','BLU','MAG','CYA','WHI','BLA','COM'/
C
      INCLUDE 'D0$INC:PI.DEF'
      LOGICAL FIRST
      DATA FIRST/.TRUE./
      SAVE FIRST
C----------------------------------------------------------------------
      PLOT_TOWERS_BEGIN = .TRUE.
C
      IF ( FIRST ) THEN
        FIRST = .FALSE.
C
        CALL INZSTP                     ! Initialize /ZEBSTP/
C
        CALL INRCP  ('PLOT_TOWERS_RCP',IER)
        CALL EZPICK ('PLOT_TOWERS_RCP')
        CALL EZGETS ('GRID_COLOR',1,ACOLOR,LL,IER)
        CALL EZGET  ('RADIUS',RADIUS,IER)
        CALL EZGET  ('ASPECT_RATIO',RATIO,IER)
        CALL EZGET  ('SIZE',SIZE,IER)
        CALL EZGET  ('CHAR_SIZE_HORIZONTAL',CHARX,IER)
        CALL EZGET  ('CHAR_SIZE_VERTICAL',CHARY,IER)
C
        CALL EZGET  ('TOWER_TYPE',SWITCH,IER)
        CALL EZGET  ('MINIMUM_TOWER_ET',ETMIN,IER)
        CALL EZGET  ('PAUSE',PAUSE,IER)
        CALL EZRSET
C
        GRID_COLOR = BLACK
        DO I = 0,9
          IF ( ACOLOR .EQ. COLOUR(I) ) THEN
            GRID_COLOR = I
            GOTO 10
          ENDIF
        ENDDO
   10   CONTINUE
C
C ****  Setup defaults
C
        IF ( RATIO  .LE. 0. ) RATIO  = 0.70
        IF ( RADIUS .LE. 0. ) RADIUS = 0.70
        IF ( SIZE   .LE. 0. ) SIZE   = 0.95
        IF ( CHARX  .LE. 0. ) CHARX  = 1.40
        IF ( CHARY  .LE. 0. ) CHARY  = 2.50
      ENDIF
C
C *************************************
C ****  SETUP Graphics parameters  ****
C *************************************
      DEVICE = 1
C
C ****  Setup range of physics indices
C
      ETA1   =-NETAL
      ETA2   = NETAL
      PHI1   = 1
      PHI2   = NPHIL
C
C ****  Setup range of world coordinates
C
      SCALE  = 180.0/PI
      PHIMIN = 0.0
      PHIMAX = 2.0*PI
      ETAMIN =-4.5
      ETAMAX = 4.5
      JETA   = 45
      JPHI   = 32
C
      UMIN = ETAMIN-0.6
      UMAX = ETAMAX+0.2
      VMIN = PHIMIN-15./SCALE
      VMAX = PHIMAX+20./SCALE
C
C ****  Determine character size
C
      CHARX = 0.01*CHARX*(UMAX-UMIN)    ! Convert to world coordinate sizes
      CHARY = 0.01*CHARY*(VMAX-VMIN)
C
C ****  Set values for labels
C
      IETA_OFFSET = 10
      IETA_STEP   = 10
      IETA_SKIP   =  5
      ETA_TICK    = 0.01*(PHIMAX-PHIMIN)        ! Tick length in phi direction
      DETA        = (ETAMAX-ETAMIN)/(2*JETA)
C
      IPHI_OFFSET =  8
      IPHI_STEP   =  8
      IPHI_SKIP   =  5
      PHI_TICK    = 0.01*(ETAMAX-ETAMIN)        ! Tick length in eta direction
      DPHI        = (PHIMAX-PHIMIN)/(2*JPHI)
C
      PHI_STEP    = DPHI*IPHI_STEP
C
      SIZEX = SIZE
      SIZEY = SIZE
      IF ( RATIO .LT. 1.0 ) THEN
        SIZEY = SIZE*RATIO
      ELSE
        SIZEX = SIZE/RATIO
      ENDIF
C
C *************************************
C ****  Initialize DI3000          ****
C *************************************
C
      CALL JBEGIN
      CALL JDINIT (DEVICE)              ! Initialize output device
      CALL JDEVON (DEVICE)
      CALL JVPORT (-SIZEX,SIZEX,-SIZEY,SIZEY)       ! Define DI3000 viewport
      CALL JWINDO (UMIN,UMAX,VMIN,VMAX)
      CALL JWCLIP (.TRUE.) ! Enable clipping
      CALL JDSIZE (CHARX,CHARY)         ! Set default character size
C
C ****  Open retained segment
C
      CALL JFRAME (DEVICE)

      CALL JROPEN (1)
C
C ****  Draw border
C
      CALL JINTEN (MIN_INTENSITY)
      CALL JCOLOR (RED)
      CALL JRECT (UMIN,VMIN,UMAX,VMAX)
C
C ****  Label eta axis
C
      XT = 0.0
      YT = 0.5*(PHIMIN-VMIN) + VMIN
      CALL PLOT_WRITE_STRING(MAX_INTENSITY,MIN_INTENSITY,
     &                      GREEN,GRID_COLOR,XT,YT,'Eta')
C
      CALL JLSTYL (LINE_STYLE)                   ! Select line style
      CALL JLWIDE (LINE_WIDTH)
C
C ****  Draw eta lines
C
      X = ETAMIN - DETA
      J = 0
      DO IETA =  -JETA,JETA
        X = X + DETA
        Y = PHIMIN
C
        IF ( MOD(IETA+IETA_OFFSET,IETA_STEP) .EQ. 0 ) THEN
          DY = ETA_TICK
          J = J + 1
          IF ( J .NE. IETA_SKIP ) THEN
            N = X + SIGN(0.05,X)
            CALL VNUMI (1,N,'(',' ',')',STRING,L)
            CALL PLOT_WRITE_STRING(MAX_INTENSITY,MIN_INTENSITY,
     &                            GREEN,GRID_COLOR,X,YT,STRING(2:L-1))
          ENDIF
        ELSE
          DY = 0.0
        ENDIF
C
        CALL JMOVE (X,Y-DY)
        Y = PHIMAX
        CALL JDRAW (X,Y+DY)
      ENDDO
C
C ****  Label phi axis
C
      CALL JLSTYL (0)                   ! Select line style
      CALL JLWIDE (0)
C
      XT = 0.5*(ETAMIN-UMIN) + UMIN
      YT = 0.5*(PHIMAX-PHIMIN) + PHIMIN
      CALL PLOT_WRITE_STRING(MAX_INTENSITY,MIN_INTENSITY,
     &                      GREEN,GRID_COLOR,XT,YT,'Phi')
C
C ****  Draw phi lines
C
      CALL JLSTYL (LINE_STYLE)                   ! Select line style
      CALL JLWIDE (LINE_WIDTH)
C
      Y = PHIMIN - DPHI
      J = 0
      DO IPHI =  -JPHI,JPHI
        X = ETAMIN
        Y = Y + DPHI
C
        IF ( MOD(IPHI+IPHI_OFFSET,IPHI_STEP) .EQ. 0 ) THEN
          DX = PHI_TICK
          J = J + 1
          IF ( J .NE. IPHI_SKIP ) THEN
            N = SCALE*Y + 0.05
            CALL VNUMI (1,N,'(',' ',')',STRING,L)
            CALL PLOT_WRITE_STRING(MAX_INTENSITY,MIN_INTENSITY,
     &                            GREEN,GRID_COLOR,XT,Y,STRING(2:L-1))
          ENDIF
        ELSE
          DX = 0.0
        ENDIF
C
        CALL JMOVE (X-DX,Y)
        X = ETAMAX
        CALL JDRAW (X+DX,Y)
      ENDDO
C
C ****  Close retained segment
C
      CALL JRCLOS
      RETURN
C
C ****  Determine position of title
C
      ENTRY PLOT_TOWERS_VALUES
     &  (DEV,PAUS,SCAL,XTT,YTT,DETA1,DPHI1,SWITCH1,ETMIN1,RADIUS1)
      DEV = DEVICE
      SCAL= SCALE
      XTT = 0.5*(ETAMAX-ETAMIN) + ETAMIN
      YTT = 0.5*(VMAX-PHIMAX) + PHIMAX
      DETA1 = DETA
      DPHI1 = DPHI
      PAUS  = PAUSE
      ETMIN1= ETMIN
      SWITCH1 = SWITCH
      RADIUS1 = RADIUS
  999 RETURN
      END
