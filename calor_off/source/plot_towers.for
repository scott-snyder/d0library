      LOGICAL FUNCTION PLOT_TOWERS
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-
C-      Simple routine to plot occupied towers in eta-phi space.
C-      Uses data in CATE bank. This routine should be called just
C-      after pre-clustering has been performed. It also plots the
C-      centres of CACL clusters and JETS.
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created   4-OCT-1989   Harrison B. Prosper
C-   Updated  12-DEC-1989   Harrison B. Prosper
C-      Show CACL centres
C-   Updated  11-JAN-1990   Harrison B. Prosper
C-      Show JETS centres
C-   Updated  17-JAN-1990   Boaz Klima
C-      Add more info( event #, run #,Et for JETS and for PJET etc.)
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INTEGER ERROR,I,J,N,DEVICE,II,JJ,KK,LSUP,LPJET,GZPJET,EVENT
      INTEGER NPJET,EVT,RUN,IER
      INTEGER IETA,IPHI,LAYER(32),NLAYER,NEXT_TOWER,CLASS,NJETS
      INTEGER TOWER,TOWER1,TOWER2,NTOWERS,NEM_TOWERS,NCLUSTERS
      INTEGER TOWER_MAX,IETA_MAX,IPHI_MAX,CLUSTER,SINGLE,SWITCH
C
      INTEGER CLASS_MAX
      PARAMETER( CLASS_MAX = 5000 )     ! Maximum number of classes
      INTEGER COLOR(CLASS_MAX)
      INTEGER HADRONIC
      PARAMETER( HADRONIC = 2 )
C
      REAL    E(7),EX,EY,EZ,ENERGY,ET,SIGEX,SIGEY,X,Y,Z,ETA,PHI,THETA
      REAL    P(4),MASS
      EQUIVALENCE (E(1),EX)
      EQUIVALENCE (E(2),EY)
      EQUIVALENCE (E(3),EZ)
      EQUIVALENCE (E(4),ENERGY)
      EQUIVALENCE (E(5),ET)
      EQUIVALENCE (E(6),SIGEX)
      EQUIVALENCE (E(7),SIGEY)
C
      INTEGER VERS(3),BANK_VERSION,CLUSTER_NUMBER,CLUSTER_TYPE
      EQUIVALENCE (VERS(1),BANK_VERSION)
      EQUIVALENCE (VERS(2),CLUSTER_NUMBER)
      EQUIVALENCE (VERS(3),CLUSTER_TYPE)
C
      REAL    DETA,DPHI,ETMIN,ENERGY_MAX,XT,YT,SCALE,RADIUS,DX
      CHARACTER*132 STRING
C
      INTEGER NORMAL,RED,GREEN,YELLOW,BLUE,MAGENTA,CYAN,WHITE,BLACK
      INTEGER COMPLEMENT,ICOLOR,MAX_INTENSITY,MIN_INTENSITY
      INTEGER POINT,PLUS,ASTERISK,BIGO,BIGX
      PARAMETER( POINT    = 1 )
      PARAMETER( PLUS     = 2 )
      PARAMETER( ASTERISK = 3 )
      PARAMETER( BIGO     = 4 )
      PARAMETER( BIGX     = 5 )
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
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:PI.DEF'
      LOGICAL PAUSE
      LOGICAL FIRST
      DATA FIRST/.TRUE./
      SAVE FIRST
C----------------------------------------------------------------------
C
      PLOT_TOWERS = .TRUE.
C
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        CALL PLOT_TOWERS_VALUES
     &    (DEVICE,PAUSE,SCALE,XT,YT,DETA,DPHI,SWITCH,ETMIN,RADIUS)
        EVENT = 0
        CALL EZPICK('PLOT_TOWERS_RCP')
        CALL EZGET('RADIUS_OFFSET',DX,IER)
        CALL EZRSET
      ENDIF
C
      CALL GTCATE_TOTAL (NTOWERS,NEM_TOWERS,ERROR)
      IF ( ERROR .NE. 0 ) THEN
        GOTO 999
      ENDIF
C
C ****  Open temporary segment
C
      CALL JFRAME (DEVICE)
      CALL JOPEN
      CALL JSIZE  (DETA,DPHI)           ! Set character size
      CALL JJUST  (2,2)                 ! Set character relative position
      CALL JCOLOR (GREEN)
      CALL JINTEN (MAX_INTENSITY)
C
C ****  Get range of towers
C
      IF ( SWITCH .EQ. HADRONIC ) THEN
        TOWER1 = NEM_TOWERS+1             ! First  tower
      ELSE
        TOWER1 = 1
      ENDIF
      CALL GTCATE_MIN_TOWER (SWITCH,ETMIN,TOWER2)       ! Last tower
C
      DO I =  1,CLASS_MAX
        COLOR(I) = 0
      ENDDO
C
C *************************************************
C ****  Loop over CATE towers
C *************************************************
C
      CLUSTER = 0                       ! Number of clusters
      SINGLE  = 0                       ! Number of clusters  with 1 element
C
      DO TOWER = TOWER1, TOWER2
C
        CALL GTCATE (TOWER,IETA,IPHI,LAYER,NLAYER,E,ERROR)
C
C ****  Convert (Ex,Ey,Ez) to real (eta,phi)
C
        IF ( ERROR .EQ. 0 ) THEN
          E(4) = SQRT(E(1)*E(1)+E(2)*E(2)+E(3)*E(3))
          CALL ETOETA (E,PHI,THETA,ETA)
          CALL JCOLOR (WHITE)
          CALL JCMARK (POINT)
          CALL JMARK (ETA,PHI)
C
C ****  Circle all towers but exclude single tower clusters.
C
          CALL GTCATE_CLUSTER (TOWER, CLASS, NEXT_TOWER, ERROR)
C
          IF ( NEXT_TOWER .NE. TOWER ) THEN
C
C ****  Choose color according to tower CLASS number
C
            IF ( COLOR(CLASS) .LE. 0 ) THEN
              CLUSTER = CLUSTER + 1     ! New cluster number
              COLOR(CLASS) = MOD(CLUSTER+6,7) + 1
            ENDIF
C
            CALL JCOLOR (COLOR(CLASS))
            CALL JMOVE  (ETA,PHI)
            CALL J1STRG ('O')
          ELSE
            SINGLE = SINGLE + 1
          ENDIF
C
        ENDIF
C
      ENDDO
C
C *************************************************
C ****  Loop over CACL clusters
C *************************************************
C
      CALL JCOLOR (GREEN)
      CALL JCMARK (BIGO)
      CALL GTCACL_TOTAL (NCLUSTERS,ERROR)
      IF ( ERROR .EQ. 0 ) THEN
        DO II = 1, NCLUSTERS
          KK = II
          CALL GTCACL (KK,VERS,E,THETA,PHI,ETA,X,Y,Z,ERROR)
          CALL JMARK (ETA,PHI)
        ENDDO
      ENDIF
C
C *************************************************
C ****  Loop over JETS
C *************************************************
C
      CALL GTJETS_TOTAL (NJETS,ERROR)
      IF ( ERROR .EQ. 0 ) THEN
        DO II = 1, NJETS
          KK = II
          CALL GTJETS (KK,VERS,E,THETA,PHI,ETA,ERROR)
          CALL JCOLOR (CYAN)
          CALL JCIRCL (ETA,PHI,0.,RADIUS,1)
          WRITE(UNIT=STRING,FMT=110) ET
  110     FORMAT(F5.1)
          CALL SWORDS (STRING,I,J,N)
          CALL PLOT_WRITE_STRING(MAX_INTENSITY,MIN_INTENSITY,
     &                          CYAN,RED,ETA+RADIUS+DX,PHI,STRING(I:J))
        ENDDO
      ENDIF
C
C *************************************************
C ****  Loop over PJET
C *************************************************
C
      NPJET = -1
      LPJET = GZPJET()
      IF ( LPJET .GT. 0 ) THEN
        LSUP = 0
        DO WHILE ( LPJET .GT. 0 )
          CALL JCOLOR (RED)
          NPJET = NPJET + 1
          CALL GTPJET(LSUP,LPJET,ET,P,MASS,PHI,THETA,ETA)
          CALL JCIRCL (ETA,PHI,0.,RADIUS,1)
          LSUP = LPJET
          WRITE(UNIT=STRING,FMT=110) ET
          CALL SWORDS (STRING,I,J,N)
          CALL PLOT_WRITE_STRING(MAX_INTENSITY,MIN_INTENSITY,
     &                          RED,RED,ETA-RADIUS-DX,PHI,STRING(I:J))
        ENDDO
      ENDIF
C
C ****  Write title
C
      EVENT = EVENT + 1
      EVT = IQ(LHEAD+9)
      RUN = IQ(LHEAD+12)
C      WRITE(UNIT=STRING,FMT=200) EVENT,NTOWERS-NEM_TOWERS,
C    &  NCLUSTERS,NJETS
C  200 FORMAT('EVENT ',I4,', TOWER count ',I4,', PRE-CLUSTER count ',I4,
C     &  ', JET count ',I4)
      WRITE(UNIT=STRING,FMT=200) EVT,RUN,NPJET,NCLUSTERS,NJETS
  200 FORMAT('EVENT NO.',I10,' , RUN NO. ',I6,'   -   NPJET = ',I2,
     &  ' , NCACL = ',I2,' , NJETS = ',I2)
      CALL SWORDS (STRING,I,J,N)
      CALL PLOT_WRITE_STRING(MAX_INTENSITY,MIN_INTENSITY,
     &                          GREEN,RED,XT,YT,STRING(I:J))
C
C ****  Close segment
C
      CALL JCLOSE
      IF ( PAUSE ) THEN
        WRITE(6,FMT='('' PLOT_TOWERS: Hit RETURN to continue'')')
        CALL JPAUSE (DEVICE)
      ENDIF
C
  999 RETURN
      END
