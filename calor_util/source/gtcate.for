      SUBROUTINE GTCATE(TOWER,IETA,IPHI,LAYER,NLAYER,E,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : For given tower number TOWER return data
C-   from CATE bank. You MUST call GTCATE_TOTAL FIRST to initialize 
C-   pointer offsets into the CATE bank. (See below.)
C-
C-   Inputs  : TOWER    [I]     Tower number
C-             
C-   Outputs : IETA     [I]     Eta index
C-             IPHI     [I]     Phi index
C-             LAYER(*) [I]     Layer numbers
C-             NLAYER   [I]     Number of layers contributing to tower
C-             E(7)     [R]     Ex,Ey,Ez,E,Et,sig**2(Ex),sig**2(Ey)
C-             IER      [I]     Error code; 0 --- OK
C-                              -4 --- No CATE bank.
C-   Controls: 
C-   
C-   Notes:
C-   
C-   Call
C-   
C-      GTCATE_TOTAL (NUM_TOWERS, NUM_EM_TOWERS, IER)
C-      
C-   to initialize pointers into the CATE bank.
C-   
C-   Call
C-   
C-      GTCATE_MAX_ENERGY 
C-      (TOWER_TYPE, ENERGY_TYPE, ENERGY, TOWER, IETA, IPHI)
C-   
C-   to return the tower with the maximum energy or Et.
C-   
C-   TOWER_TYPE         'EM' for Em towers, 'HAD' for hadronic towers
C-   ENERGY_TYPE        'ET' For Et, 'EN' for ENergy
C-
C-  Updated 17-SEP-1991  Allen I. Mincer  make consistent for E<0
C-
C----------------------------------------------------------------------
C
C  Bank name: CATE
C  Author   : Serban D. Protopopescu and A.Zieminski,R.Raja
C  Date     : May 7,1989
C  Tree description: HITS_ZEBANKS,CALDAT_ZEBANKS
C
C             Hardware towers energy bank  
C             (summation over electromagnetic or hadronic layers, separately)
C
C    LQ     Q/IQ
C----------------------------------------------------------------------
C     -1            struct.  (free)
C      0            next     to next CATE
C     +1            up       to CAHT
C     +2            origin   to CAHT for first and previous CATE for others
C......................................................................
C            I-5             bank number
C             -4             bank name,  'CATE'
C             -3             NL=1
C             -2             NS=1
C             -1             ND=NR*ntowers+3
C              0             STATUS
C             +1     I       bank version (=2)
C             +2     I       NR=14 (repetition number)
C             +3     I       ntowers + (number of EM towers)*CATENM
C             +4     F       Ex
C             +5     F       Ey
C             +6     F       Ez
C             +7     F       E
C             +8     F       Et
C             +9     F       sig**2(Ex) - sum of estimated resolution for
C             +10    F       sig**2(Ey)   individual cells (from CAEH)
C             +11    I       number of layers contributing
C             +12    I       eta index
C             +13    I       phi index
C             +14    I       electromagnetic(1), or total(2) index
C             +15    I       Cluster number for EM 
C             +16    I       Cluster number for HAD
C             +17    I       Bit pattern corresponding to layers contributing.
C                            If bit 1 is on Layer 1 contributed to this tower 
C
C                  4-17 repeated ntowers
C-
C-   Created   3-OCT-1989   Harrison B. Prosper
C-   Updated  26-JAN-1996   Bob Hirosky  reset IER flag in all entry points 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INTEGER TOWER
      INTEGER IETA,IPHI
      INTEGER LAYER(*)
      INTEGER NLAYER
      REAL    E(7), ETMIN
      INTEGER CLASS,NEXT,IER
C
      INTEGER NTOWERS,NEM_TOWERS,POINTER
      INTEGER NUM_TOWERS,NUM_EM_TOWERS,NREP
      CHARACTER*4 TOWER_TYPE,ENERGY_TYPE
      REAL    ENERGY_MAX
      INTEGER TOWER_MAX,IETA_MAX,IPHI_MAX,FIRST_TOWER,LAST_TOWER
      INTEGER MIN_TOWER, HAD_SWITCH
C
      INTEGER GZCATE
      INTEGER I,J,K,BASE,JBIT,OFFSET
C
      INCLUDE 'D0$INC:ZLINKC.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:CATENM.PARAMS'
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      SAVE NREP, NUM_TOWERS,NUM_EM_TOWERS
C----------------------------------------------------------------------
C
      IER = 0
C
      IF ( TOWER .LT. 1 .OR. TOWER .GT. NUM_TOWERS ) THEN
        IER = -3
        GOTO 999
      ENDIF
C
      LCATE = GZCATE()
      IF ( LCATE .LE. 0 ) THEN
        IER = - 4
        GOTO 999
      ENDIF
C
      BASE = (TOWER-1)*NREP + LCATE     ! Base address of TOWER data
      IETA = IQ(BASE+12)                ! Get physics indices
      IPHI = IQ(BASE+13)
      DO I =  1,7  
        E(I) = Q(BASE+I+3)              ! Get energies
      ENDDO
C
C ****  Get the layers contributing to this tower
C
      NLAYER = IQ(BASE+11)              ! Number of layers contributing
      J = 0
      DO I =  1,NLYRL
        IF ( JBIT(IQ(BASE+17),I) .NE. 0 ) THEN
          J = J + 1
          LAYER(J) = I
        ENDIF
      ENDDO
      IF ( J .NE. NLAYER ) THEN
        IER = -5
        GOTO 999
      ENDIF
C
      RETURN
C
C ****************************************************
C ****  ENTRY point to get cluster number of TOWER
C ****************************************************
      ENTRY GTCATE_CLUSTER (TOWER,CLASS,NEXT,IER)
      IER = 0
C
      IF ( TOWER .LT. 1 .OR. TOWER .GT. NUM_TOWERS ) THEN
        IER = -3
        GOTO 999
      ENDIF
C
      LCATE = GZCATE()
      IF ( LCATE .LE. 0 ) THEN
        IER = - 4
        GOTO 999
      ENDIF
C
      BASE = (TOWER-1)*NREP + LCATE     ! Base address of TOWER data
      CLASS= IQ(BASE+15)                ! Get Class number
      NEXT = IQ(BASE+16)                ! Get index of next tower
      RETURN
C
C ****************************************************
C ****  ENTRY point to initialize CATE pointers
C ****************************************************
      ENTRY GTCATE_TOTAL (NTOWERS,NEM_TOWERS,IER)
C
      IER = 0
      LCATE = GZCATE()
      IF ( LCATE.EQ.0 ) THEN
        IER = -4
        GOTO 999
      ENDIF
C
      NREP          = IQ(LCATE+2)          ! CATE repetition number
      NUM_EM_TOWERS = IQ(LCATE+3)/CATENM
      NUM_TOWERS    = IQ(LCATE+3)-CATENM*NUM_EM_TOWERS
      NTOWERS       = NUM_TOWERS
      NEM_TOWERS    = NUM_EM_TOWERS
      RETURN
C
C ****************************************************
C ****  ENTRY point to get tower with maximum energy
C ****************************************************
      ENTRY GTCATE_MAX_ENERGY 
     &  (TOWER_TYPE,ENERGY_TYPE,ENERGY_MAX,TOWER_MAX,IETA_MAX,IPHI_MAX)
C
      IER = 0
      LCATE = GZCATE()
      IF ( LCATE.EQ.0 ) THEN
        IER = -4
        GOTO 999
      ENDIF
C
      IF ( TOWER_TYPE(1:2) .EQ. 'EM' ) THEN
        FIRST_TOWER = 1
        LAST_TOWER  = NUM_EM_TOWERS
      ELSE
        FIRST_TOWER = NUM_EM_TOWERS + 1
        LAST_TOWER  = NUM_TOWERS
      ENDIF
C
      IF ( ENERGY_TYPE(1:2) .EQ. 'ET' ) THEN
        OFFSET = 8                      ! Get Et
      ELSE
        OFFSET = 7                      ! Get E
      ENDIF
C
      ENERGY_MAX = -1.E8
      TOWER_MAX  = 0
      POINTER    = LCATE + OFFSET + (FIRST_TOWER-2)*NREP
      DO I =  FIRST_TOWER, LAST_TOWER  
        POINTER = POINTER + NREP
        IF ( Q(POINTER) .GT. ENERGY_MAX ) THEN
          ENERGY_MAX = Q(POINTER)
          TOWER_MAX  = I
        ENDIF
      ENDDO
C
      BASE = (TOWER_MAX-1)*NREP + LCATE ! Base address of TOWER data
      IETA_MAX = IQ(BASE+12)            ! Get physics indices
      IPHI_MAX = IQ(BASE+13)
C
      RETURN
C
      ENTRY GTCATE_MIN_TOWER(HAD_SWITCH,ETMIN,MIN_TOWER)
C---------------------------------------------------------------------
C
C ****  This subroutine returns the tower number which is just
C ****  above the cutoff ETmin (the minimum ET cutoff)
C
C     Inputs:    HAD_SWITCH   1: EM towers; 2: EM+HAD towers
C                ETMIN        min ET for cut
C     Outputs:   MIN_TOWER    Tower number just above ETMIN cut
C
C     Created:   12-Oct-1989      Stephen Kahn
C
C--------------------------------------------------------------------
      IER = 0
      LCATE = GZCATE()
      IF ( LCATE.EQ.0 ) THEN
        IER = -4
        GOTO 999
      ENDIF
C
      IF(HAD_SWITCH.EQ.2) THEN      ! Hadron+EM towers
        FIRST_TOWER= NUM_EM_TOWERS+1
        LAST_TOWER = NUM_TOWERS
      ELSE                          ! EM towers
        FIRST_TOWER=1
        LAST_TOWER=NUM_EM_TOWERS
      END IF
C
      DO I = FIRST_TOWER, LAST_TOWER
      J = (I-1)*NREP + LCATE
      IF(Q(J+8).LT.ETMIN)GO TO 100
      END DO
      MIN_TOWER = LAST_TOWER
      RETURN
  100 CONTINUE
      MIN_TOWER = I-1
C
  999 RETURN
      END
