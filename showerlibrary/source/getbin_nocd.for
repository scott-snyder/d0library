      SUBROUTINE GETBIN_NOCD(ISAPART,RVERTEX,P4)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Given Vertex and 4 vector of track,
C-                         uses CLINPH_FAST to work out Calorimeter
C-                         Cell of impact and the KEY for showerlibrary.
C-                         To be used for showerlibrary with No Central
C-                         Detector in.
C-
C-   Inputs  : ISAPART = PARTICLE ID (ISAJET)
C-             RVERTEX(3) = Vertex of track
C-             P4(4)     = 4 vector of track
C-   Outputs : KEY(*)    = Key for showerlibrary, IN SHLCON
C-   Controls:
C-
C-   Created  14-MAY-1990   Rajendran Raja
C-   Updated  14-APR-1992   W.G.D.Dharmaratna, FEW CHANGES FOR VERSION 2.0 
C-   Updated   2-DEC-1992   W. Dharmaratna  going back to ENERGY 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:SHLCON.INC'
      INCLUDE 'D0$INC:SHLDAT.INC'
      INCLUDE 'D0$INC:PI.DEF'
C
      INTEGER IER,ISAPART,GEAPART
      LOGICAL EM,HAD
      REAL    RVERTEX(*),P4(*)
      REAL    DIR_COSINE(3),PP,ZV,P_KE
      INTEGER ARGSOK,I
      INTEGER IBIN
      INTEGER KEY_ID
      LOGICAL FIRST
      DATA FIRST/.TRUE./
      REAL    CC_EM1_RADIUS,CC_EM1_CRACK_WIDTH,CRACK_ARC,EM1_ARC
      REAL    PHIC_RED,PHI_LIM_EM(2),PHI_LIM_HAD(2),P_MASS
      INTEGER IND_PHI,ICC
C----------------------------------------------------------------------
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        CALL EZPICK('SHOWERLIBRARY_RCP')
        CALL EZGET('CC_EM1_RADIUS',CC_EM1_RADIUS,IER)
        CALL EZGET('CC_EM1_CRACK_WIDTH',CC_EM1_CRACK_WIDTH,IER)
C
C ****  THESE REDUCED QUANTITIES ARE BEING READ IN FROM SHOWERLIBRARY.RCP
C ****  THEY SHOULD EVENTUALLY BE READ IN FROM CAL_STPFILE!
C
        CRACK_ARC = CC_EM1_CRACK_WIDTH/CC_EM1_RADIUS        ! HALF-ARC
        EM1_ARC = TWOPI/32.
        PHI_LIM_EM(1) = 0.5*CRACK_ARC
        PHI_LIM_EM(2) = EM1_ARC - PHI_LIM_EM(1)       ! GOOD PHI REGIONS
C
        CALL EZGET('PHI_LIM_HAD',PHI_LIM_HAD,IER)       ! HADRONIC PHI LIMITS
                                        ! IN RADIANS
C
        CALL EZRSET
      ENDIF
C
      CALL UCOPY(P4,P4_PRIMARY,4)       ! STORE
C
      PP = SQRT(P4(1)*P4(1)+P4(2)*P4(2)+P4(3)*P4(3))
      DO I = 1 , 3
        DIR_COSINE(I) = P4(I)/PP
      ENDDO
      CALL CLINPH_FAST(RVERTEX,DIR_COSINE,ETAC_PRIMARY,PHIC_PRIMARY,
     &  IETAC_PRIMARY,IPHIC_PRIMARY,ICC,ARGSOK)
      IF(ARGSOK.NE.0)THEN
        GO TO 990
      ENDIF
C
C ****  determine keys
C
      CALL ISAGEA(ISAPART,GEAPART)      ! CONVERT TO GEANT.
      IPART_PRIMARY = GEAPART           ! STORE GEANT ID
      CALL GEAN_KEYID(GEAPART,KEY_ID)   ! KEY_ID GIVEN GEAN_PARTID
C
C      IF (GEAPART.LE.4 .OR. GEAPART.EQ.48) THEN
C        P_MASS = 0.0
C      ELSE
C        P_MASS  = SQRT((P4(4)+PP)*(P4(4)-PP))   
C      ENDIF
C      P_KE = P4(4) - P_MASS

      IF(P4(4).LT.MOMBIN(1).OR.KEY_ID.EQ.0)GOTO 990  
C
      DO 100 IBIN=1,NMBIN
        IF (MOMBIN(IBIN).LE.P4(4))THEN
          IF(MOMBIN(IBIN+1).GT.P4(4))THEN
            KEY(3)=IBIN
            GO TO 101
          ENDIF
        ENDIF
  100 CONTINUE
  101 CONTINUE
C
      KEY(2)=ABS(IETAC_PRIMARY)
C
C ****  determine vertex position bin KEY(3)
C
      ZV = SIGN(1,IETAC_PRIMARY)*SIGN(1.0,RVERTEX(3))*ABS(RVERTEX(3))
      DO 300 IBIN=1,NVBIN
        IF (VTXBIN(IBIN).LE.ZV)THEN
          IF(VTXBIN(IBIN+1).GT.ZV)THEN
            KEY(1)=IBIN
            GO TO 301
          ENDIF
        ENDIF
  300 CONTINUE
  301 CONTINUE
C
      KEY(4) = KEY_ID                   ! PARTICLE ID
      KEY(5) = 1                        ! PHI_ID FOR NOW
C
      IF(KEY(4).EQ.1.AND.ICC.EQ.1)THEN
C IN CC EM IF HERE
        IND_PHI = PHIC_PRIMARY/EM1_ARC
        PHIC_RED = PHIC_PRIMARY - EM1_ARC*IND_PHI
        IF(PHIC_RED.LT.PHI_LIM_EM(1).OR.
     &    PHIC_RED.GT.PHI_LIM_EM(2))KEY(5)=2
C
C ****  IF(KEY(5).EQ.2)FOR EM, THEN IT IS IN CC EM CRACK.
C
      ENDIF
      IF(KEY(4).EQ.3)THEN
C HADRONIC PHI LIMITS FOR MAIN RING BEAM PIPE MODULE
        IF(PHIC_PRIMARY.GT.PHI_LIM_HAD(1).AND.
     &    PHIC_PRIMARY.LT.PHI_LIM_HAD(2))KEY(5)=2
      ENDIF
  999 RETURN
C
  990 CALL UZERO(KEY,1,NKEY)               ! ZERO THEM
      RETURN
      END
