      SUBROUTINE USE_SHLB(PS,ITRA)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :GIVEN SHLB ARRAY, Takes the GCAH info
C-   from there and dumps it into Geant.
C-
C-   Inputs  :PS(4) = 4 VECTOR OF ISAJET TRACK
C-            ITRA = TRACK NUMBER
C-   Outputs :
C-   Controls:
C-
C-   Created  15-JUN-1990   Rajendran Raja
C-   Updated  29-JUL-1992   W.G.D.Dharmaratna, updated for SHLB version 2. 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$INC:SHLCON.INC'
      INCLUDE 'D0$INC:SHLDAT.INC'
C
      INTEGER PTCUR,IPART
      INTEGER PT_GCAH
C
      LOGICAL EM,HAD,LNEW
C
      INTEGER ITRA
      INTEGER I
      INTEGER IETRK,IPTRK,IESTO,IPSTO,INDEX
      INTEGER IP,IE,IL,IDTRK,NUM_HITS
      REAL PS(4),PP(9),MOM,P_MASS,P_KE
C
      REAL SMRFAC,SMF
      REAL ECELL,ESCALE,ENER_MISS,ENER_TOT,ESCIN
      REAL  TOT_MG1_ENERGY,TOT_ICD_ENERGY,TOT_MG2_ENERGY,
     &  TOT_CAL_ENERGY
      REAL    MISS_MG1_ENERGY,MISS_ICD_ENERGY,MISS_MG2_ENERGY,
     &  MISS_CAL_ENERGY
      REAL    MG1_SCALE,ICD_SCALE,MG2_SCALE,CAL_SCALE
C
C----------------------------------------------------------------------
      DO I =  1, 4
        PP(I) = PS(I)                   ! Track 4 momentum
        PP(4+I) = 0                     ! Unused
      ENDDO
      IESTO = SHLB(10)                   ! ETA OF STORED TRACK
      IPSTO = SHLB(11)                  ! PHI OF STORED TRACK
C
      IETRK = IETAC_PRIMARY
      IPTRK = IPHIC_PRIMARY             ! ETA AND PHI OF CURRENT TRACK
C
      PT_GCAH = SHLB(2)                 ! POINTER TO 1ST GCAH AREA IN SHLB
C
C *** LW is now the address of the desired track bank
C
      LNEW=.TRUE.
C
C
C ****  Calculate energy scaling from library track to event track
C
      MOM = SQRT(PS(1)*PS(1)+PS(2)*PS(2)+PS(3)*PS(3))
      P_MASS  = SQRT((PS(4)+MOM)*ABS(PS(4)-MOM))   
      P_KE = PS(4) - P_MASS

C      ESCALE=P_KE/(SHLB(9)-SHLB(12)-SHLB(19)) !Isajet track K. energy/(stored
C      ! track K energy - KE of dropped gcah banks)
      ESCALE = PS(4)/SHLB(9)
C
      IPART = SHLB(5)
      IF(IABS(IPART).EQ.14)THEN       ! MUON
        ESCALE = 1.0                      ! DO not scale
      ENDIF
C
C
 1000 NUM_HITS=SHLB(PT_GCAH+12)
C
      IPART = SHLB(PT_GCAH + 5)         ! GEANT ID OF GCAH SEGMENT
C
C ****  DO HADRONS AND EM . MUONS SHOULD BE HANDLED SEPARATELY
C
      EM = .FALSE.
      HAD = .FALSE.
      IF(IPART.LE.3)THEN
        EM= .TRUE.          ! E+E- GAMMA GEANT CONVENTION
      ELSEIF(IPART.GT.6)THEN            ! MUONS AND NEUTRINOS
        HAD = .TRUE.
      ENDIF
C
      CALL GEAISA(IPART,IDTRK)
C
C ****  Loop over hits and store them
C
      IF (NUM_HITS.GE.1)THEN
        TOT_MG1_ENERGY = 0.
        TOT_ICD_ENERGY = 0.
        TOT_MG2_ENERGY = 0.
        TOT_CAL_ENERGY = 0.
C
        MISS_MG1_ENERGY = SHLB(PT_GCAH+7)
        MISS_ICD_ENERGY = SHLB(PT_GCAH+8)
        MISS_MG2_ENERGY = SHLB(PT_GCAH+9)
        MISS_CAL_ENERGY = SHLB(PT_GCAH+6)
C
        DO I = 1,NUM_HITS
          INDEX=SHLB(PT_GCAH+12+2*I)
          ECELL=SHLB(PT_GCAH+11+2*I)
C
C ****  Unpack the index to IETAC, IPHI, ILYR coordinates
C
          IE=ISIGN(IABS(INDEX)/10000,INDEX)
          IP=MOD(IABS(INDEX)/100,100)
          IL=MOD(IABS(INDEX),100)
          IF ( IL.EQ.MNLYMG ) THEN
            TOT_MG1_ENERGY = TOT_MG1_ENERGY + ECELL
          ELSEIF ( IL.EQ.LYICD) THEN
            TOT_ICD_ENERGY = TOT_ICD_ENERGY + ECELL
          ELSEIF ( IL.EQ.MXLYMG ) THEN
            TOT_MG2_ENERGY = TOT_MG2_ENERGY + ECELL
          ELSE
            TOT_CAL_ENERGY = TOT_CAL_ENERGY + ECELL
          ENDIF
        ENDDO
C
        MG1_SCALE = 1.
        ICD_SCALE = 1.
        MG2_SCALE = 1.
        CAL_SCALE = 1.
        IF ( TOT_MG1_ENERGY.GT.0. ) THEN
          MG1_SCALE = (MISS_MG1_ENERGY+TOT_MG1_ENERGY)/TOT_MG1_ENERGY
        ENDIF
        IF ( TOT_ICD_ENERGY.GT.0. ) THEN
          ICD_SCALE = (MISS_ICD_ENERGY+TOT_ICD_ENERGY)/TOT_ICD_ENERGY
        ENDIF
        IF ( TOT_MG2_ENERGY.GT.0. ) THEN
          MG2_SCALE = (MISS_MG2_ENERGY+TOT_MG2_ENERGY)/TOT_MG2_ENERGY
        ENDIF
        IF ( TOT_CAL_ENERGY.GT.0. ) THEN
          CAL_SCALE = (MISS_CAL_ENERGY+TOT_CAL_ENERGY)/TOT_CAL_ENERGY
        ENDIF
C
        DO 550 I=1,NUM_HITS                ! LOOP OVER THE NON ZERO HITS
C
C ****  Get packed index of hit
C
          INDEX=SHLB(PT_GCAH+12+2*I)
          ECELL=SHLB(PT_GCAH+11+2*I)
C
C ****  Unpack the index to IETAC, IPHI, ILYR coordinates
C
          IE=ISIGN(IABS(INDEX)/10000,INDEX)
          IP=MOD(IABS(INDEX)/100,100)
          IL=MOD(IABS(INDEX),100)
C
C ****  Rotate hit in phi to match event track
C
          IP=IP-IPSTO+IPTRK
          IF(IP.GT.64)IP=IP-64
          IF(IP.LT.1)IP=IP+64
          IF(ABS(IE).GE.33.AND.MOD(IP,2).EQ.0)IP=IP-1
C
C ****  Flip hit in +- z if necessary
C
          IF(IESTO*IETRK.LT.0)THEN
            IE=-1*IE
C
C ****  Rearrange EM3 layers where division is 0.05 x 0.05
C
            IF(ABS(IE).LE.26)THEN
              IF(IL.EQ.3)THEN
                IL=5
              ELSEIF(IL.EQ.5)THEN
                IL=3
              ELSEIF(IL.EQ.4)THEN
                IL=6
              ELSEIF(IL.EQ.6)THEN
                IL=4
              ENDIF
            ENDIF
          ENDIF
C
C ****  ADDING BACK THE MISSING STUFF PROPORTIONATELY
C
          IF ( IL.EQ.MNLYMG ) THEN
            ECELL = ECELL*MG1_SCALE
          ELSEIF ( IL.EQ.LYICD) THEN
            ECELL = ECELL*ICD_SCALE
          ELSEIF ( IL.EQ.MXLYMG ) THEN
            ECELL = ECELL*MG2_SCALE
          ELSE
            ECELL = ECELL*CAL_SCALE
          ENDIF
C
          PP(9) = ECELL
          ECELL=ECELL*ESCALE*SMRFAC(LNEW,PP,IDTRK,IE,IP,IL)
C
          LNEW=.FALSE.
C
C ****  Store the energies
C
          CALL DHSTOR(ITRA,IE,IP,IL,ECELL)  ! STORE HITS
C
  550   CONTINUE
      ENDIF
C
C **** STORE DEAD ENERGY
C
      IL=MXLYCH + 1
      IF(ABS(IETRK).GE.14)IL=MXLYCH + 2
      CALL DHSTOR(ITRA,IETRK,IPTRK,IL,ESCALE*SHLB(PT_GCAH+10))
C
      PT_GCAH = SHLB(PT_GCAH)
      IF(PT_GCAH.NE.0)GO TO 1000        ! MORE GCAH STUFF
C
 9999 CONTINUE
  999 RETURN
      END
