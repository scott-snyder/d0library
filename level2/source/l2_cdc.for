      SUBROUTINE L2_CDC(ROAD, TRTIME, L2CD, RESULT_FLAG)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fast tacking for electron id in CDC
C-
C-   Inputs  : PHIMIN,PHIMAX,ETAMIN,ETAMAX
C-   Outputs :
C-   Controls:
C-
C-   Created  19-AUG-1991   Daniel R. Claes
C-            23-SEP-1991   Accept TRTIME (trigger timing) as input
C-            21-OCT-1991   Per discussions with Chang Kee Jung:
C-                          New version requires consective outer layers
C-                          hit with track-matching and delay lines used
C-            10-NOV-1992   Applying an additional match within a second
C-                          tighter PHI road for wires with single hits
C----------------------------------------------------------------------
      IMPLICIT NONE
C--------------------------------------------------------------------
      INCLUDE 'D0$INC:L2TRAK.INC/LIST'
      INCLUDE 'D0$INC:SECTLIST.INC/LIST'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC/LIST'
      INCLUDE 'D0$INC:PI.DEF/LIST'
      INCLUDE 'D0$PARAMS:L2CD.PARAMS'
C
      REAL    HIT, ROAD(1:6,0:6,0:3), TRTIME
      REAL    PHI_AVG, PHI_POSIT(0:6), PHItol
      INTEGER HITADR(0:1,0:1,5)! LAYER, WIRE, pulse#
      INTEGER HITLEN(0:1,0:1,5)! LAYER, WIRE, pulse#
      INTEGER POINTR(0:1,0:1,5)! LAYER, WIRE, pulse#
      INTEGER NUM_DEL(0:1,0:1)   ! LAYER(0:inner,1:outer)
C                                ! WIRE(0:inside,1:outside)
      INTEGER HITLST(5,3)     ! (Pulse#,HITADR:HITLEN,POINT)
      INTEGER GZSL2H, GZL2DRFT, GZL2DTMH, GZL2DPDH, I, INNER
      INTEGER IOPT, IWIRE, J, K, LSL2H, L2CD, NDIFF, NUM_SINGLES
      INTEGER N_INNER, N_OUTER, NPULSE, NWIR, OUTER, OUTER_WIRE
      INTEGER IN_PHI_ROAD,SECND_COUNT
      LOGICAL STATUS, RESULT_FLAG
      LOGICAL SINGLES(0:6)
C
      COMMON /NARROW_PHI/ PHItol
C
      RESULT_FLAG = .FALSE.
C
      L2DRFT = GZL2DRFT()
      L2DTMH = GZL2DTMH()
      L2DPDH = GZL2DPDH()
C
      CALL L2_FNDSEC(ROAD)            ! Returns, via common block,
C                                     ! Layer, Sector of ALL  hit CDC cells
C                                     ! Identifies crates serving hit cells
      CALL L2_CDCUNP(STATUS)          ! Maps CDC addresses for chnl numbers
      IF (.NOT.STATUS) GO TO 990      ! No CDC hits - no crates to unpack
C                                     ! Since this will happen when there is
C                                     ! no RAW data banks, pass event.
C
C Need to book a new L2CD bank for each candidate (call to CD_MATCH). Since number of
C hit wires seen are stored for each layer,sector within the road, must book it here.
C
      CALL BKL2CD(NROAD,L2CD)
C
      IF (L2CD.GT.0) THEN
        DO I = 1, NROAD
          IQ( L2CD + NTOP_L2CD + (I-1)*NREP_L2CD + 1) = LAYLST(I)  ! Store in Filt RESults (FRES) bank
          IQ( L2CD + NTOP_L2CD + (I-1)*NREP_L2CD + 2) = SECLST(I)
        ENDDO
      ENDIF
C
      N_OUTER = 0                     ! Start with 1st entry in SECLST
      OUTER = 3                       ! and the OUTER most (3rd) layer
C
  100 CONTINUE
      DO I = N_OUTER+1, NROAD         ! Loop through hit sectors ONLY
C
        IF (LAYLST(I).EQ.OUTER) THEN
C         Perform hit finding through outer most layer
          NWIR = 0
          NUM_SINGLES = 0
          PHI_AVG = 0                 ! A check that ALL single HITS line up
          DO IWIRE = 0,6
            SINGLES(IWIRE) = .FALSE.
            CALL L2_CDHITS(ROAD,LAYLST(I),SECLST(I),IWIRE,NPULSE,
     &                     HIT,HITLST,TRTIME) ! Returns hitlist of
C                               ! HITADR,HITLEN of all hits in the road and
C                               ! HIT is del_PHI from wire of most likely hit
            IF (NPULSE.GT.0) THEN
              NWIR = NWIR + 1           ! Count wires hit in this sector
              IF (NPULSE.EQ.1) THEN
                SINGLES(IWIRE) = .TRUE.
                PHI_POSIT(IWIRE)=HIT
                PHI_AVG = PHI_AVG + HIT
                NUM_SINGLES = NUM_SINGLES + 1
              ENDIF
            ENDIF
            IF (IWIRE.EQ.0 .OR. IWIRE.EQ.6) THEN  ! Save hits for delay lines
              OUTER_WIRE = INT(IWIRE/6)           ! 0:inner wire,1:outer wire
C
              IF (L2CD.GT.0) THEN
                IQ(L2CD+NTOP_L2CD+(I-1)*NREP_L2CD+4+OUTER_WIRE)=NPULSE
              ENDIF
C
              NUM_DEL(1,OUTER_WIRE)=NPULSE        ! (OUTER layer, delay line)
              DO J = 1, NPULSE
                HITADR(1,OUTER_WIRE,J) = HITLST(J,1)
                HITLEN(1,OUTER_WIRE,J) = HITLST(J,2)
                POINTR(1,OUTER_WIRE,J) = HITLST(J,3)
              ENDDO
            ENDIF
  110       CONTINUE
          ENDDO
C
          IF (NUM_SINGLES.GT.0) PHI_AVG = PHI_AVG/NUM_SINGLES
          IN_PHI_ROAD = 0
C
          IF (NUM_SINGLES.GT.1) THEN
            SECND_COUNT = 0
            DO J = 0, 6
              IF (SINGLES(J)) THEN
                    IF (ABS(PHI_AVG - PHI_POSIT(J)).LT.PHItol)
     &                IN_PHI_ROAD = IN_PHI_ROAD + 1
  120             CONTINUE
              ENDIF
            ENDDO
            SECND_COUNT = IN_PHI_ROAD
            NWIR = NWIR - NUM_SINGLES + SECND_COUNT
          ENDIF
C
          IF (L2CD.GT.0) THEN
            IQ( L2CD + NTOP_L2CD + (I-1)*NREP_L2CD + 3) = NWIR   ! Store in Filt RESults bank
          ENDIF
C
          IF (NWIR .GE. WIRE_MIN) THEN
            N_OUTER = I
              GOTO 150                  ! Found hit OUTER cell
          ENDIF
        ENDIF
      ENDDO
      IF (OUTER.GT.1) THEN
        OUTER = OUTER - 1                 ! Try another OUTER layer
        N_OUTER = N_OUTER + 1
        GOTO 100                          !
      ENDIF
C
C If this point is reached, no hits were found in layers 2 or 3
C      Conclude that there is NO track
C
      GOTO 999                           ! Return w/ no track
C
C Check for hits in an inner layer (2,1,0)
C
  150 CONTINUE
      N_INNER = N_OUTER                  ! Start from this point in SECLST
      INNER = OUTER - 1                  ! Move to next layer in
  200 CONTINUE
      DO I = N_INNER+1, NROAD
        IF (LAYLST(I).EQ.INNER) THEN
C          NDIFF =  MOD(SECLST(I),32)          ! For large ROADS need to
C     &             - MOD(SECLST(N_OUTER),32)  ! check  SECTORS  overlap
          NDIFF =  MOD( ( SECLST(I)-SECLST(N_OUTER) ) ,31 )
          IF (ABS(NDIFF).GT.1) GOTO 250
          IF ( MOD(OUTER,2).EQ.1 .AND. NDIFF.LT.0) GOTO 250
          IF ( OUTER.EQ.2 .AND. NDIFF.GT.0) GOTO 250
C         Perform hit finding through next layer in
          NWIR = 0
          NUM_SINGLES = 0
          PHI_AVG = 0                   !C% Check ALL single HITS line up
          DO IWIRE = 0,6
            SINGLES(IWIRE) = .FALSE.
            CALL L2_CDHITS(ROAD,LAYLST(I),SECLST(I),IWIRE,NPULSE,
     &                     HIT,HITLST,TRTIME)      ! Returns hit
            IF (NPULSE.GT.0) THEN
              NWIR = NWIR + 1           ! Count wires hit in this sector
              IF (NPULSE.EQ.1) THEN
                SINGLES(IWIRE) = .TRUE.
                PHI_POSIT(IWIRE)=HIT
                PHI_AVG = PHI_AVG + HIT
                NUM_SINGLES = NUM_SINGLES + 1
              ENDIF
            ENDIF
            IF (IWIRE.EQ.0 .OR. IWIRE.EQ.6) THEN  ! Save hits for delay lines
              OUTER_WIRE = INT(IWIRE/6) ! 0:inner wire,1:outer wire
C
              IF (L2CD.GT.0) THEN
                IQ(L2CD+NTOP_L2CD+(I-1)*NREP_L2CD+4+OUTER_WIRE)=NPULSE
              ENDIF
C
              NUM_DEL(0,OUTER_WIRE)=NPULSE        ! (INNER layer, delay line)
              DO J = 1, NPULSE
                HITADR(0,OUTER_WIRE,J) = HITLST(J,1)
                HITLEN(0,OUTER_WIRE,J) = HITLST(J,2)
                POINTR(0,OUTER_WIRE,J) = HITLST(J,3)
              ENDDO
            ENDIF
          ENDDO
C
          IF (NUM_SINGLES.GT.0) PHI_AVG = PHI_AVG/NUM_SINGLES
          IN_PHI_ROAD = 0
C
          IF (NUM_SINGLES.GT.1) THEN
            SECND_COUNT = 0
            DO J = 0, 6
              IF (SINGLES(J)) THEN
                    IF (ABS(PHI_AVG - PHI_POSIT(J)).LT.PHItol)
     &                IN_PHI_ROAD = IN_PHI_ROAD + 1
  260             CONTINUE
              ENDIF
            ENDDO
            SECND_COUNT = IN_PHI_ROAD
            NWIR = NWIR - NUM_SINGLES + SECND_COUNT
          ENDIF
C
          IF (L2CD.GT.0) THEN
            IQ( L2CD + NTOP_L2CD + (I-1)*NREP_L2CD + 3) = NWIR   ! Store in Filt RESults bank
          ENDIF
C
          IF (NWIR .GE. WIRE_MIN) THEN
              N_INNER = I
C
C May skip delay line requirement for testing purposes
C
              IF (.NOT.DEL_FLAG) GOTO 990
C
C  Need to check DELAY lines and see if z position is in ROAD
              CALL L2_CDEL(ROAD,N_INNER,N_OUTER,NUM_DEL,HITADR,HITLEN,
     &          POINTR,L2CD,TRTIME,RESULT_FLAG)
              IF (RESULT_FLAG) GOTO 990
C            ENDIF
          ENDIF
  250     CONTINUE
        ENDIF
      ENDDO
C
      INNER = INNER - 2            ! Try  pairing  OUTER cell
      IF (INNER.GE.0) GOTO 200     ! with another INNER layer
C                                               ! Try  remaining
      IF (LAYLST(N_OUTER+1).EQ.OUTER) GOTO 100  ! sectors in the
C                                               ! OUTER  layer
      IF (OUTER.GT.1) THEN         !
        OUTER = OUTER - 1          ! Try another OUTER layer
        GOTO 100                   !
      ELSE
        GOTO 999
      ENDIF
C
  990 RESULT_FLAG = .TRUE.
C
  999 CONTINUE
      RETURN
      END
