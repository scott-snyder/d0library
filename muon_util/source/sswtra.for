C+
      SUBROUTINE SSWTRA (DIR, NTRK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : subroutine for reconstruction of segments of
C-                         tracks that cross SB and WC (after magnet)
C-
C-   Inputs  : DIR - direction number.
C-   Outputs : NTRK - number of tracks.
C-   Controls: none.
C-
C-   Based on Efimov's SAMTRA
C-                   
C-   Created   29-MAY-1994   Joao R.T. de Mello Neto
C-   Updated   24-FEB-1995   Andre Sznajder ( bug fixing and clean up )
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INTEGER DIR, NTRK
C -   planes are in station B
      INTEGER NPL, N_PLANES
      PARAMETER (NPL=12, N_PLANES=3)
      INTEGER MTRK,HITS, LINK, NHIT, WORK
      INTEGER LMTRH, LSAMT, LSAHS
      INTEGER GZMTRH, GZSAMT, GZSAHS, GZSATW, GZSTNA, GZSTSA
      INTEGER LD, IND, NST, IPL
      INTEGER I, J, K, L, N
      INTEGER PL, LT, NT, JTR
      REAL    DRFMX, DIST
      INTEGER NSAMIN,NHTMX, NTRMX, NHTBA, NHTMA
      LOGICAL FIRST
C  
      INTEGER ICHMAX,MSH
      PARAMETER( ICHMAX = 24 ) ! max # roads
      PARAMETER( MSH = 40 )    ! max # Samus hits in a road
      INTEGER LTRG2, NCH, ICH(ICHMAX,4), NSOL
      INTEGER NSH(ICHMAX),TUBE_ADD(ICHMAX,MSH),GEOM_ADD(ICHMAX,MSH)
      REAL AT0(24), R0(24), PH0(24), PAV(3), CH(ICHMAX,3)  
      REAL BT0(24), SBH(24,3),SAH(24,3),MAGH(24,3),COSBM(24,3)
      LOGICAL LHITS_IN_ROAD
      INTEGER LP, WWAREA
      SAVE FIRST
      SAVE NHTMX, NTRMX, NHTBA, NHTMA, DRFMX
      DATA FIRST /.TRUE./
C
C *** Initialization
C
      IF (FIRST) THEN
        NHTMX = 100
        NTRMX = 5    ! max. # tracks in one direction
        NHTBA = 5    ! min. # tubes on track after magnet
        NHTMA = 4    ! min. # hits on track after magnet (with times)
        DRFMX = 1.5  ! max. value of drift distance 
        FIRST = .FALSE.
      END IF
      LMTRH = GZMTRH()
      LSAMT = GZSAMT()
      LSAHS = GZSAHS()
      HITS = 1 + GZSATW()
      IF (DIR .EQ. 1) THEN
        JTR = 1 + GZSTNA()
      ELSE
        JTR = 1 + GZSTSA()
      END IF
      NTRK = 0                 ! initialize # of tracks
C
C *** Find roads in SA-SB that point to WC
C
      CALL SATGSW(DIR, LTRG2, R0, AT0, PH0,BT0,SBH,SAH,
     &            MAGH,COSBM,TUBE_ADD,GEOM_ADD,NSH)
      IF (LTRG2 .LE. 0) RETURN ! if no roads are found then return
      LHITS_IN_ROAD = .FALSE.  ! no roads with hits yet
      NST = 3 * DIR - 1        ! Samus station B: station 2 (N) or 5 (S) 

C
C *** Setup the HITS area (hits on planes) and NHIT area (number of hits
C     on each plane) in SATW work bank. By now, only for SAMUS station B
C
      LINK = HITS + NHTMX * NPL
      NHIT = LINK + NHTMX * NPL
      WORK = NHIT + NPL + 1
      IQ(WORK) = 0             ! initialize # tubes associated to a given track
      DO PL = 1, NPL
        IQ(NHIT+PL) = 0                  ! reset number of hits in planes
      END DO
      DO IPL = 1, N_PLANES
        IND = 3 * (NST - 1) + IPL
        N = IQ(LSAHS+IND)                ! number of hits in plane
        LD = LQ(LSAHS-IND)               ! hits address
        DO I = 1, N                      ! hits loop
          IF (IQ(LD+1) .EQ. 3) THEN      ! good SSW hit for testing
            CALL SATRPL (LD, PL)
            L = IQ(NHIT+PL)              ! number of hits in plane
            L = L + 1
            IQ(NHIT+PL) = L
            K = (PL - 1) * NHTMX + L
            IQ(HITS+K) = LD
            IQ(LINK+K) = 0
          END IF
          LD = LD + 13
        END DO
      END DO
C
C *** Setup pointer for Wamus overlap hits work area at the END of SATW 
C
      WWAREA =  NHIT + NPL + 33
      LP = WWAREA + 1
      IQ(LP) = LTRG2                      ! first word: number of roads     
      DO I = 1, LTRG2  ! look for WAMUS C hits in this SAMUS roads
         CALL SSWHIT(DIR, R0(I), AT0(I), PH0(I), NSOL, PAV, 
     &               CH, ICH, NCH)
         IF (NSOL.EQ.1) THEN  ! check if there are WAMUS C hits in this road 
            LHITS_IN_ROAD = .TRUE.
            IQ(LP+1) = NCH    ! fill the Wamus work bank
            Q(LP+2)  = PAV(1)
            Q(LP+3)  = PAV(2)
            Q(LP+4)  = PAV(3)
            LP = LP + 4
            DO J = 1, NCH
               IQ(LP+1) = ICH(J,1)        ! plane (0,1,2,...11) up to four PDT's
               IQ(LP+2) = ICH(J,2)        ! module
               IQ(LP+3) = ICH(J,3)        ! MUOH address
               Q (LP+4) = CH(J,1)         ! XH
               Q (LP+5) = CH(J,2)         ! YH
               Q (LP+6) = CH(J,3)         ! ZH
               IQ(LP+7) = 0               ! status -> or MUOH flag????
               LP = LP + 7
            END DO
         ELSE
            IQ(LP+1)=0                    ! No WAMUS hits found in this road
            LP=LP+1
         END IF         
      END DO
      IF (.NOT. LHITS_IN_ROAD) RETURN  ! return if no roads with hits in WC
      NSAMIN = 6                 ! think later: minimum number of tubes in track
  100 CONTINUE
      MTRK = NTRK
      CALL SSWTWR (DIR, NSAMIN, NTRMX, NTRK) ! search tracks without time info.
      IF (NTRK .GE. NTRMX) GO TO 200
      IF (NTRK .GT. MTRK) THEN
        NSAMIN = NSAMIN - 1
        IF (NSAMIN .LT. NHTBA) GO TO 200
        GO TO 100
      END IF
  200 CONTINUE      
C
C ****  finding track after magnet with time information
C
      LINK = HITS + NPL
      NHIT = LINK + NPL
      NSAMIN = NHTMA
      DO NT = 1, NTRK
        LT = JTR + (NT - 1) * 64
        DO PL = 1, NPL
          IQ(NHIT+PL) = 0                  ! reset number of hits in planes
        END DO
        N = IQ(LT+1)                       ! number of tubes on track
        DO J = 1, N
          LD = IQ(LT+J+8)                  ! hit address
          DIST = Q(LD+4)
          IF (DIST .LT. DRFMX) THEN
            CALL SATRPL (LD, PL)
            IQ(HITS+PL) = LD
            IQ(NHIT+PL) = 1
          END IF
        END DO
        CALL SSWTCP (DIR, NSAMIN,N,NTRK)   ! search tracks with time info.
      END DO
C
      RETURN
      END
