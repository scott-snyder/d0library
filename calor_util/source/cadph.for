      SUBROUTINE CADPH(CRATE,ADC,BLS,ROTOW,DEPTH,IETAC,IPHIC,ILYRC,
     &  ICOND)
C--------------------------------------------------------------------
C
C   Purpose and Methods : Converts an address in the ADC system to the
C                         PHysics system used in analysis. The inputs
C                         are just the separated bit fields from the 
C                         address area of the raw data word. In the
C                         current version, CRATE, ADC, and DEPTH are 
C                         checked for validity of range. (The other
C                         variables fill their bit fields and don't
C                         need checking if provided by CADUPK.) 
C                         Reference for the conversion is D0 Note 774. 
C                         (Inverse transformation done by CPHAD.FOR) 
C
C   Inputs  : CRATE     ADC crate number         [NCRATE*10+BANK] 
C                            where NCRATE=0,5 and BANK=7,8
C                            old version [96,101],[112,117] (before 3/90)
C             ADC       ADC card number in crate [0,11]
C             BLS       BLS number in ADC        [0,7]
C             ROTOW     readout tower in BLS     [0,3]
C             DEPTH     depth in readout tower   [0,11]
C
C   Outputs : IETAC     offline eta index        [-37,-1],[1,37]
C             IPHIC     offline phi index        [1,64]
C             ILYRC     offline radial index     [1,17]
C             ICOND     return code: 0 = OK
C                                    1 = invalid CRATE input
C                                    2 = invalid combination of
C                                        ADC,BLS,ROTOW,DEPTH
C
C    Created   6-JUN-1988   A.P. White
C    Updated  16-JAN-1989   K. Wyatt Merritt 
C    Updated  12-JUN-1989   K. Wyatt Merritt   revised MG/ICD cabling
C                      as per J. Kourlas' memo to Linnemann, 5/24/89
C-   Updated  28-FEB-1990   Chip Stewart   - New CRATE number scheme
C-   Updated  30-MAY-1992   Joan Guida  - fix for disconnected channels
C-   Updated  21-NOV-1992  Joan Guida  for missing main-ring channels
C
C--------------------------------------------------------------------
C
      IMPLICIT NONE
C
      INTEGER CRATE,ADC,BLS,ROTOW,DEPTH,IETAC,IPHIC,ILYRC,ICOND
      INTEGER DATCAB,NC
      INTEGER IESGN,TIMSL,SUBPHI,INDXTS,NTSCRT,INDXPH
      INTEGER IEE,ID
      LOGICAL FIRST,OLD
      INCLUDE 'D0$PARAMS:CAL_ADC_NO.PARAMS'
C
C-----------------------------------------------------------------------
      DATA FIRST /.TRUE./
C-----------------------------------------------------------------------
      IF (FIRST) THEN
        FIRST = .FALSE.
        NTSCRT  = NADCC * 16     ! 192  # time slices in one full crate 
C                                 ! 16 time slices per ADC card
      END IF
C-----------------------------------------------------------------------
C
      ICOND = 0                  !Initialize return code to OK setting
      OLD = CRATE.GT.58
C
      IF (OLD) THEN
        DATCAB = CRATE/16 + 1      !Calulate data cable (7 or 8)
        NC = MOD(CRATE,16)         !Get crate # within cable ([0,5])
      ELSE
        DATCAB = MOD(CRATE,10)     !Calulate data cable (7 or 8)
        NC = CRATE /10             !Get crate # within cable ([0,5])        
      END IF

      IF (ADC .GT. 11) THEN      ! Check for invalid ADC number;
        ICOND = 2                !   set code
        GO TO 999                !   and return
      ENDIF
C
C
C       Find the sign of IETAC:
C
      IF (DATCAB .EQ. 7) THEN
        IESGN = -1                                   ! North half: - z
      ELSE IF (DATCAB .EQ. 8) THEN
        IESGN = +1                                   ! South half: + z
      ELSE       
        ICOND = 1                                    ! Invalid CRATE
        GO TO 999
      END IF
C
C       Find the physics variables:
C
      TIMSL = 2*BLS + ROTOW/2               ! time slice in this ADC
      SUBPHI = MOD(ROTOW,2)                 ! phi index inside time slice  
      IF (NC.LE.1) THEN                               
C
C            For the Central Calorimeter:
C
C           MISSING EC MAIN RING CHANNELS
        IF (CRATE.EQ.7 .AND. ADC.EQ.0) THEN
          IF (BLS.EQ.3 .AND. ROTOW.EQ.3 .AND. DEPTH.EQ.10) THEN
            ICOND=2
            GO TO 999
          ENDIF
          IF (BLS.EQ.4 .AND. ROTOW.EQ.1 .AND. DEPTH.EQ.10) THEN
            ICOND=2
            GO TO 999
          ENDIF
          IF (BLS.EQ.4 .AND. ROTOW.EQ.3 .AND. DEPTH.EQ. 9) THEN
            ICOND=2
            GO TO 999
          ENDIF
          IF (BLS.EQ.5 .AND. ROTOW.EQ.1 .AND. DEPTH.EQ.10) THEN
            ICOND=2
            GO TO 999
          ENDIF
        ENDIF
        IF (CRATE.EQ.18 .AND. ADC.EQ.11) THEN
          IF (BLS.EQ.5 .AND. ROTOW.EQ.2 .AND. DEPTH.EQ.10) THEN
            ICOND=2
            GO TO 999
          ENDIF
          IF (BLS.EQ.6 .AND. ROTOW.EQ.0 .AND. DEPTH.EQ.10) THEN
            ICOND=2
            GO TO 999
          ENDIF
          IF (BLS.EQ.6 .AND. ROTOW.EQ.2 .AND. DEPTH.EQ. 9) THEN
            ICOND=2
            GO TO 999
          ENDIF
          IF (BLS.EQ.7 .AND. ROTOW.EQ.0 .AND. DEPTH.EQ.10) THEN
            ICOND=2
            GO TO 999
          ENDIF
        ENDIF
C
        INDXTS = NC*NTSCRT + ADC*16 + TIMSL ! Find time slice index: 0-383
C              get IETAC
        IETAC = MOD(INDXTS,12) + 1          ! 12 time slices/sector for CC
        IETAC = IESGN * IETAC               ! Eta index gets its proper sign
C              get IPHIC
        INDXPH = INDXTS/12                  ! Readout phi index (= sector)
        IF (IESGN.EQ.1) THEN                ! For + z, the physics phi index
          IPHIC = 80 - 2*INDXPH - SUBPHI    ! runs opposite to INDXPH ...
        ELSE IF (IESGN .EQ. -1) THEN        ! For - z, the physics phi index
          IPHIC = 17 + 2*INDXPH + SUBPHI    ! runs with INDXPH ...
        ENDIF
        IF (IPHIC .GT. 64) IPHIC = IPHIC - 64 ! ... & is displaced 1 quadrant
C                                             ! clockwise
C              get ILYRC
        CALL CDPLYR(DEPTH,IETAC,ILYRC)    ! Lookup for layer index
        IF (ILYRC .EQ. -1) THEN           ! Check for invalid return
          ICOND = 2                       !   from lookup table; set code
          GO TO 999                       !   and return
        END IF
C
      ELSE IF (NC .LE. 5) THEN        
C
C            For the End Calorimeters:
C
        INDXTS = (NC-2)*NTSCRT + ADC*16 + TIMSL ! Find time slice index: 0-767
C               get modified eta index
        IEE = MOD(INDXTS,24) + 1             ! 24 time slices/sector
        ILYRC = 0                            ! initialize to allow later
C                                            ! checking for special condition
C
C          EC is not simple: big IF structure to get IETAC, ILYRC, and 
C           SUBPHI for all the special cases:  ICD
C                                              odd eta massless gaps
C                                              even eta massless gaps
C                                              regular EC (0.1 eta x 0.1 phi)
C                                              wide EC (variable eta x 0.2 phi)
C          See D0 Note 774, especially p. 7 and pp. 13-15.
C
        IF(IEE.EQ.24 .AND. SUBPHI.EQ.0) THEN     ! ICD :
          IF (DEPTH .GT. 11) THEN            ! Check for invalid depth;
            ICOND = 2                       !   set code and
            GO TO 999                       !   return
          END IF
          IETAC = (DEPTH/2) + 9             ! Get eta from DEPTH
          IETAC = IESGN * IETAC             !  and give it the proper sign
          SUBPHI = MOD(DEPTH,2)             ! Reset SUBPHI from DEPTH
          ILYRC = 9                         ! ILYRC fixed for ICD
          GO TO 100                         ! Branch to code for IPHIC
        ELSE IF (IEE.EQ.23 .AND. SUBPHI.EQ.1) THEN  ! Massless gap (odd eta)
          IF (DEPTH .GT. 11) THEN           ! Check for invalid depth;
            ICOND = 2                       !   set code and
            GO TO 999                       !   return
          END IF
          ID = DEPTH/4                      ! Modify DEPTH to get proper
C                                           !  pointer to eta
          IETAC = 2*ID + 8                  ! Get IETAC from modified DEPTH
          IETAC = IESGN * IETAC             !  and give it the proper sign
          SUBPHI = MOD(DEPTH/2,2)           ! Reset SUBPHI from DEPTH
          ILYRC = 8                         ! ILYRC if CC MG
          IF (MOD(DEPTH,2).EQ.1) ILYRC = 10 ! ILYRC if EC MG
          GO TO 100                         ! Branch to code for IPHIC
        ELSE IF (IEE.EQ.24 .AND. SUBPHI.EQ.1) THEN  ! Massless gap (even eta)
          IF (DEPTH .GT. 9) THEN            ! Check for invalid depth;
            ILYRC = -1                      ! Disconnected channel
            ICOND = 2                       !   set code and
            GO TO 999                       !   return
          END IF
          ID = DEPTH/4                      ! Modify DEPTH to get proper
C                                           !  pointer to eta
          IETAC = 2*ID + 9                  ! Get IETAC from modified DEPTH
          IETAC = IESGN * IETAC             !  and give it the proper sign
          SUBPHI = MOD(DEPTH/2,2)           ! Reset SUBPHI from DEPTH
          ILYRC = 8                         ! ILYRC if CC MG
          IF (MOD(DEPTH,2).EQ.1) ILYRC = 10 ! ILYRC if EC MG
          IF (ABS(IETAC).EQ.13) THEN        ! Special case: no CC for IE=13
            SUBPHI = MOD(DEPTH,2) 
            ILYRC = 10
          ENDIF
          GO TO 100                         ! Branch to code for IPHIC
        ELSE                                   ! Regular EC
          IF(IEE.LE.20) THEN                  ! Eta slices of 0.1
            IETAC = IEE+12                  ! Add offset of 12 for the CC eta's
            IETAC = IESGN * IETAC           !  and give it the proper sign
            GO TO 100
          ELSE IF (IEE.EQ.21 .AND. SUBPHI.EQ.0) THEN ! Wider eta's:
            IETAC = 33                               ! IETAC set explicitly
            IETAC = IESGN * IETAC                    ! for each eta/phi slice;
            SUBPHI = MAX0(IESGN,0)                  ! SUBPHI always reset
            GO TO 100                                ! because only
          ELSE IF (IEE.EQ.21 .AND. SUBPHI.EQ.1) THEN ! odd values of IPHIC
            IETAC = 34                               ! exist for IETAC > 32;
            IETAC = IESGN * IETAC                    ! SUBPHI = 1 for + z
            SUBPHI = MAX0(IESGN,0)                  !          0 for - z
            GO TO 100
          ELSE IF (IEE.EQ.22 .AND. SUBPHI.EQ.0) THEN
            IETAC = 35
            IETAC = IESGN * IETAC
            SUBPHI = MAX0(IESGN,0)
            GO TO 100
          ELSE IF (IEE.EQ.22 .AND. SUBPHI.EQ.1) THEN
            IETAC = 36
            IETAC = IESGN * IETAC
            SUBPHI = MAX0(IESGN,0)
            GO TO 100
          ELSE IF (IEE.EQ.23 .AND. SUBPHI.EQ.0) THEN
            IETAC = 37
            IETAC = IESGN * IETAC
            SUBPHI = MAX0(IESGN,0)
          ENDIF
        ENDIF
  100   CONTINUE
C          get ILYRC from lookup table (except for ICD, MG)
        IF (ILYRC.GE.8 .AND. ILYRC.LE.10) GO TO 101
        CALL CDPLYR(DEPTH,IETAC,ILYRC)    ! Lookup for layer index
        IF (ILYRC .EQ. -1) THEN           ! Check for invalid return
          ICOND = 2                       !   from lookup table; set code
          GO TO 999                       !   and return
        END IF
C          get IPHIC for all the EC cases
  101   CONTINUE
        INDXPH = INDXTS/24                  ! Readout phi index (= sector)
        IF (IESGN.EQ.1) THEN                ! For + z, the physics phi index
          IPHIC = 80 - 2*INDXPH - SUBPHI    ! runs opposite to INDXPH ...
        ELSE IF (IESGN .EQ. -1) THEN        ! For - z, the physics phi index
          IPHIC = 17 + 2*INDXPH + SUBPHI    ! runs with INDXPH ...
        ENDIF
        IF (IPHIC .GT. 64) IPHIC = IPHIC - 64 ! ... & is displaced 1 quadrant
C
      ELSE              ! Not EC or CC!
        ICOND = 1                                       ! Invalid CRATE
      ENDIF
C
  999 RETURN
      END

