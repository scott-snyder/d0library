      SUBROUTINE MNT0_END
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Calculate scintillator T0 corrections and update
C-                         the MSTC banks
C-
C-   Inputs  : None
C-   Outputs : None
C-   Controls:
C-
C-   Created  10-NOV-1994   Rich Markeloff
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBSTP.INC'
C
      INTEGER CALIB_T, IER, I, J, K, LMSTC, NTZER(0:1), IPOINT
      INTEGER NSMOD, IETA, RUN, ITIME, JTIME, JDATE, LUN
      INTEGER DELAY_OLD, DELAY_NEW, LMUMOD, NMODUL, MODNUMS(400)
      PARAMETER (NSMOD=30)
      INTEGER SMOD(NSMOD)
      INTEGER SSUNIT, GZMSTC
C
      CHARACTER*26 TIME
C
      REAL    NENTRIES
      REAL    HSUM
C
      REAL    CONST, MEAN, SD, CHI2, SIG(3), TZER, TSLP, TZER_SUM(0:1)
      REAL    AV_TOF(0:4), TSLP_SUM(0:1), TCOARSE, CORRECT, AV_TZER
      REAL    AV_TSLP, F1, F2, F3, F4
C
      DATA SMOD/ 200,201,202,203,204,207, 210,211,212,213,214,217,
     1           220,221,222,223,224,227, 230,231,232,233,234,237,
     2           240,241,242,243,244,247 /
C
      DATA AV_TOF / 33., 29., 27., 29., 33. / ! Average TOF + WLS delay
C
C----------------------------------------------------------------------
C
C     Get time and date
C
      CALL MUJUNK(1,RUN,ITIME,JTIME,JDATE,F1,F2,F3,F4)
      CALL OFTSTR(ITIME,TIME)
C
C     Check to see if KMUMOD option is chosen in RCP
C
      CALL EZPICK('MURECO_RCP')
      CALL EZGET('LMUMOD',LMUMOD,IER)
      IF (LMUMOD .EQ. 4) THEN
        CALL EZGSET('KMUMOD()',NMODUL,1)       ! Number of modules
        CALL EZGSET('KMUMOD',MODNUMS(1),1)         ! Array of module numbers
      ENDIF
C
C     Get CALIB_T RCP switch
C
      CALL EZGET('CALIB_T',CALIB_T,IER)
      CALL EZRSET
C
C     Get logical unit for standard output
C
      LUN = SSUNIT()
C
      IF (CALIB_T .EQ. 4) THEN
C
C       Do coarse T0 calculation
C
        WRITE(LUN,400)
  400   FORMAT(' ****** SCINTILLATOR COARSE T0 CALCULATION ****** ',/)
        WRITE(LUN,420)
  420   FORMAT(
     &' MODULE     ENTRIES       COARSE T0    OLD DELAY      NEW DELAY'
     &    ,/)
C
        DO 100 I = 1, NSMOD
C
C         Only do modules on list if LMUMOD = 4
C
          IF (LMUMOD .EQ. 4) THEN
            DO J = 1, NMODUL
              IF (SMOD(I) .EQ. MODNUMS(J)) GOTO 50
            ENDDO
            GOTO 100
          ENDIF
C
   50     CONTINUE
C
          LMSTC = GZMSTC(SMOD(I))
          IF (LMSTC .NE. 0) THEN
C
C           Update header words
C
            IC(LMSTC+1) = 100
            IC(LMSTC+6) = RUN
            IC(LMSTC+7) = JDATE
            IC(LMSTC+8) = JTIME
C
C           Update constants if statistics are sufficient
C
            NENTRIES = HSUM(41000+SMOD(I))
            IF (NENTRIES .GT. 40.) THEN
              CALL HFITGA(41000+SMOD(I),CONST,MEAN,SD,CHI2,112,SIG)
C
C             Loop over phototubes
C
              DO K = 0,1
                TZER_SUM(K) = 0.
                TSLP_SUM(K) = 0.
                NTZER(K)= 0
C
C               Loop over scintillators
C
                DO J = 0,7
                  IPOINT = LMSTC + 17 + J*8 + K*4
                  IF (C(IPOINT) .NE. 0.) THEN
                    TZER_SUM(K) = TZER_SUM(K) + C(IPOINT)
                    TSLP_SUM(K) = TSLP_SUM(K) + C(IPOINT+1)
                    NTZER(K)= NTZER(K) + 1
                  ENDIF
                ENDDO
C
C               Roughly estimate T0 correction from average time of flight
C
                IETA = (SMOD(I) - 200)/10
                IF (NTZER(K) .GT. 0) THEN
                  AV_TZER = TZER_SUM(K)/NTZER(K)
                  AV_TSLP = TSLP_SUM(K)/NTZER(K)
                  TCOARSE = AV_TOF(IETA)/AV_TSLP + MEAN
                  CORRECT = AV_TZER - TCOARSE
                ELSE
                  TCOARSE = 6.8*AV_TOF(IETA) + MEAN
                  AV_TSLP = 1./6.8
                  CORRECT = 0.
                ENDIF
C
C               Update constants in MSTC bank
C
                C(LMSTC + 13 + K) = TCOARSE             ! Average T0
                C(LMSTC + 15 + K) = AV_TSLP             ! Average slope
                DO J = 0,7
                  IPOINT = LMSTC + 17 + J*8 + K*4
                  C(IPOINT) = C(IPOINT) - CORRECT
                ENDDO
              ENDDO
C
C             Update cable delay
C
              IF (NTZER(0) .GT. 0 .OR. NTZER(1) .GT. 0) THEN
C
C               Calculate averages over all channels
C
                AV_TSLP = (TSLP_SUM(0)+TSLP_SUM(1))/(NTZER(0)+NTZER(1))
                AV_TZER = (TZER_SUM(0)+TZER_SUM(1))/(NTZER(0)+NTZER(1))
                TCOARSE = AV_TOF(IETA)/AV_TSLP + MEAN
                CORRECT = AV_TZER - TCOARSE
              ENDIF
              DELAY_OLD = IC(LMSTC+12)
              DELAY_NEW = DELAY_OLD - CORRECT*AV_TSLP
              IC(LMSTC+12) = DELAY_NEW
              WRITE(LUN,450)SMOD(I),NENTRIES,TCOARSE,DELAY_OLD,
     &          DELAY_NEW
  450         FORMAT(4X,I3,4X,F7.0,7X,F8.2,3X,I10,5X,I10)
C
C             Update more header words
C
              IC(LMSTC+1) = 101
              IC(LMSTC+2) = 100.*(NTZER(0) + NTZER(1))/16. !Good channel frac.
              IC(LMSTC+3) = 1000.*SD*AV_TSLP  ! Resolution in picoseconds
            ELSE
C
C             Insufficient statistics for tuning
C
              WRITE(LUN,450)SMOD(I),NENTRIES
            ENDIF
          ELSE
            WRITE(LUN,460)SMOD(I)
  460       FORMAT(' NO MSTC BANK FOR MODULE ',I3)
            CALL ERRMSG('NO MSTC BANK','MNT0_END',' ','W')
          ENDIF
  100   CONTINUE
      ELSEIF (CALIB_T .EQ. 2 .OR. CALIB_T .EQ. 3) THEN
C
C       Do fine corrections to scintillator T0
C
        WRITE(LUN,300)
  300   FORMAT(' ****** SCINTILLATOR FINE T0 CALCULATION ****** ',/)
        WRITE(LUN,320)
  320   FORMAT(
     &    '   MODULE   ENTRIES       T0 CORRECTION      RESOLUTION',/)
C
        DO 200 I = 1, NSMOD
C
C         Only do modules on list if LMUMOD = 4
C
          IF (LMUMOD .EQ. 4) THEN
            DO J = 1, NMODUL
              IF (SMOD(I) .EQ. MODNUMS(J)) GOTO 150
            ENDDO
            GOTO 200
          ENDIF
C
  150     CONTINUE
c
          LMSTC = GZMSTC(SMOD(I))
          IF (LMSTC .NE. 0) THEN
C
C           Update header words
C
            IC(LMSTC+1) = 100
            IC(LMSTC+6) = RUN
            IC(LMSTC+7) = JDATE
            IC(LMSTC+8) = JTIME
C
C           Update constants if statistics are sufficient
C
            NENTRIES = HSUM(40000+SMOD(I))
            IF (NENTRIES .GT. 40.) THEN
              CALL HFITGA(40000+SMOD(I),CONST,MEAN,SD,CHI2,112,SIG)
              CALL HFILL(49500,MEAN,0.,1.)
              CALL HFILL(49600,SD,0.,1.)
C
C             Loop over phototubes
C
              DO K = 0,1
                TZER_SUM(K) = 0.
                NTZER(K) = 0
C
C               Loop over scintillators
C
                DO J = 0,7
                  IPOINT = LMSTC + 17 + J*8 + K*4
                  TZER = C(IPOINT)
                  IF (TZER .NE. 0.) THEN
                    TSLP = C(IPOINT+1)
                    C(IPOINT) = TZER + MEAN/TSLP
                    TZER_SUM(K) = TZER_SUM(K) + C(IPOINT)
                    NTZER(K) = NTZER(K) + 1
                  ENDIF
                ENDDO
C
C               Update average value
C
                IF (NTZER(K) .GT. 0) C(LMSTC+13+K) =
     &            TZER_SUM(K)/NTZER(K)
              ENDDO
              WRITE(LUN,350)SMOD(I),NENTRIES,MEAN,SD
  350         FORMAT(4X,I3,4X,F7.0,7X,F10.4,8X,F10.4)
C
C             Update more header words
C
              IC(LMSTC+1) = 101
              IC(LMSTC+2) = 100.*(NTZER(0) + NTZER(1))/16. !Good channel frac.
              IC(LMSTC+3) = 1000.*SD  ! Resolution in picoseconds
            ELSE
C
C             Insufficient statistics for tuning
C
              WRITE(LUN,350)SMOD(I),NENTRIES
            ENDIF
          ELSE
            WRITE(LUN,460)SMOD(I)
            CALL ERRMSG('NO MSTC BANK','MNT0_END',' ','W')
          ENDIF
  200   CONTINUE
      ELSE
        CALL ERRMSG('MUST CHOOSE FINE OR COARSE SCINT. T0 CALIB.',
     &    'MNT0_END',' ','W')
      ENDIF
  999 RETURN
      END
