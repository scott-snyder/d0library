      FUNCTION TB90_INSPILL_FINISH ()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Calculates means and sigmas for inspill pedestals.
C-                         Compares to double digitized pedestals.
C-                         Puts in zebra banks under STPN.
C-
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created  23-OCT-1990   Jan Guida
C-   Updated  31-DEC-1990   Jan Guida  Will no longer book the CPB8 bank or
C-                                       write the pedestal bank for runs
C-                                       without any inspill ped events
C-   Updated  21-JAN-1991   Jan Guida  Skip this routine if no pedestal file
C-                                      was read from the DBL3 data base
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL TB90_INSPILL_FINISH
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZSTPC.LINK'
      INCLUDE 'D0$LINKS:IZSCAL.LINK'
      INCLUDE 'D0$LINKS:IZCPDH.LINK'
      INCLUDE 'D0$LINKS:IZCPD8.LINK'
      INCLUDE 'D0$LINKS:IZCRCP.LINK'
      INCLUDE 'D0$PARAMS:PRTPDG.DEF'
      INCLUDE 'D0$PARAMS:BYTE_ORDER.PARAMS'
      CHARACTER*80 MSG_STRING
      CHARACTER*50 FILENAME
      CHARACTER*8 STIME
      REAL INPED(2,4608),SIG
      INTEGER CHAN,NEVT(4608)
      INTEGER IRUN,CRATE,CNTRLWRD
      INTEGER I,J,K,JDATE,JTIME
      INTEGER ADC,BLS,TWR,DEP
      INTEGER GZCPDH,LAST,LZLAST,LDT,LCPD8,LCPB8
      INTEGER NDATA,FULWRD
      INTEGER NTOTBD,ICRD,NTOT,NOK,NBAD,NCH,NBADFL(4608),BADFL
      INTEGER NBADLN,IBAD8,BADCHN
      INTEGER KSTPC,KSCAL,KCPDH,KSRCP
      INTEGER IERR,LUN,LOUT
      INTEGER*2 HLFWRD(2)
      EQUIVALENCE (FULWRD,HLFWRD)
      LOGICAL OK
      COMMON /INSPILL/INPED,NEVT,IRUN
C----------------------------------------------------------------------
      TB90_INSPILL_FINISH =  .TRUE.
C
      CALL IDATE(I,J,K)
      JDATE=I*10000+J*100+K
      CALL TIME(STIME)
      READ(STIME,'(I2,1X,I2,1X,I2)')I,J,K
      JTIME=I*10000+J*100+K
C
      LCPDH = GZCPDH()
      IF (LCPDH.LE.0) THEN              ! No pedestal bank from DBL3
        CALL INTMSG
     &    (' INSPILL_FINISH:  LCPDH = 0, NO DBL3 pedestal bank')
        GO TO 999
      ENDIF
      CNTRLWRD = IC(LCPDH+3)
      CRATE = IC(LCPDH+9)
C
      CALL BKSCAL('STPN',LSCAL)
      IC(LSCAL+4)=1
      IC(LSCAL+5)=999999999
      IC(LSCAL+6)=IRUN
      IC(LSCAL+7)=JDATE
      IC(LSCAL+8)=JTIME
C
      CALL BKCPDH(LSCAL,10,LCPDH)
      IC(LCPDH+3)=CNTRLWRD
      IC(LCPDH+4)=1
      IC(LCPDH+5)=999999999
      IC(LCPDH+6)=IRUN
      IC(LCPDH+7)=JDATE
      IC(LCPDH+8)=JTIME
      IC(LCPDH+9)=CRATE
C
      IC(LCPDH+10)=12                   ! Number of ADC cards
      NTOTBD = 0
      NDATA=NHEAD+384*2
      CALL UZERO(NBADFL,1,4608)
      BADCHN=0
C
      DO ICRD = 0,11
        IF(ICRD.EQ.0) THEN
          CALL BKCPD8(LCPDH,IZCPD8,NDATA,LCPD8)
        ELSE
          LAST=LZLAST(IDVSTP,LCPD8)
          CALL BKCPD8(LAST,0,NDATA,LCPD8)
        ENDIF
C
        DO 12 J=1,9
   12   IC(LCPD8+J)=IC(LCPDH+J)
        IC(LCPD8+11)=ICRD
        NTOT=0
        NOK=0
        NBAD=0
C
C--     Loop over expected ADC channels
C
        DO I = 1,384
          NCH=I
          LDT=LCPD8+NHEAD+(NCH-1)*2
          NTOT=NTOT+1
C
          CHAN = ICRD*384 + NCH
          IF (NEVT(CHAN).NE.0) THEN
            INPED(1,CHAN) = INPED(1,CHAN)/NEVT(CHAN)
            SIG = INPED(2,CHAN)/NEVT(CHAN) - INPED(1,CHAN)*INPED(1,CHAN)
            INPED(2,CHAN) = 0.
            IF(SIG.GT.0) INPED(2,CHAN) = SQRT(SIG)
          ELSE
            INPED(1,CHAN)=0.
            INPED(2,CHAN)=0.
          ENDIF
          IF (NEVT(CHAN).LT.10) BADCHN = BADCHN + 1     ! Not a good inspill ped
C
          C(LDT+1)=INPED(1,CHAN)
          C(LDT+2)=INPED(2,CHAN)
C
          CALL TB90_INSPILL_BAD
     &      (ICRD,I,INPED(1,CHAN),INPED(2,CHAN),BADFL)
          NBADFL(CHAN) = BADFL
          IF (BADFL.EQ.0) THEN
            NOK=NOK+1
          ELSE
            NBAD=NBAD+1
            NTOTBD=NTOTBD+1
          ENDIF
        ENDDO                           ! I=1,384
C
C--     Fill Header parts for two levels
C
        IC(LCPD8+12)=NTOT
        IC(LCPD8+13) =NOK
        IC(LCPD8+14) =NBAD
      ENDDO                           ! ICRD=0,11
C
C--   End loop over cards
C
      CALL GTUNIT(10,LOUT,IERR)
      WRITE(FILENAME,40)IRUN
   40 FORMAT('USR$OUT:RUN_',I7.7,'.BAD')
      CALL D0OPEN(LOUT,FILENAME,'OF',OK)
      IF(.NOT.OK) THEN
         WRITE(MSG_STRING,100)FILENAME
         CALL INTMSG(MSG_STRING)
         GOTO 999
      ENDIF
      WRITE(LOUT,200)IRUN
  200 FORMAT(//,' Bad Channel list for inspill pedestals, run ',I9)
      WRITE(LOUT,220)
  220 FORMAT(/,5X,'Bad channel flags:',
     &       /,5X,'0 - good channel',
     &       /,5X,'1 - mean too small (<100)',
     &       /,5X,'2 - mean too large (>500)',
     &       /,5X,'3 - mean different from double-digitized mean',
     &       /,5X,'4 - sigma different from double-digitized sigma',
     &       /,5X,'5 - mean and sigma different from double-digitized',
     &        ' sigma')
      WRITE(LOUT,202)NTOTBD
  202 FORMAT(/,' Total number of bad channels ',I5,/)
C
      NBADLN = NTOTBD + 10    !SPARE
      LCPD8 = LC(LCPDH-IZCPD8)
      CALL BKCPB8(LCPD8,NBADLN,LCPB8)
      IC(LCPB8+1) = NTOTBD    !Total number of bad channels
      IF (BADCHN.GT.4000) THEN        ! Not enough good inspill pedestal events
        WRITE(LOUT,300) BADCHN
  300   FORMAT(5X,' There are ',I5,' channels with less than 10 ',
     &    'inspill pedestal events'/,
     &    5X,' No inspill pedestal file will be written')
      ELSE
        IBAD8 = 0
        DO ADC = 0,11
          DO BLS = 0,7
            DO TWR = 0,3
              DO DEP = 0,11
                CHAN = ADC*384 + BLS*48 + TWR*12 + DEP + 1
                IF(NBADFL(CHAN).NE.0) THEN        ! IF HERE BAD FLAG
                  HLFWRD(WORD1) = NBADFL(CHAN)
                  HLFWRD(WORD2) = (ADC*16*4*8+BLS*16*4+TWR*16+DEP+1)*2
                  IBAD8 = IBAD8 + 1
                  IC(LCPB8+IBAD8+1) = FULWRD
                  WRITE(LOUT,201)ADC,BLS,TWR,DEP,
     &            INPED(1,CHAN),INPED(2,CHAN),NBADFL(CHAN)
  201             FORMAT(' ADC=',I2,3X,'BLS=',I1,3X,'TWR=',I1,3X,'DEP=',
     &            I2,3X,2F10.3,5X,I1)
                ENDIF
              ENDDO                       ! IDEP
            ENDDO                         ! ITWR
          ENDDO                           ! IBLS
        ENDDO                             ! IADC
      ENDIF
C
      IF(IBAD8 .NE. NTOTBD)THEN
        WRITE(MSG_STRING,101)IBAD8,NTOTBD
  101   FORMAT(' Wrong logic in x8 Bad flags ',2I5)
        CALL INTMSG(MSG_STRING)
      ENDIF
C
      CLOSE (UNIT=LOUT)
      CALL RLUNIT(10,LOUT,IERR)
C
C--   Store PEDESTALS banks
C
      KSTPC = LC(LSTPH-IZSTPC)
      KSCAL = LC(KSTPC-IZSCAL)
      KCPDH = LC(KSCAL-IZCPDH)
      KSRCP = LC(KCPDH-IZCRCP)
      CALL ZSHUNT(IXSTP,KSRCP,LCPDH,-IZCRCP,0)
C
      WRITE(FILENAME,50)IRUN
   50 FORMAT('USR$OUT:RUN_',I7.7,'.PEDS')
      IF (BADCHN.LE.4000) THEN        ! Not enough good inspill pedestal events
        CALL GTUNIT(12,LUN,IERR)
        CALL D0OPEN(LUN,FILENAME,'OU',OK)
        IF(.NOT.OK) THEN
          WRITE(MSG_STRING,100)FILENAME
          CALL INTMSG(MSG_STRING)
          GOTO 999
        ENDIF
        CALL FZFILE(LUN,0,'O')
        CALL FZLOGL(LUN,-2)
        CALL FZOUT(LUN,IDVSTP,LCPDH,1,' ',1,0,0)
        CALL FZENDO(LUN,'T')
        CLOSE (UNIT=LUN)
        CALL RLUNIT(12,LUN,IERR)
      ENDIF
C
  100   FORMAT(' Cannot open file, ',A)
C
  999 RETURN
      END
