      SUBROUTINE VTX_HVCHK(VAL,DELTA_V,GOOD)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Check 
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  11-DEC-1992   Ed Oltman
C-   Updated  30-MAR-1993   Ed Oltman  Treat HV record as single one -- either
C-                                 its all good or not good (GOOD(6) --> GOOD) 
C-   Updated  15-FEB-1994   Ed Oltman  ADD CALL TO VDYNSAVE 
C-   Updated  25-FEB-1994   Ed Oltman  Bug fix: call VTX_LUMDTM(0.) ...
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL VAL(*),DELTA_V(*)
      LOGICAL GOOD
      INCLUDE 'D0$INC:DBMUKEY.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
C locals:
      LOGICAL FIRST,BYPASS_DBL3_ERROR,NO_CORRECTION
      INTEGER MAX_PER_CAT
      PARAMETER (MAX_PER_CAT = 20)
      INTEGER CATEG(192),CAT,STAT(MAX_PER_CAT,6),ERROR,NCAT(6)
      INTEGER OFF(6),DYN(6),NODYN(6)
      REAL    SUM1(3,6),AVE,SIG,SIG_MAX,CURR,PAR(3,192)
      REAL    WORK(MAX_PER_CAT,6)
      INTEGER NSEC(192),SUPPLY(2,0:31,0:2),DEVSUP(96)
      REAL    V_TGT(192),RESISTOR(6),REFF(96),ISCALE,HV_TOL,SCALES(2)
      INTEGER NCH,SUP,LAYER,SECTOR,NS(0:2),FEED,ERR,I,N
      INTEGER NEWSCALE_RUN,LEN,IDATE,ITIME,PRUNIT,RUN,LAST_RUN,LVDTM
      REAL    LUM,RATE,LUM_CORR,DMIN,DAVE,DMAX
      CHARACTER*80 TXT
      CHARACTER*1  SEVER
c External:
      INTEGER RUNNO,TRULEN,GZVDTM
      INTEGER USUNIT
c Data:
      DATA FIRST/.TRUE./
      DATA NS  /15,31,31/
      DATA LAST_RUN/-1/
      DATA NO_CORRECTION/.FALSE./
C----------------------------------------------------------------------
      IF (FIRST) THEN
        PRUNIT = USUNIT()
        FIRST = .FALSE.
        CALL VTX_IVLPAR(PAR)
        CALL EZPICK('VTRAKS_RCP')
        CALL EZGET('BYPASS_DBL3_ERROR',BYPASS_DBL3_ERROR,ERR)
        ERROR = ERR
        CALL EZGETA('HV_ISCALE',1,1,0,NEWSCALE_RUN,ERR)
        ERROR = ERROR + ERR
        CALL EZGETA('HV_ISCALE',2,3,1,SCALES,ERR)
        ERROR = ERROR + ERR
        CALL EZGET('HV_SIG_MAX',SIG_MAX,ERR)
        ERROR = ERROR + ERR
        CALL EZGET('HV_REFF',RESISTOR,ERR)
        ERROR = ERROR + ERR
        CALL EZGET('HV_LUMCONV',RATE,ERR)
        ERROR = ERROR + ERR
        CALL EZGET('HV_LUMCORR',LUM_CORR,ERR)
        ERROR = ERROR + ERR
        CALL EZGET('HVREAD_TOL',HV_TOL,ERR)
        ERROR = ERROR + ERR
        CALL EZRSET
        SEVER = 'F'
        IF (BYPASS_DBL3_ERROR) SEVER = 'W'
        IF (ERROR .NE. 0) THEN
          CALL ERRMSG('VTX HV corr NOT made','VTX_HVCHK',
     &      'VTRAKS_RCP errors -- cannot apply HV correction',SEVER)
          NO_CORRECTION = .TRUE.
          GO TO 999
        ENDIF
      ENDIF
      RUN = RUNNO()
      IF (RUN .NE. LAST_RUN) THEN
        LAST_RUN = RUN
        CALL VTX_HVSLOG(NSEC,SUPPLY,V_TGT)
        ISCALE = 1./SCALES(1)
        IF (RUN .GE. NEWSCALE_RUN) ISCALE = 1./SCALES(2)
        NCH = 0
        CALL VZERO(NCAT,6)
        DO SUP = 1,192
          IF (NSEC(SUP) .GT. 0) THEN
            DO LAYER = 0,2
              DO SECTOR = 0,NS(LAYER)
                DO FEED = 1,2
                  IF (SUP .EQ. SUPPLY(FEED,SECTOR,LAYER)) GO TO 10
                ENDDO
              ENDDO
            ENDDO
            CALL ERRMSG('VTX HV corr NOT made','VTX_HVCHK',
     &        'VTRAKS_RCP has inconsistant HV - logical assignments',
     &        SEVER)
   10       CAT = 2*LAYER + FEED
            NCAT(CAT) = NCAT(CAT) + 1
            IF (NCAT(CAT) .GT. MAX_PER_CAT) THEN
              WRITE(TXT,'(A,I3,A,2I3)') 'Encountered more then ',
     &          MAX_PER_CAT,' HV supplies for Layer,Feed ',LAYER,FEED
              LEN = TRULEN(TXT)
              CALL ERRMSG('VTX HV corr NOT made','VTX_HVCHK',TXT(1:LEN),
     &          SEVER)
              NO_CORRECTION = .TRUE.
              GO TO 999
            ENDIF
            CATEG(NCH/2+1)= CAT
            REFF(NCH/2+1) = RESISTOR(CAT)/NSEC(SUP)
            DEVSUP(NCH/2+1) = SUP
            NCH = NCH + 2
          ENDIF
        ENDDO
      ENDIF
C
C -------------------->   Normal entry
C
      GOOD = .FALSE.
      IF (NO_CORRECTION) GO TO 999
      GOOD = .TRUE.
      LUM = 1. - LUM_CORR*RATE*VAL(NCH+1)
      IF ( (LUM .GT. 0.) .AND. (LUM .LT. 1.) ) THEN
        LUM = -ALOG(LUM)/LUM_CORR
      ELSE
        LUM = -999.
      ENDIF
      CALL VDYNSAVE('LUM',LUM,DBKEYS(3))
      CALL VZERO(SUM1,18)
      CALL VZERO(NCAT,6)
      CALL VZERO(OFF,6)
      CALL VZERO(DYN,6)
      CALL VZERO(NODYN,6)
C
C ****  array VAL contains HV voltages and currents -- the array is densely
C ****  packed -- V(25),I(25),...,V(192),I(192).  
C ****  Compute the voltage sag: V_anode - V_target:
C

      DO I = 1,NCH/2
        CAT = CATEG(I)
        NCAT(CAT) = NCAT(CAT) + 1
        SUP = DEVSUP(I)
        WORK(NCAT(CAT),CAT) = 0.
        IF (VAL(2*I) .GT. -99.) THEN
          IF ( 1000.*VAL(2*I) .LT. V_TGT(SUP) - HV_TOL) THEN
C..HV less then 100% -- no corrections made
            OFF(CAT) = OFF(CAT) + 1
            STAT(NCAT(CAT),CAT) = 0
          ELSEIF(1000.*VAL(2*I) .LT. V_TGT(SUP) + HV_TOL) THEN
c..HV dynamic adjustment not on: use lum. based scheme
            IF (LUM .GT. 0.)  THEN 
              NODYN(CAT) = NODYN(CAT) + 1
              STAT(NCAT(CAT),CAT) = 1
              CURR = PAR(1,SUP) + PAR(2,SUP)*LUM + PAR(3,SUP)*LUM**2
              WORK(NCAT(CAT),CAT) = -CURR*RESISTOR(CAT)
            ELSE
              STAT(NCAT(CAT),CAT) = -1
            ENDIF
          ELSE
c..HV dynamic adjustment probably underway
            DYN(CAT) = DYN(CAT) + 1
            STAT(NCAT(CAT),CAT) = 2
            WORK(NCAT(CAT),CAT) = 
     &        1000.*VAL(2*I) - REFF(I)*ISCALE*VAL(2*I-1) - V_TGT(SUP)
            SUM1(1,CAT) = SUM1(1,CAT) + 1.
            SUM1(1,CAT) = SUM1(1,CAT) + WORK(NCAT(CAT),CAT)
            SUM1(1,CAT) = SUM1(1,CAT) + WORK(NCAT(CAT),CAT)**2
          ENDIF
        ELSE
          STAT(NCAT(CAT),CAT) = -1
        ENDIF
      ENDDO
C
C ****  Look at the NCAT supplies in each category:  Check spread in values of
C ****  the delta_V -- they should be "small"
C
      CALL DBUPTS(IDATE,ITIME,DBKEYS(3))
      IF (PRUNIT .GT. 0) WRITE(PRUNIT,'(A,2I10,A,F10.3)') 
     &  ' HV sagging cor: Date, Time ',IDATE,ITIME,' Luminosity ' ,LUM
      DO CAT = 1,6
        IF ( (DYN(CAT) .GT. 5) .OR. (NODYN(CAT) .GT. 5) ) THEN
          SIG = 0.
          IF (SUM1(1,CAT) .GE. 3) THEN
            AVE = SUM1(2,CAT)/SUM1(1,CAT)
            SIG = SQRT(AMAX1(SUM1(3,CAT)/SUM1(1,CAT) - AVE*AVE,0.))
            IF (SIG .GT. SIG_MAX) THEN
              WRITE(TXT,'(A,I3,A,F10.3)') 
     &          'Large RMS(V_sag) for cat = ',cat,': ',SIG
              LEN = TRULEN(TXT)
              CALL ERRMSG('VTX HV corr large RMS','VTX_HVCHK',
     &          TXT(1:LEN),'W')
            ENDIF
          ENDIF
          N = 0
          DMIN =  9999.
          DMAX = -9999.
          DAVE = 0.
          DO I = 1,NCAT(CAT)
            IF (STAT(I,CAT) .LT. 0) WORK(I,CAT) = AVE
            IF (STAT(I,CAT) .GE. 1) THEN
              N = N + 1
              DMIN = AMIN1(DMIN,WORK(I,CAT))
              DMAX = AMAX1(DMAX,WORK(I,CAT))
              DAVE = DAVE + WORK(I,CAT)
            ENDIF
          ENDDO
          IF (N .GT. 0) DAVE = DAVE/N
          IF (PRUNIT .NE. 0) WRITE(PRUNIT,'(A,1X,I2,1X,3F10.3,A,4I3)') 
     &      ' HV categ, Min,Ave,Max Delta_V = ',CAT,DMIN,DAVE,DMAX,
     &      ' TOT, OFF, DYN, NODYN = ',
     &       NCAT(CAT),OFF(CAT),DYN(CAT),NODYN(CAT)
        ELSE
          GOOD = .FALSE.
        ENDIF
      ENDDO
C
C ****  Shuffle this back..
C
      CALL VZERO(NCAT,6)
      DO I = 1,NCH/2
        CAT = CATEG(I)
        NCAT(CAT) = NCAT(CAT) + 1
        SUP = DEVSUP(I)
        DELTA_V( SUP ) = WORK( NCAT(CAT) , CAT )
      ENDDO

      N = 0
      DO CAT = 1,6
        IF ( (DYN(CAT) .EQ. 0) .AND. (NODYN(CAT) .GT. 0) ) N = N + 1
      ENDDO
      IF (N .EQ. 6) THEN
        CALL VTX_LUMDTM(LUM)
      ELSE
        LVDTM = GZVDTM(0,0)
        IF (IC(LVDTM-5) .NE. 0) CALL VTX_LUMDTM(0.)
      ENDIF
  999 RETURN
      END
