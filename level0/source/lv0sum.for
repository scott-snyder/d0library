      FUNCTION LV0SUM()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Evaluating Mean and Sigma for LEVEL0 Pedestals
C-
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created  26-JUN-1992   Q. Rushdy Ahmad
C-   Updated  28-JAN-1994   Jeffrey Bantly  adjust to D0 standards 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL LV0SUM
C
      INTEGER CHANNEL, CATEGORY
      INTEGER TIMEBIN,CHARGEBIN
      INTEGER BINVALUE,BINNUM
      PARAMETER( CATEGORY = 8)
      PARAMETER( CHANNEL = 72 )
      PARAMETER( TIMEBIN = 200 )
      PARAMETER( CHARGEBIN = 100 )
      INTEGER ICHAN,ID,NID(100)
      INTEGER BONES,TIMEID,CHARGEID
      INTEGER I,J
      INTEGER IER,LUNIT1,LUNIT2,LUNIT3,ERR
      INTEGER MINBIN,MAXBIN
      INTEGER RUNNUM,EVTNUM
C
      REAL LV0_HIST2(4,10)
      REAL MARK1(TIMEBIN),MARK2(CHARGEBIN)
      REAL MAXVAL,SUMBIN,SUM,SUMT,SUMQ,MEANT,RMST,MEANQ,RMSQ
C
      LOGICAL EZERROR
      LOGICAL PRODUC, PRODFL
C
      CHARACTER*26 NAME(CATEGORY)
C
      DATA NAME/'Pedestal Mean Time','Pedestal Mean Charge',
     &   'Pedestal RMS Time','Pedestal RMS Charge',
     &   'Pedestal Mean Time(Off)','Pedestal Mean Charge(Off)',
     &   'Pedestal RMS Time(Off)','Pedestal RMS Charge(Off)'/
C
C---------------------------------------------------------------------
C
      LV0SUM=.FALSE.
      PRODFL = PRODUC()
      IF ( PRODFL ) GOTO 990
      CALL DHDIR('LEVEL0_RCP','HBOOK_DIRECTORY',ERR,' ')
      IF(ERR.NE.0) THEN
        CALL ERRMSG('LEVEL0-bad-hbk-dir','LV0HIS',
     &          ' ERROR SETTING HBK DIR','W')
        GOTO 999
      ENDIF
C
      CALL EZPICK('LEVEL0_RCP')
      IF ( EZERROR(ERR) ) THEN
        CALL ERRMSG('LEVEL0-no-rcp','LV0HIS',
     &                      'LEVEL0_RCP not found.','W')
        GOTO 999
      ELSE
        CALL EZGET('LV0_HIST2(1)',LV0_HIST2(1,1),ERR)
        CALL EZRSET
      ENDIF
C
      CALL EVNTID(RUNNUM,EVTNUM)
      CALL GTUNIT(702,LUNIT1,IER)
      CALL GTUNIT(703,LUNIT2,IER)
      CALL GTUNIT(704,LUNIT3,IER)
C
C   Book Histograms
C
      DO ID=1,CATEGORY
        J=90+ID
        CALL HBOOK1(J,NAME(ID),72,0.5,72.5,0.)
      ENDDO
C
C  reading the histograms and dumping the contents into an array
C
      DO BONES=1,2   ! 1:correct bunch, 2:off bunch
C
        IF (BONES.EQ.1) THEN
          IF ((LV0_HIST2(1,3).EQ.1).AND.(LV0_HIST2(1,4).EQ.1)) THEN
            TIMEID=3
            CHARGEID=4
            WRITE(LUNIT1,*)' RUN',RUNNUM
            WRITE(LUNIT1,*)
     &        ' CHANNEL NO.     MEAN(T)        RMS(T)     ',
     &        '    MEAN(Q)         RMS(Q)'
          ELSE
            TIMEID=0
            CHARGEID=0
          ENDIF
        ELSE
          IF ((LV0_HIST2(1,7).EQ.1).AND.(LV0_HIST2(1,8).EQ.1)) THEN
            LUNIT1=LUNIT1+1
            TIMEID=7
            CHARGEID=8
            WRITE(LUNIT1,*)' RUN',RUNNUM
            WRITE(LUNIT1,*)
     &        ' CHAN. NO.(OFF)  MEAN(T)        RMS(T)     ',
     &        '    MEAN(Q)         RMS(Q)'
          ELSE
            TIMEID=0
            CHARGEID=0
          ENDIF
        ENDIF
C
        IF ((TIMEID.NE.0).AND.(CHARGEID.NE.0)) THEN
          DO ICHAN=1,CHANNEL !READING TIME PEDESTAL HISTOGRAMS
            ID=100*TIMEID+ICHAN
            CALL HUNPAK(ID,MARK1,' ',0)
C
C  find bin with max value, for time pedestals
C
            MAXVAL=MARK1(TIMEBIN)
            BINVALUE=TIMEBIN
            DO J=TIMEBIN-1,1,-1
              IF (MARK1(J).GT.MAXVAL)THEN
                MAXVAL=MARK1(J)
                BINVALUE=J
              ENDIF
            ENDDO
C
C   now evaluate the mean and rms for time pedestals
C
            SUM=0.0
            SUMT=0.0
            SUMBIN=0
            MEANT=0.0
            RMST=0.0
            IF ( (BINVALUE.LE.200).AND.(BINVALUE.GT.0) ) THEN
              MINBIN=BINVALUE-10
              MAXBIN=BINVALUE+10
              IF ( MINBIN.LE.0 ) MINBIN=1
              IF ( MAXBIN.GT.200 ) MAXBIN=200
              DO I=MINBIN,MAXBIN
                BINNUM=840+I
                SUM=SUM+(FLOAT(BINNUM)*MARK1(I))
                SUMT=SUMT+((FLOAT(BINNUM)**2)*MARK1(I))
                SUMBIN=SUMBIN+MARK1(I)
              ENDDO
              IF (SUMBIN.GT.0) THEN
                MEANT=SUM/SUMBIN
                IF ((SUMT/SUMBIN-(MEANT**2.)).GT.0.0) THEN
                  RMST=SQRT(SUMT/SUMBIN-(MEANT**2.))
                ELSE
                  IF ((SUMT/SUMBIN-(MEANT**2.)).EQ.0.0) THEN
                    IF (BONES.EQ.1) THEN
                      WRITE(*,*) 'Time Pedestal(CBUNCH)',ICHAN,
     &                  '  needs adjustment.'
                      WRITE(LUNIT2,*) 'Time Pedestal(CBUNCH)',
     &                  ICHAN,'  needs adjustment.'
                    ELSE
                      WRITE(*,*) 'Time Pedestal(OBUNCH)',ICHAN,
     &                  '  needs adjustment.'
                      WRITE(LUNIT2,*) 'Time Pedestal(OBUNCH)',
     &                  ICHAN,'  needs adjustment.'
                    ENDIF
                  ELSE
                    WRITE(*,*) 'NEGATIVE ROOT ERROR(TIME),CHAN =',ICHAN
                    WRITE(LUNIT3,*) 'CHAN.NO.(TIME)',ICHAN,' ,NEG. ROOT'
                  ENDIF
                ENDIF
              ENDIF
            ENDIF
C
C  NOW DO THE SAME FOR THE CHARGE PEDESTALS
C
            ID=100*CHARGEID+ICHAN
            CALL HUNPAK(ID,MARK2,' ',0)
C
C  find bin with max value, for charge pedestals
C
            MAXVAL=MARK2(CHARGEBIN)
            BINVALUE=CHARGEBIN
            DO J=CHARGEBIN-1,1,-1
              IF (MARK2(J).GT.MAXVAL)THEN
                MAXVAL=MARK2(J)
                BINVALUE=J
              ENDIF
            ENDDO
C
C   now evaluate the mean and rms for charge pedestals
C
            SUM=0.0
            SUMQ=0.0
            SUMBIN=0
            MEANQ=0.0
            RMSQ=0.0
            IF ( (BINVALUE.LE.70).AND.(BINVALUE.GT.0) ) THEN
              MINBIN=BINVALUE-10
              MAXBIN=BINVALUE+10
              IF ( MINBIN.LE.0 ) MINBIN=1
              IF ( MAXBIN.GT.100 ) MAXBIN=100
              DO I=MINBIN,MAXBIN
                BINNUM=I
                SUM=SUM+(FLOAT(BINNUM)*MARK2(I))
                SUMQ=SUMQ+((FLOAT(BINNUM)**2)*MARK2(I))
                SUMBIN=SUMBIN+MARK2(I)
              ENDDO
              IF (SUMBIN.GT.0.0) THEN
                MEANQ=SUM/SUMBIN
                IF ((SUMQ/SUMBIN-(MEANQ**2.)).GT.0.0 ) THEN
                  RMSQ=SQRT(SUMQ/SUMBIN-(MEANQ**2.))
                ELSE
                  IF ((SUMQ/SUMBIN-(MEANQ**2.)).EQ.0.0) THEN
                    IF (BONES.EQ.1) THEN
                      WRITE(*,*) 'Charge Pedestal(CBUNCH)',ICHAN,
     &                  '  needs adjustment.'
                      WRITE(LUNIT2,*)'Charge Pedestal(CBUNCH)',ICHAN,
     &                  '  needs adjustment.'
                    ELSE
                      WRITE(*,*) 'Charge Pedestal(OBUNCH)',ICHAN,
     &                  '  needs adjustment.'
                      WRITE(LUNIT2,*)'Charge Pedestal(OBUNCH)',ICHAN,
     &                  '  needs adjustment.'
                    ENDIF
                  ELSE
                    WRITE(*,*) 'NEGATIVE ROOT ERROR(CHARGE),CHAN =',
     &                ICHAN
                    WRITE(LUNIT3,*)'CHAN.NO.(CHARGE)',ICHAN,
     &                ' ,NEG. ROOT'
                  ENDIF
                ENDIF
              ENDIF
            ENDIF
            IF (BONES.EQ.1) THEN
              CALL HFF1(91,NID(91),FLOAT(ICHAN),MEANT)
              CALL HFF1(92,NID(92),FLOAT(ICHAN),MEANQ)
              CALL HFF1(93,NID(93),FLOAT(ICHAN),RMST)
              CALL HFF1(94,NID(94),FLOAT(ICHAN),RMSQ)
              WRITE(LUNIT1,*) FLOAT(ICHAN),MEANT,RMST,MEANQ,RMSQ
            ELSE
              CALL HFF1(95,NID(95),FLOAT(ICHAN),MEANT)
              CALL HFF1(96,NID(96),FLOAT(ICHAN),MEANQ)
              CALL HFF1(97,NID(97),FLOAT(ICHAN),RMST)
              CALL HFF1(98,NID(98),FLOAT(ICHAN),RMSQ)
              WRITE(LUNIT1,*) FLOAT(ICHAN),MEANT,RMST,MEANQ,RMSQ
            ENDIF
          ENDDO
        ENDIF
      ENDDO
C
  990 CONTINUE
      LV0SUM=.TRUE.
C------------------------------------------------------------------------
  999 RETURN
      END
