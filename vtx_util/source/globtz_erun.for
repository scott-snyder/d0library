      LOGICAL FUNCTION GLOBTZ_ERUN()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : fits gaussian and gets GLOBAL TZERO for each 
C-                         CRATE
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created   28-JAN-1993   V. D. Elvira 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER NX,CR,I,J,L,START,IE,IER
      INTEGER LUN,IUSER,IERR,RUNNUM,RUNNO,NOENT(10),NENT
      INTEGER LENGTH
C----------------------------------------------------------------------
      REAL CC,AV,SD,CHI2,SIG(3),IMI,IMA
      REAL FRAC,NEWTZ(10)
      REAL BS,IMIP,HI,TOP,BOT,X,YMI,YMA
C----------------------------------------------------------------------
      LOGICAL HEXIST,OK
C----------------------------------------------------------------------
      CHARACTER*5 CHANNEL
      CHARACTER*40 MSG,CHT
      CHARACTER*60 TZNAME
C----------------------------------------------------------------------
      CALL EZPICK('GLOBTZ_RCP')
      CALL EZGET_i('NX',NX,IER)
      CALL EZGET('IMI',IMI,IER)
      CALL EZGET('IMA',IMA,IER)
      CALL EZGET('FRAC',FRAC,IER)
      CALL EZGETS('TZNAME',1,TZNAME,LENGTH,IER)
      CALL EZRSET
C----------------------------------------------------------------------
      GLOBTZ_ERUN = .TRUE.
C----------------------------------------------------------------------
C- For each CRATE, finds the leading edge of the drift time distrib.
C- and calculates the upper time limit of the fitted function from a
C- weighted average of the absiza.
C----------------------------------------------------------------------
      NENT=0
      CALL DHDIR(' ','//PAWC/VTXTZ',IE,' ')
      IF (IE.NE.0) THEN
        CALL ERRMSG('VTRAKS','VTX_EXM_TZ',
     &    ' ERROR SETTING HBOOK DIRECTORY://PAWC/VTXTZ','W')
      ENDIF
C----------------------------------------------------------------------
      DO CR=1,10
        CALL HNOENT(100+10*(CR-1),NOENT(CR))
        NENT=NENT+NOENT(CR)
        BS=(IMA-IMI)/FLOAT(NX)
        IMIP=IMI-0.5*BS
        DO L = 1,NX
          IF (HI(100+10*(CR-1),L) .NE. 0.) THEN
            START = L
            GO TO 5
          ENDIF
        ENDDO
    5   TOP = 0.
        BOT = 0.
        DO L = START,NX
          X = HI(100+10*(CR-1),L)
          TOP = TOP + (IMIP + FLOAT(L)*BS)*X
          BOT = BOT + X
        ENDDO
C----------------------------------------------------------------------
C- If there are enough entries in the crate, the histo is blowed up
C- and copied into memory . A gaussian is fitted to them.
C----------------------------------------------------------------------
        WRITE(CHT,100) CR
        CHT='Drift time dist.'//CHT
        IF (BOT .GE. 10.) THEN
          AV = TOP/BOT
          CALL VHBLOW(100+10*(CR-1),200+10*(CR-1),CHT,.FALSE.,IMI,AV,
     &      YMI,YMA,OK)
          CALL HFITGA(200+10*(CR-1),CC,AV,SD,CHI2,102,SIG)
          NEWTZ(CR)=AV-SD*SQRT(-2*LOG(FRAC))
        ELSE
          AV=0.
          SD=0.
          WRITE(CHANNEL,100) CR
          MSG='not enough entries histo='//CHANNEL
          CALL ERRMSG('ERROR','GLOBTZ_ERUN',MSG,'S')
        ENDIF
        IF (HEXIST(100+10*(CR-1))) CALL HDELET(100+10*(CR-1))
        IF (HEXIST(200+10*(CR-1))) CALL HDELET(200+10*(CR-1))
      ENDDO
C------------------------------------------------------------------------
C ****  Write results to output file(s)
C------------------------------------------------------------------------
      NENT=NENT/10
      RUNNUM=RUNNO()
      IF (NENT.GT.7500) THEN
        CALL GTUNIT(6453,LUN,IERR)
        OPEN(UNIT=LUN,FILE=TZNAME,STATUS='UNKNOWN',ACCESS='APPEND',FORM=
     &    'FORMATTED')
        WRITE(LUN,'(1X,I6,10(1X,F6.2))') RUNNUM, (NEWTZ(J), J=1,10)
        CLOSE(LUN)
        CALL RLUNIT(6453,LUN,IERR)
      ENDIF
C------------------------------------------------------------------------
  100 FORMAT(I5)
C------------------------------------------------------------------------
  999 RETURN
      END
