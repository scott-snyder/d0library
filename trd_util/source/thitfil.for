      SUBROUTINE THITFIL
C----------------------------------------------------------------------
C-
C-   PURPOSE AND METHODS : FILL BANK THIT WITH THE TRD TRACK NUMBER
C-                         (TO BE CALLED ONCE THE TRD HITS HAVE BEEN ASSOCIATED
C-                         TO TRACKS)
C-   INPUTS  :
C-   OUTPUTS :
C-   CONTROLS:
C-
C-   CREATED  29-JUL-1991   A. ZYLBERSTEJN
C-   UPDATED  17-SEP-1992   A. ZYLBERSTEJN
C-   UPDATED  22-JAN-1993   ALAIN PLUQUET  NEW VERSION OF TPRL
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:FIRSHT.INC'
      INCLUDE 'D0$INC:TCNTRL.INC'
      INCLUDE 'D0$INC:TRDBGU.INC'
      INCLUDE 'D0$INC:WORKSP.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER NTRAKM
      PARAMETER( NTRAKM =1000  )
      INTEGER WIRE_TRACK(NTRAKM),TRACK_NB(NTRAKM)
      EQUIVALENCE (WIRE_TRACK(1),IWS(1)),(TRACK_NB(1),IWS(1001))
      INTEGER LOUT,IER,LZTRDT,GZTRDT,LZTRDH,GZTRDH,LZTPRL,NA,NC,NCL
      INTEGER IW,ID,IERR,JI,JJ,IUCOMP,nztrdt
      INTEGER LSHF,LZTHIT,GZTHIT,NTOT,NTHIT,NBHIT
      INTEGER I,ICH,ITR,EVTI,TRUNIT
      INTEGER IR,II,JBYT
      LOGICAL DOPRINT,FIRST,TRD_DO_PRINT
      REAL VERSION
      INTEGER nword
      PARAMETER (NWORD=300)
      INTEGER INTEGER_WORD(NWORD)
      REAL REAL_WORD(NWORD)
      DATA FIRST/.TRUE./
      IF(FIRST)THEN
        FIRST=.FALSE.
        LOUT=TRUNIT()
        CALL EZPICK('TRD_RCP')
        CALL EZGET('HSTBOK',IWS,IERR)
        EVTI=0
        ITR=0
        CALL EZRSET
      END IF
      DOPRINT=TRD_DO_PRINT()
      LZTRDH=GZTRDH()
      IF(LZTRDH.LE.0)        GO TO 999
C****
      LZTHIT=GZTHIT()
      IF(LZTHIT.LE.0)THEN
        IF(DOPRINT )WRITE(LOUT,*)' NO THIT BANK'
        GO TO 999
      END IF
      LZTRDT=GZTRDT()
      IF(LZTRDT.LE.0)THEN
        WRITE(LOUT,*)' NO TRDT BANK'
        IF(DOPRINT .AND. SWTDBG.GT.2)WRITE(LOUT,*)' NO TRDT BANK'
        GO TO 999
      END IF
      NTHIT=0
      nztrdt=0
   10 IF(LZTRDT.LE.0)GO TO 71 ! LOOP ON TRACKS
      nztrdt=nztrdt+1
      DO 54 ICH=1,3
        LZTPRL=LQ(LZTRDT-ICH)
        IF(LZTPRL.LE.0)THEN
          IF(DOPRINT .AND. SWTDBG.GT.2)WRITE(LOUT,*)' NO TPRL BANK'
          WRITE(LOUT,*)' NO TPRL BANK'
          GO TO 54
        END IF
        JI=ICH*1000
        if(doprint)write(lout,*)
     +   ' in thitfil layer',ich,'version',q(lztprl+1),
     &    ' nztrdt',nztrdt,' lztprl',lztprl
        CALL UNPACK_TPRL(LZTPRL,VERSION,REAL_WORD,INTEGER_WORD,IER)
        NA=INTEGER_WORD(4)
        NC=INTEGER_WORD(5)
        if(na.le.0)go to 54
        DO I=1,NA ! LOOP ON HIT ANODES
          NTHIT=NTHIT+1
          IF(NTHIT.GE.NTRAKM)THEN
            CALL ERRMSG('TOO MANY TRD TRACKS ','THITFIL',' ','W')
            GO TO 999
          END IF
          JJ=INT(INTEGER_WORD(32+I))+JI
          WIRE_TRACK(NTHIT)=JJ
          TRACK_NB(NTHIT)=IQ(LZTRDT-5)
          if(doprint)write(lout,*)' wire_track anode',jj
        END DO
        if(nc.le.0)go to 54
        DO I=1,NC ! LOOP ON HIT CATHODES
          NTHIT=NTHIT+1
          IF(NTHIT.GE.NTRAKM)THEN
            CALL ERRMSG('TOO MANY TRD TRACKS ','THITFIL',' ','W')
            GO TO 999
          END IF
          JJ=INT(INTEGER_WORD(32+NA+I))+JI
          WIRE_TRACK(NTHIT)=JJ+10000
          TRACK_NB(NTHIT)=IQ(LZTRDT-5)
          if(doprint)write(lout,*)' wire_track cathode',jj
        END DO
        if(doprint)
     +   write(lout,*)' in thitfil avant  54,version',q(lztprl+1),
     &    ' nztrdt',nztrdt
   54 CONTINUE
      LZTRDT=LQ(LZTRDT)
      IF(LZTRDT.NE.0)then
        do ich=1,3
        LZTPRL=LQ(LZTRDT-ICH)
        if(lztprl.le.0)go to 10
        end do
        GO TO 10
      end if
   71 CONTINUE
      IF(DOPRINT .AND. SWTDBG.GT.2)THEN
        WRITE(LOUT,*)' IN THITFIL, INFORMATIONS ON TRACKS '
        WRITE(LOUT,'(10I8)')(WIRE_TRACK(I),TRACK_NB(I),I=1,NTHIT)
      END IF
      NTOT=2
      NBHIT=IQ(LZTHIT+2)
      DO I=1,NBHIT         ! LOOP ON THE HITS
        LSHF=LZTHIT+NTOT
        II=IQ(LSHF+1)
        IF(DOPRINT .AND. SWTDBG.GT.2)
     +    WRITE(LOUT,1048)JBYT(II,2,8)+1, JBYT(II,10,2)+1,JBYT(II,1,1),
     &    JBYT(II,12,3), FLOAT(IQ(LSHF+2))*.1
 1048   FORMAT(' HIT: WIRE',I4,' LAYER',I2, ' TYPE',I2,
     &    ' NB. OF CLUSTERS',I2,' TOTAL ENERGY', F8.1)

C       IWS(I)= WIRE         +LAYER*1000            +TYPE*10000
        NCL=JBYT(II,12,3)
        II=JBYT(II,2,8)+1+(JBYT(II,10,2)+1)*1000+JBYT(II,1,1)*10000
        ID=IUCOMP(II,WIRE_TRACK,NTHIT)
        IF(DOPRINT .AND. SWTDBG.GT.2
     &    )WRITE(LOUT,*)' II',II,(WIRE_TRACK(JJ),JJ=1,NTHIT), ' ID',ID
        IF(ID.NE.0)THEN
          CALL SBYT(TRACK_NB(ID),IQ(LSHF+1),25,8)
        END IF
        NTOT=NTOT+2
        IF(NCL.NE.0)THEN
C          NCL=(NCL-1)/4+1
C          NTOT=NTOT+MIN0(NCL,2)*2
          NTOT=NTOT+2
        END IF
      END DO
      IF(DOPRINT )        CALL PRTHIT(LOUT,0)
C----------------------------------------------------------------------
  999 RETURN
      END
