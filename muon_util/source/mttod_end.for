      SUBROUTINE MTTOD_END
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : looks at time-->distance histograms
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created   5-FEB-1992   David Hedin
C-   DH 3/92 update MDFT banks
C    DH 3/92 use STANDARD SUMMARY OUTPUT; kill HFITGA prints
C    DH 5/92 update headers
C    RM 5/94 gets IFAST from existing banks
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      REAL TRES, VEL, CTE, D_TZERO, HSUM, CONTENT,OFF(20),OERR(20)
      REAL MEAN1,MEAN2, SIGMA1,SIGMA2, AV,SD,D,CHI2,SIG(3),HSTATI
      INTEGER MODNUM, MUNMOD3, NMODU, ID, I, DUM, MODULE,J,LMDFT,
     A  GZMDFT,CALIB_T,IER,UNIT,SSUNIT
      REAL F1,F2,F3,F4,T2D(2:3,16)
      INTEGER RUN,ITIME,JTIME,JDATE,IFAST
      CHARACTER*26 TIME
      LOGICAL OK
C==========================================================================
      UNIT=SSUNIT()
      CALL HOUTPU(10)
C  get time/date and convert to string and 64 bit VAX standard
      CALL MUJUNK(1,RUN,ITIME,JTIME,JDATE,F1,F2,F3,F4)
      CALL OFTSTR(ITIME,TIME)
      WRITE(UNIT,*) '****                                 ******'
      WRITE(UNIT,*) '           MTTOD_END RESULTS             '
      WRITE(UNIT,*) '****                                 ******'
      WRITE(UNIT,*) ' RUN ',RUN, ' TIME ',TIME
      WRITE(UNIT,*) '    OFFSETS AS A FUNCTION OF DRIFT DISTANCE '
      DO I=1,20
        ID=30000+I
        CONTENT = HSUM(ID)          ! see how many entries
        OFF(I)=999.
        OERR(I)=999.
        IF (CONTENT.GT.0.) THEN
          CALL HFITGA(ID,D,AV,SD,CHI2,112,SIG)
          OFF(I) = AV                   ! Deviation's mean value (cm)
          OERR(I)=SIG(2)
        ENDIF
        WRITE(UNIT,533) I,OFF(I),OERR(I),HSTATI(ID,1,'HIST',1)
      ENDDO
  533 FORMAT('  XBIN (2.5 mm) (fit,ave)',I5,F10.3,'  +-  ',2F10.3)
      WRITE(UNIT,*) ' **                                       **'
      WRITE(UNIT,*) ' Residuals close and far from the wire '
      WRITE(UNIT,*) 'MODULE        (AVE-)ERROR         (AVE+)ERROR   '
C
C---- Get the IFAST value from the MDFT banks
C
      CALL GTMDFT(10,IFAST,T2D)
      NMODU = MUNMOD3(0,DUM)         ! number of modules
      DO  I = 1,NMODU               ! loop over modules
        MODNUM = MUNMOD3(1,I)
CCC   TEMP
        LMDFT=GZMDFT(MODNUM)
        IF(LMDFT.NE.0) THEN
          IC(LMDFT+3)=IFAST
          IC(LMDFT+1)=101
          IC(LMDFT+7)=JDATE        ! time/date
          IC(LMDFT+8)=JTIME        ! time/date
        ENDIF
        ID = 30000 + MODNUM*10
        CONTENT = HSUM(ID)          ! see how many entries
        IF (CONTENT.GT.50.) THEN
          CALL HFITGA(ID,D,AV,SD,CHI2,112,SIG)
          MEAN1 = AV                   ! Deviation's mean value (cm)
          SIGMA1 = SIG(2)                  ! mean value error (cm)
        ELSE
          WRITE(UNIT,*) MODNUM, '    NO ENTRIES FOR THIS MODULE(-)'
          GO TO 10
        ENDIF
        ID = 30001 + MODNUM*10
        CONTENT = HSUM(ID)          ! see how many entries
        IF (CONTENT.GT.50.) THEN
          CALL HFITGA(ID,D,AV,SD,CHI2,112,SIG)
          MEAN2 = AV                   ! Deviation's mean value (cm)
          SIGMA2 = SIG(2)              ! mean value error (cm)
          WRITE(UNIT,800)  MODNUM, MEAN1,SIGMA1, MEAN2,SIGMA2
        ELSE
          WRITE(UNIT,*) MODNUM, '   NO ENTRIES FOR THIS MODULE(+)'
        ENDIF
   10   CONTINUE
      ENDDO
C
  800 FORMAT(2X, I8, 4X,F10.5,1X, F10.5, 3X, F10.5,1X,F10.5)
C
C    update MDFT bank for 'good' modules (which isn't as yet defined)
C    do only if CALIB_T = 1,2
C
      CALL EZGET('CALIB_T',CALIB_T,IER)
      IF(CALIB_T.EQ.1.OR.CALIB_T.EQ.2) THEN
        NMODU = MUNMOD3(0,DUM)         ! number of modules
        DO  I = 1,NMODU               ! loop over modules
          MODNUM = MUNMOD3(1,I)
          ID = 30000 + MODNUM*10
          CONTENT = HSUM(ID)          ! see how many entries
          IF (CONTENT.GT.50.) THEN
            CALL HFITGA(ID,D,AV,SD,CHI2,112,SIG)
            MEAN1 = AV                   ! Deviation's mean value (cm)
            SIGMA1 = SIG(2)                  ! mean value error (cm)
          ELSE
            GO TO 20
          ENDIF
          ID = 30001 + MODNUM*10
          CONTENT = HSUM(ID)          ! see how many entries
c          IF (CONTENT.GT.50.) THEN
          IF (CONTENT.GT.-1.) THEN
            CALL HFITGA(ID,D,AV,SD,CHI2,112,SIG)
            MEAN2 = AV                   ! Deviation's mean value (cm)
            SIGMA2 = SIG(2)              ! mean value error (cm)
CCCC   would want to possibly cut here
            LMDFT=GZMDFT(MODNUM)
            IF(LMDFT.NE.0) THEN
CC   DON'T CORRECT FIRST 2 BINS
              IC(LMDFT+1)=101
              IC(LMDFT+2)=SD*10000.    ! quality
              IC(LMDFT+3)=2
              IC(LMDFT+4)=RUN            ! new run
              IC(LMDFT+5)=9999999        ! new run
              IC(LMDFT+6)=RUN            ! new run
              IC(LMDFT+7)=JDATE        ! time/date
              IC(LMDFT+8)=JTIME        ! time/date
              DO J=3,20
                C(LMDFT+10+J)=C(LMDFT+10+J)+OFF(J)
c                C(LMDFT+10+J)=OFF(J)
              ENDDO
              C(LMDFT+10+21)=C(LMDFT+31)+OFF(20)
c              C(LMDFT+10+21)=OFF(20)
              WRITE(UNIT,*) MODNUM,' bank MDFT update in MTTOD_END '
            ENDIF
          ENDIF
   20     CONTINUE
        ENDDO
      ENDIF
      CALL HOUTPU(UNIT)
  999 RETURN
      END
