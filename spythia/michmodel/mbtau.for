      SUBROUTINE MBTAU(IOPT,GBOT,GTAU)

      IMPLICIT NONE
      INCLUDE 'D0$SPYTHIA$INC:VARS.INC'
      INTEGER NUMEQS,IOPT
      REAL*8 VARS1(3),WORK(36),SCALE0,SCALE1,EPS,H0,MBMZ,MTAUMZ
      REAL*8 GBOT,GTAU
      EXTERNAL DIFFEQS_MB,DIFFEQS_MTAU


      IF (IOPT.EQ.2) GOTO 100
      IF (IOPT.NE.1) THEN
        WRITE(*,*) 'Bad option to MBTAU!'
        STOP
      ENDIF

C       Find mb(Mz)
C
      VARS1(1)=ALPHAW         !first get alpha and alpha3 at Mb
      VARS1(2)=ALPHA(3)
      VARS1(3)=3.0
      SCALE0=LOG(MZ)
      SCALE1=LOG(MQ(6))
      EPS=0.001
      H0=1.0
      NUMEQS=3
C       print *, 'Point 1'
      CALL DDEQMR(NUMEQS,SCALE0,SCALE1,VARS1,H0,EPS,DIFFEQS_MB,WORK)
C       write(*,*) 'Alpha3(mb)=',vars1(2)
C       write(*,*) 'Alpha(mb)=',vars1(1)

      SCALE0=LOG(MQ(6))               !now run Mb from mb to mz
      SCALE1=LOG(MZ)
      VARS1(3)=MQ(6)/(1.+4.*VARS1(2)/(3.*PI)+12.4*(VARS1(2)/PI)**2)
C       print *, 'Point 2'
      CALL DDEQMR(NUMEQS,SCALE0,SCALE1,VARS1,H0,EPS,DIFFEQS_MB,WORK)
C..JTL..        write(*,*) 'Alpha3(mz)=',vars1(2)
C       write(*,*) 'Alpha(mz)=',vars1(1)
C..JTL..        write(*,*) 'Mb(mz)=',vars1(3)

      MBMZ=VARS1(3)
      GBOT=SQRT(2.)*MBMZ/(VEV*COS(ATAN(TBETA)))
C       write(*,*) 'In MBTAU: gbot=',gbot
C        write(*,*) 'Also: tbeta,vev,mbmz=',tbeta,vev,mbmz

C       Find mtau(Mz)
C
      VARS1(1)=ALPHAW             !first find alpha and alpha3 at Mtau
      VARS1(2)=ALPHA(3)
      VARS1(3)=0.
      SCALE0=LOG(MZ)
      SCALE1=LOG(ML(6))
      EPS=0.001
      H0=1.0
      NUMEQS=3
C       print *, 'Point 3'
      CALL DDEQMR(NUMEQS,SCALE0,SCALE1,VARS1,H0,EPS,DIFFEQS_MTAU,WORK)
C       write(*,*) 'Alpha3(mtau)=',vars1(2)
C       write(*,*) 'Alpha(mtau)=',vars1(1)

      SCALE0=LOG(ML(6))                   !now run Mtau from mtau to mz
      SCALE1=LOG(MZ)
      VARS1(3)=ML(6)
C       print *, 'Point 4'
      CALL DDEQMR(NUMEQS,SCALE0,SCALE1,VARS1,H0,EPS,DIFFEQS_MTAU,WORK)
C       write(*,*) 'Alpha3(mz)=',vars1(2)
C       write(*,*) 'Alpha(mz)=',vars1(1)
C       write(*,*) 'Mtau(mz)=',vars1(3)

      MTAUMZ=VARS1(3)
      GTAU=SQRT(2.)*MTAUMZ/(VEV*COS(ATAN(TBETA)))
C       write(*,*) 'In MBTAU: gtau=',gtau

      RETURN

  100 CONTINUE

C       Now run mb back down to mb
C
      VARS1(1)=ALPHAW
      VARS1(2)=ALPHA(3)
      VARS1(3)=GBOT/SQRT(2.)*VEV*COS(ATAN(TBETA))
      EPS=0.001
      H0=1.0
      NUMEQS=3
      SCALE0=LOG(MZ)
      SCALE1=LOG(MQ(6))
C       write(*,*) 'Point 7'
C       write(*,*) (vars1(i),i=1,3)
      CALL DDEQMR(NUMEQS,SCALE0,SCALE1,VARS1,H0,EPS,DIFFEQS_MB,WORK)
C..JTL..        write(*,*) 'Alpha3(mb)=',vars1(2)
C       write(*,*) 'Alpha(mb)=',vars1(1)

      MBOT=VARS1(3)*(1.+4.*VARS1(2)/(3.*PI)+12.4*(VARS1(2)/PI)**2)
C..JTL..        write(*,*) 'mbot=',mbot

      RETURN
      END
