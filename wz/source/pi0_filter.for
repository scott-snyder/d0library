      LOGICAL FUNCTION PI0_FILTER
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : find pi0's by looking for two photon conversions
C-    
C-   Created  15-NOV-1992   Ulrich Heintz
C-   Updated   7-MAR-1994   Ulrich Heintz  modified for streaming events with 
C-                                              at least one pi0 candidate 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:PI.DEF'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKC.INC'
      INTEGER LFDCT,LDTRK,GZPELC,NVER,NTRAKS,ZLINK_TRAKS(450),IER,I,J
      INTEGER N_2MIP_LOOSE,N_2MIP_TIGHT,N_1MIP,N_1MIP_MAX
      PARAMETER( N_1MIP_MAX = 100 )
      REAL    LO_2MIP_LOOSE,HI_2MIP_LOOSE,LO_2MIP_TIGHT,HI_2MIP_TIGHT
      REAL    LO_1MIP,HI_1MIP,R_MIN,R_MAX,XPELC,YPELC,ZPELC
      REAL    PHI(N_1MIP_MAX),X(N_1MIP_MAX),Y(N_1MIP_MAX),ZV(14),DZ(14)
      REAL    PHI_ROAD,THETA_ROAD_FACT,MIN_THETA_ROAD,PHI_CENTER
      REAL    PHI_LO,PHI_HI,THETA_LO,THETA_HI,DELZ,RHO,DEDX
      REAL    DELTA(2),T1(2),T2(2),A,XX,YY,D,PHIPT1,PHIPT2,PHIPT,DPHI,R
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C **** for testing only
C      INTEGER runno,evonum
C      CHARACTER*80 msg
C **** for testing only
C----------------------------------------------------------------------
C
C **** initializations
C
      PI0_FILTER=.FALSE.
      IF(FIRST)THEN
        FIRST=.FALSE.
        CALL INRCP('ZTRAKS_RCP',IER)
        IF(IER.NE.0) CALL ERRMSG('no ZTRAKS_RCP','PI0_FILTER',' ','F')
        CALL INRCP('CAPHEL_RCP',IER)
        IF(IER.NE.0) CALL ERRMSG('no CAPHEL_RCP','PI0_FILTER',' ','F')
        CALL INRCP('PI0_FILTER_RCP',IER)
        IF(IER.NE.0) CALL ERRMSG('no PI0_FILTER_RCP','PI0_FILTER',' ',
     &    'F')
        CALL EZPICK('PI0_FILTER_RCP')
        CALL EZGET('LO_2MIP_LOOSE',LO_2MIP_LOOSE,IER)
        IF(IER.EQ.0)CALL EZGET('HI_2MIP_LOOSE',HI_2MIP_LOOSE,IER)
        IF(IER.EQ.0)CALL EZGET('LO_2MIP_TIGHT',LO_2MIP_TIGHT,IER)
        IF(IER.EQ.0)CALL EZGET('HI_2MIP_TIGHT',HI_2MIP_TIGHT,IER)
        IF(IER.EQ.0)CALL EZGET('LO_1MIP',LO_1MIP,IER)      
        IF(IER.EQ.0)CALL EZGET('HI_1MIP',HI_1MIP,IER)      
        IF(IER.EQ.0)CALL EZGET('MINIMUM_R_1MIP',R_MIN,IER)      
        IF(IER.EQ.0)CALL EZGET('MAXIMUM_R_1MIP',R_MAX,IER)      
        IF(IER.NE.0)CALL ERRMSG('EZGET error from PI0_FILTER_RCP',
     &    'PI0_FILTER',' ','F')
        CALL EZRSET
        CALL EZPICK('CAPHEL_RCP')
        CALL EZGET('PHI_ROAD',PHI_ROAD,IER)
        IF(IER.EQ.0)CALL EZGET('THETA_ROAD_FACT',THETA_ROAD_FACT,IER)
        IF(IER.EQ.0)CALL EZGET('MIN_THETA_ROAD',MIN_THETA_ROAD,IER)
        IF(IER.NE.0)CALL ERRMSG('EZGET error from CAPHEL_RCP',
     &    'PI0_FILTER',' ','F')
        CALL EZRSET
      ENDIF
C----------------------------------------------------------------------
C
C **** event vertex
C
      CALL ZVERTE(NVER,ZV,DZ)                ! Vertex from tracking
C----------------------------------------------------------------------
C
C **** loop over PLECs until a candidate was found
C
      LPELC=GZPELC()
      DO WHILE (LPELC.GT.0.AND..NOT.PI0_FILTER)
        IF(Q(LPELC+21).GE.2)THEN      ! if at least 2 tracks in road
          XPELC=Q(LPELC+23)           ! x
          YPELC=Q(LPELC+24)           ! y
          ZPELC=Q(LPELC+25)           ! z
C
C **** look for tracks in road (code adapted from CLEANEM)
C
          LHMTE=LQ(LPELC-1)
          IF(LHMTE.GT.0)THEN
            PHI_LO = Q(LHMTE+9)
            PHI_HI = Q(LHMTE+10)
            THETA_LO = Q(LHMTE+11)
            THETA_HI = Q(LHMTE+12)
C
            RHO = SQRT(YPELC**2+XPELC**2)
            DELZ = DZ(1)*THETA_ROAD_FACT
            THETA_LO = ATAN2(RHO,ZPELC-ZV(1)+DELZ)
            THETA_HI = ATAN2(RHO,ZPELC-ZV(1)-DELZ)
            IF(ABS(THETA_HI-THETA_LO).LT.2.*MIN_THETA_ROAD) THEN
              THETA_LO = Q(LPELC+8)-MIN_THETA_ROAD
              THETA_HI = Q(LPELC+8)+MIN_THETA_ROAD
            ENDIF
C
C ****  get the links of all Ztracks in the electron road
C
            CALL ZTRK_IN_ROAD(ZV(1),PHI_LO,PHI_HI,THETA_LO,THETA_HI,
     &        NTRAKS,ZLINK_TRAKS)
C
C **** if at least 2 tracks were found loop over all tracks in road
C
            IF(NTRAKS.GE.2)THEN
              N_2MIP_LOOSE=0 ! number of tracks that pass loose 2-mip dE/dx cut
              N_2MIP_TIGHT=0 ! number of tracks that pass tight 2-mip dE/dx cut
              N_1MIP=0       ! number of tracks that pass 1-mip dE/dx cut
              DO I=1,NTRAKS
                LZTRK=ZLINK_TRAKS(I)
                IF(LZTRK.LE.0)THEN
                  CALL ERRMSG('ZTRK=0','PI0_FILTER',' ','W')
                ELSE
                  LFDCT=LQ(LZTRK-8)
                  LDTRK=LQ(LZTRK-7)
                  IF(LDTRK.NE.0)THEN
                    DEDX=Q(LDTRK+20)
                  ELSEIF(LFDCT.NE.0)THEN
                    DEDX=Q(LFDCT+20)
                  ENDIF
                  IF(DEDX.GT.LO_2MIP_LOOSE.AND.DEDX.LT.HI_2MIP_LOOSE) 
     &              N_2MIP_LOOSE=N_2MIP_LOOSE+1
                  IF(DEDX.GT.LO_2MIP_TIGHT.AND.DEDX.LT.HI_2MIP_TIGHT)
     &              N_2MIP_TIGHT=N_2MIP_TIGHT+1
                  IF(DEDX.GT.LO_1MIP.AND.DEDX.LT.HI_1MIP.AND.N_1MIP.LT.
     &              N_1MIP_MAX)THEN
                    N_1MIP=N_1MIP+1
                    LZTRK=ZLINK_TRAKS(I)
                    LFDCT=LQ(LZTRK-8)
                    LDTRK=LQ(LZTRK-7)
                    IF(LDTRK.NE.0)THEN
                      X(N_1MIP)=Q(LDTRK+7)  ! x
                      Y(N_1MIP)=Q(LDTRK+8)  ! y
                      PHI(N_1MIP)=Q(LDTRK+6)
                    ELSEIF(LFDCT.NE.0)THEN
                      X(N_1MIP)=Q(LFDCT+4)   ! x
                      Y(N_1MIP)=Q(LFDCT+5)   ! y
                      PHI(N_1MIP)=Q(LFDCT+6)
                    ENDIF
                    IF(X(N_1MIP).EQ.0.AND.Y(N_1MIP).EQ.0)N_1MIP=N_1MIP-1
                  ENDIF
                ENDIF
              ENDDO
C
C **** now check whether a candidate was found
C
              IF(N_2MIP_LOOSE.GE.2.AND.N_2MIP_TIGHT.GE.1)THEN
C
                PI0_FILTER=.TRUE.       ! 2-track candidate ---> pass this event
C
              ELSEIF(NTRAKS.GE.3)THEN   ! check for 3 track candidates
C
                IF(N_2MIP_TIGHT.GE.1.AND.N_1MIP.GE.2)THEN
                  DO I=1,N_1MIP-1       ! loop over all 1-mip pairs
                    DO J=I+1,N_1MIP
                      DELTA(1)=X(1)-X(2)  ! track parameters
                      DELTA(2)=Y(1)-Y(2)
                      T1(1)=COS(PHI(1))
                      T1(2)=SIN(PHI(1))
                      T2(1)=COS(PHI(2))
                      T2(2)=SIN(PHI(2))
                      PHIPT1=ATAN2(Y(1),X(1)) ! phi of bisector 
                      PHIPT2=ATAN2(Y(2),X(2))
                      DPHI=PHIPT2-PHIPT1
                      IF(DPHI.GT.PI)DPHI=DPHI-2.*PI
                      IF(DPHI.LT.-PI)DPHI=DPHI+2.*PI
                      PHIPT=PHIPT1+DPHI/2.
                      D=T1(2)*T2(1)-T1(1)*T2(2) ! intersect. in transverse plane
                      IF(D.NE.0)THEN
                        A=(DELTA(1)*T2(2)-DELTA(2)*T2(1))/D
                        XX=X(1)+A*T1(1)
                        YY=Y(1)+A*T1(2)
                        R=XX*COS(PHIPT)+YY*SIN(PHIPT)   ! R
                        IF(R.GT.R_MIN.AND.R.LT.R_MAX)THEN
C
                          PI0_FILTER=.TRUE. ! 3-track candidate ---> pass event
C
                        ENDIF
                      ENDIF
                    ENDDO
                  ENDDO     ! loop over all 1-mip track pairs
                ENDIF       ! failed 3 track criteria
              ENDIF         ! failed 2 track criteria and < 3 tracks found
            ELSE
              CALL ERRMSG('<2 tracks','PI0_FILTER',' ','W')
            ENDIF           ! < 2 tracks in road found
          ENDIF             ! LHMTE=0
        ENDIF               ! < 2 tracks in road in PELC
        LPELC=LQ(LPELC)     ! pointer to next electron
      ENDDO
C----------------------------------------------------------------------
C **** for testing only
C      if(pi0_filter)then
C        i=evonum()
C        j=runno()
C        write (msg,900) j,i
C  900   format(1x,' found pi0 candidate: run/event,'2I8)
C        call intmsg(msg)
C      endif
C **** for testing only
C----------------------------------------------------------------------
  999 RETURN
      END
