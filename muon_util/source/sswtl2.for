      SUBROUTINE SSWTL2(DIR,NTRG)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : SSW filter(L2) main routine
C-   Inputs  : DIR ( 1->North   2->South )
C-
C-   Outputs : NTRG ( # of triggers )
C-   Controls:
C-
C-   Created  30-FEB-1994   Joao de Mello
C-   Modified 11-JUL-1994   Andre Sznajder
C-   Modified 20-FEB-1995   Andre Sznajder ( fill MUOT, MHTT and STTH )
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INTEGER MWH,MXR,MSH
      PARAMETER(MSH=40)   ! max # of Samus hits in a road
      PARAMETER(MWH=24)   ! max # of Wamus hits in a road
      PARAMETER(MXR=24)   ! max # of roads
      INTEGER NSH(MXR),TUBE_ADD(MXR,MSH),GEOM_ADD(MXR,MSH)
      INTEGER DIR,LTRG2,NTRG,NSOL,I,J,K,JHT
      INTEGER NCH,IQUAD,IMUOT,ISTA,ISEC,ITUBE,IMOD
      INTEGER NHIT,ICH(MWH,4),IADD(40)
      INTEGER LMUOT,GZMUOT,LMHTT,LPMHTT,LMUOH,GZMUOH
      REAL DRFT(40),GEOM(6,40),CH(MWH,3)
      REAL RT0(MXR), AT0(MXR), PH0(MXR), BT0(MXR)
      REAL SAH(MXR,3),SBH(MXR,3),MAGH(MXR,3),COSBM(MXR,3)
      REAL COSAM(3),PAV(3),DPMOD,PMOD
C
C*** Initialization
C
      NTRG=0
C
C*** Find roads from Samus A and B triplets
C
      CALL SATGSW(DIR,LTRG2,RT0,AT0,PH0,BT0,SBH,SAH,
     &            MAGH,COSBM,TUBE_ADD,GEOM_ADD,NSH)
C
C*** Uses the roads to find corresponding hits in Wamus C
C
      DO 100 I=1,LTRG2          
        NHIT=NSH(LTRG2)                 ! # Samus hits in this road
        IF (NHIT.GE.MSH) GOTO 100        ! protection for too many hits
        CALL SSWHIT(DIR,RT0(I),AT0(I),PH0(I)
     &              ,NSOL,PAV,CH,ICH,NCH)
        IF (NSOL.GT.0) THEN
          NTRG=NTRG+1
C
C*** Calculates the momentum 
C
          CALL SSWML2(BT0(I),SBH(I,1),SBH(I,2),
     &                SBH(I,3),PAV,PMOD,COSAM)
C
C*** Book and fills MUOT bank at L2 
C
          DPMOD=.4
          IF (DIR.EQ.1) THEN
            IQUAD=15
          ELSE
            IQUAD=16
          ENDIF
          CALL MUOTFL(IMUOT,NCH,NHIT,IQUAD,0,1024,0,0,
     &    SAH(I,1),SAH(I,2),SAH(I,3),MAGH(I,1),
     &    MAGH(I,2),MAGH(I,3),COSBM(I,1),COSBM(I,2),     
     &    COSBM(I,3),COSAM(1),COSAM(2),COSAM(3),
     &    -1,-1,-1,PMOD,DPMOD,-1,-1,-1)
C
C*** Book and fill bank MHTT
C

          LMUOT=GZMUOT(IMUOT)                   ! MUOT pointer for this track 
          CALL BKMHTT(LMUOT,NCH*5,LMHTT)        ! book MHTT
          LMUOH=GZMUOH(0)                       ! MUOH pointer
          DO J=1,NCH
            LPMHTT=LMHTT+5*(J-1)
            IQ(LPMHTT+1)=ICH(J,4)               ! wire address
            IQ(LPMHTT+2)=ICH(J,3)               ! IHIT (pointer into MUOH bank)
            IQ(LPMHTT+3)=0                      ! not using drift times in L2
            IQ(LPMHTT+4)=1                      ! we are allways taking TDIV1
            IQ(LPMHTT+5)=0                      ! not using pads in L2
          END DO   
C
C*** Book and fill bank STTH
C
          DO J=1,NHIT 
            CALL CBYT (TUBE_ADD(LTRG2,J),1, ISTA, 1, 5)  ! unpack station
            CALL CBYT (TUBE_ADD(LTRG2,J),6, ISEC, 1, 5)  ! unpack section
            CALL CBYT (TUBE_ADD(LTRG2,J),17,ITUBE,1,16)  ! unpack tube
            CALL SATOPM(ISTA,ISEC,IMOD)                ! get the module #
            IADD(J)=IMOD*256+ITUBE                     ! cell adress
            JHT = GEOM_ADD(LTRG2,J)                    ! tube geometry address
            DO K=1,6
              GEOM(K,J)=C(JHT+K)                       ! Save tube geometry
            ENDDO
          ENDDO
          CALL STTHFL(IMUOT,NHIT,IADD,GEOM,DRFT)       ! book and fill STTH
          CALL MUIFW3(IMUOT)                           ! sets IFW3 flag
        ENDIF
  100 CONTINUE                      
*
  999 RETURN
      END
