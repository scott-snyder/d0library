      SUBROUTINE CJET_MUCATEFL
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-     Book and fill CATE bank starting from CAEH bank. Identical to 
C-     CATEFL, except that CATE is booked regardless of whether one 
C-     already exists.
C-
C-   CREATED : 21-FEB-93 : ALEX SMITH 
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZLINKC.INC'       ! Protected Link area
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$INC:PTCATE.INC'
C
      INCLUDE 'D0$LINKS:IZCATE.LINK'
C
      LOGICAL OK
      CHARACTER*40 MSG
      INTEGER I,J,J1,K,L,ID,IOH,CATEPT
      INTEGER GZCAHT
      INTEGER GZCATE,GZCAEH
      INTEGER IETA,IPHI,ILYR,IHD,PT_CATE,PT_CAEH,PT_CAT1
      INTEGER NCH,NR,NCHT,NRT,NTOT,NALOC
C
      INTEGER NTWMAX
      PARAMETER (NTWMAX=4800)
      REAL    ETLIST(NTWMAX,2),ETCATE(-NETAL:NETAL,NPHIL,2),ET
      REAL EXCATE(-NETAL:NETAL,NPHIL,2),EYCATE(-NETAL:NETAL,NPHIL,2)
      REAL EX,EY
      INTEGER NTWRS(2),IDLIST(NTWMAX,2),ORLIST(NTWMAX,2)
      INTEGER EVNTID,EVONUM
C
      INCLUDE 'D0$PARAMS:CATENM.PARAMS'
C
C----------------------------------------------------------------------
C
      LCATE=GZCATE()
CC      IF(LCATE.NE.0) GOTO 999    ! If bank exists do nothing
C
      LCAEH=GZCAEH()
      IF(LCAEH.LE.0) THEN        ! Abort if CAEH does not exist 
        CALL ERRMSG('NO_CATE_W/O_CAEH','CJET_MUCATEFL',
     &    'SKIP CJET_MUCATEFL','W')
        GOTO 999
      ENDIF
C
C        initialize
C
      PTTFLG=.TRUE.
      NTOT=(2*NETAL+1)*NPHIL*2
      CALL UZERO(ETCATE,1,NTOT)
      CALL UZERO(EXCATE,1,NTOT)
      CALL UZERO(EYCATE,1,NTOT)
      CALL vZERO_i(PTCATE,NTOT)
C--
C--   UZEROs below not needed; useful for debugging
C--
      CALL UZERO(ETLIST,1,2*NTWMAX)
      CALL vZERO_i(IDLIST,2*NTWMAX)
      CALL vZERO_i(ORLIST,2*NTWMAX)
C
C        loop over CAEH hits for the first time; find number of towers
C
      NCH=IQ(LCAEH+3)                   ! Number of cells
      NR=IQ(LCAEH+2)                    ! CAEH repetition number
      PT_CAEH=LCAEH
      IF(NCH.LE.0 .OR. NR.LE.0) GOTO 999
      DO 1 I=1,NCH
        IETA=IQ(PT_CAEH+12)
        IPHI=IQ(PT_CAEH+13)
        ILYR=IQ(PT_CAEH+14)
        ET  = Q(PT_CAEH+8)
        EX  = Q(PT_CAEH+4)
        EY  = Q(PT_CAEH+5)
        IF(ILYR.LE.MXLYEM)then
           ETCATE(IETA,IPHI,1)=ETCATE(IETA,IPHI,1)+ET
           EXCATE(IETA,IPHI,1)=EXCATE(IETA,IPHI,1)+EX
           EYCATE(IETA,IPHI,1)=EYCATE(IETA,IPHI,1)+EY
        ENDIF
        ETCATE(IETA,IPHI,2)=ETCATE(IETA,IPHI,2)+ET
        EXCATE(IETA,IPHI,2)=EXCATE(IETA,IPHI,2)+EX
        EYCATE(IETA,IPHI,2)=EYCATE(IETA,IPHI,2)+EY
        PT_CAEH=PT_CAEH+NR
    1 CONTINUE
C
C ****  find number of towers
C
      NTWRS(1)=0
      NTWRS(2)=0
      DO 11 IETA = -NETAL ,  NETAL
        DO 12 IPHI = 1 ,  NPHIL
          DO 13 K = 1 ,  2
            EX=EXCATE(IETA,IPHI,K)
            EY=EYCATE(IETA,IPHI,K)
            ET=SQRT(EX*EX+EY*EY)
            IF(ETCATE(IETA,IPHI,K).LT.0.0)ET=-ET
            ETCATE(IETA,IPHI,K)=ET
            IF(ABS(ETCATE(IETA,IPHI,K)).GT. 0.0) THEN
              NTWRS(K)=NTWRS(K)+1
              L=NTWRS(K)
              IF(L.GT.NTWMAX) THEN
                WRITE(MSG,111) EVONUM()
                CALL ERRMSG('Too many CAL Towers',
     &            'CJET_MUCATEFL',MSG,'W')
                GOTO 999        !  failure return
              ELSE
                ETLIST(L,K)=ETCATE(IETA,IPHI,K)
                IDLIST(L,K)=(IETA+NETAL)*100+IPHI
                ORLIST(L,K)=L 
              ENDIF
            ENDIF
   13     CONTINUE
   12   CONTINUE
   11 CONTINUE
C
C ****  Order lists; fill PTCATE array
C
      DO 31 K = 1 ,  2
        CALL SRTFLT(ETLIST(1,K),NTWRS(K),ORLIST(1,K))
        DO 32 J = 1 ,  NTWRS(K)
          J1=NTWRS(K)+1-J
          L=ORLIST(J1,K)
          ID=IDLIST(L,K)
          IETA=ID/100-NETAL
          IPHI=MOD(ID,100)
          PT_CAEH=J 
          IF(K.EQ.2) PT_CAEH=PT_CAEH+NTWRS(1)
          PTCATE(IETA,IPHI,K)=PT_CAEH
   32   CONTINUE
   31 CONTINUE
C
C ****  Book the CATE bank
C
      NCHT=NTWRS(1)+NTWRS(2)
      CALL BKCATE(NCHT,LCATE)
C
C         fill CATE bank
C
      IQ(LCATE+3)=NCHT + CATENM*NTWRS(1)  ! number of channels + CATENM*NEM
C
C          loop over CAEH channels
C
      LCAEH=GZCAEH()
      NR =IQ(LCAEH+2)
      NCH=IQ(LCAEH+3)
      PT_CAEH=LCAEH
      NRT=IQ(LCATE+2)
      DO 41 I=1,NCH
        IETA=IQ(PT_CAEH+12)
        IPHI=IQ(PT_CAEH+13)
        ILYR=IQ(PT_CAEH+14)
        IHD=1
C
C ****  HADRON FLAG =2 MEANS NON-EM ENERGY AT THIS POINT.
C ****  FH LAYER 1 IS INCLUDED IN THE BIT LIST FOR IHD=1
C ****  BUT NOT ADDED TO IHD=1 TOWERS.
C
        IF(ILYR.GT.MXLYEM) IHD=2     ! HADRONIC.
C
   45   CATEPT=PTCATE(IETA,IPHI,IHD)
        IF (CATEPT.LE.0) GOTO 43
        PT_CATE=LCATE+(CATEPT-1)*NRT
C            fill energies and sigmas
        DO 42 J = 4 ,  10
          Q(PT_CATE+J)=Q(PT_CATE+J)+Q(PT_CAEH+J)
   42   CONTINUE
C                 calculate ET correctly
        IF(Q(PT_CATE+7).GT.0.0)THEN
          Q(PT_CATE+8) = SQRT(Q(PT_CATE+4)**2 + Q(PT_CATE+5)**2)
        ELSE
          Q(PT_CATE+8) = -SQRT(Q(PT_CATE+4)**2 + Q(PT_CATE+5)**2)
        ENDIF
C                 fill rest
        IQ(PT_CATE+11)=IQ(PT_CATE+11)+1 ! layer
        IQ(PT_CATE+12)=IETA
        IQ(PT_CATE+13)=IPHI
        IQ(PT_CATE+14) = IHD
        IQ(PT_CATE+15) = 0                 ! Reserved for EM clusters
        IQ(PT_CATE+16) = 0                 ! Reserved for hadronic clusters
        CALL SBIT1(IQ(PT_CATE+17),ILYR)    ! Layer ILYR contributes
C              add first fine hadronic to bit list in em tower
        IF(ILYR.EQ.MNLYFH) THEN
          CATEPT=PTCATE(IETA,IPHI,1)
          IF(CATEPT.GT.0)THEN
            PT_CAT1=LCATE+(CATEPT-1)*NRT
            CALL SBIT1(IQ(PT_CAT1+17),ILYR)
            IQ(PT_CAT1+11)=IQ(PT_CAT1+11)+1 ! layer
          ENDIF
        ENDIF
C
        IF(IHD.EQ.1) THEN
          IHD=2
          GOTO 45
        ENDIF  
   43   PT_CAEH=PT_CAEH+NR
   41 CONTINUE
C
c protect against cases where Et > E due to round off errors
c      
      DO I=1,NCHT 
        PT_CATE=LCATE+(I-1)*NRT
        IF(ABS(Q(PT_CATE+8)).GT.ABS(Q(PT_CATE+7)))THEN
          Q(PT_CATE+7)=Q(PT_CATE+8)
        ENDIF
      ENDDO
      PTTFLG=.FALSE.           ! set flag indicating PTCATE is not 0
  999 RETURN
  111 FORMAT(' SKIP PROCESSING EVENT NO.',I8)
      END
