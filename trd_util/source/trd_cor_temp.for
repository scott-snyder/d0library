      REAL FUNCTION TRD_COR_TEMP
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : compute Temperature correction
C-
C-   Returned value  :
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  14-FEB-1996   A. Zylberstejn
C-
C----------------------------------------------------------------------
C-
      IMPLICIT NONE
      INTEGER MPOINT,NPOINT
      PARAMETER( MPOINT =5500 )
      REAL TIM_REF(MPOINT),COR_REF(MPOINT),DAT_EVT,COR_EVT
      REAL R1(10)
      INTEGER LTCOR,GZTCOR,LOUT,TRUNIT
      INTEGER YEAR,MONTH,DAY,HOUR,MINUTE,SECOND
      INTEGER I,IFOIS,DICHOTOM,NP,TIME_OF_RUN(2)
      LOGICAL OK,SYS$NUMTIM,DOPRINT,TRD_DO_PRINT
      SAVE NP
      EXTERNAL SYS$NUMTIM
      INTEGER*2 UNPACKED_TIME(7)
      INCLUDE 'D0$INC:zebcom.INC'
      INCLUDE 'D0$INC:zebstp.INC'
      LOGICAL FIRST,RUN1A
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      IF(FIRST)THEN
        FIRST=.FALSE.
        IFOIS=0
        LOUT=TRUNIT()
        DOPRINT=TRD_DO_PRINT().AND.LOUT.NE.0
C        np=0
C  6     read(65,1001,ERR=8)(r1(i),i=1,10)
Cc        if(np.le.20)write(lout,*)' r1',r1
C        if(np.gt.npoint)then
C         write(lout,*)' error in  TRD_COR_TEMP'
C          np=np-1
C          go to 8
C        end if
C        do i=1,10,2
C          np=np+1
C         TIM_REF(np)=r1(i)
C         np=np+1
C         COR_REF(np)=r1(i+1)
C         if(np.le.25)print*,' np',np,'tim_ref,cor_ref',TIM_REF(np),
Cc     &     COR_REF(np)
C         end do
C         go to 6
C 1001 format(5(f10.4,f6.3))
        NP=0
        LTCOR=GZTCOR()
        IF(DOPRINT)WRITE(LOUT,*)' in TRD_COR_TEMP,ltcor',LTCOR
        IF(LTCOR.NE.0)THEN
          NPOINT=IC(LTCOR-1)
          IF(DOPRINT)WRITE(LOUT,*)' in TRD_COR_TEMP,npoint',NPOINT
          NP=1
          DO I=2,NPOINT-1,2
C             write(lout,*)' np',np,
C     &          ' time',c(ltcor+I),' tcor',c(ltcor+I+1)
            IF(C(LTCOR+I).NE.0.)THEN
              NP=NP+1
              IF(NP.GT.MPOINT)THEN
                CALL ERRMSG(' error reading TCOR','TRD_COR_TEMP',
     &            ' too many input points','w')
                IF(DOPRINT)WRITE(LOUT,*) ' npoint,np',NPOINT,NP
                NP=NP-1
                GO TO 8
              END IF
              TIM_REF(NP)=C(LTCOR+I)
              COR_REF(NP)=C(LTCOR+I+1)
            ELSE
              GO TO 8
            END IF
          END DO
        ELSE
          CALL ERRMSG('Bank TCOR does not exist ','TRD_COR_TEMP',
     &      ' temp. corrections not applied ' ,'W')
          GO TO 999
        END IF
C
    8   CONTINUE
        IF(DOPRINT)
     +    WRITE(LOUT,*) ' In  TRD_COR_TEMP nb. of correction points',NP
      END IF  ! end of initialization
      TRD_COR_TEMP=1.
      IF(NP.LE.0 .OR. RUN1A())GO TO 999
      IFOIS=IFOIS+1
      DOPRINT=TRD_DO_PRINT().AND.LOUT.NE.0
C      DOPRINT=IFOIS.LE.5
      IF(DOPRINT)
     +  WRITE(LOUT,*)' enter TRD_COR_TEMP ifois',IFOIS
C
      TIME_OF_RUN(1) = IQ(LHEAD+4)
      TIME_OF_RUN(2) = IQ(LHEAD+5)
      OK     = SYS$NUMTIM(UNPACKED_TIME(1),TIME_OF_RUN(1))
      YEAR   = UNPACKED_TIME(1) - (UNPACKED_TIME(1)/100)*100
      MONTH  = UNPACKED_TIME(2)
      DAY    = UNPACKED_TIME(3)
C      IDAT_EVENT   = 10000*YEAR+100*MONTH+DAY
      HOUR   = UNPACKED_TIME(4)
      MINUTE = UNPACKED_TIME(5)
      SECOND = UNPACKED_TIME(6)
      CALL CLDR(94,10000*YEAR+100*MONTH+DAY, HOUR*10000+MINUTE*100,
     &  DAT_EVT)
      I=DICHOTOM(DAT_EVT,TIM_REF,NP)
      IF(DOPRINT)WRITE(LOUT,*) ' dat,time', 10000*YEAR+100*MONTH+DAY,
     &   HOUR*10000+MINUTE*100,' dat_evt',DAT_EVT,' cor_evt',COR_REF(I)
      TRD_COR_TEMP=COR_REF(I)
  999 CONTINUE
      RETURN
      END
