      SUBROUTINE MAKE_LV0_PMT(INCH_CM,ZEE,LUN)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :WRITE OUT PMT PARAMETERS FOR GEOLV0
C-
C-   Inputs  : INCH TO CM CONVERSION FACTOR, +/- Z,
C-             LUN [I] UNIT NUMBER TO WRITE OUT SRCP FILE
C-
C-   Outputs :NONE
C-   Controls:SRCP_RAW_LV0.DAT
C-
C-   Created  19-FEB-1992   Freedy Nang
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL    INCH_CM, TILE(3), ENCLOSURE(3)
      INTEGER VAL, IVAL, PM_NP, SHIFT
      INTEGER LUN, TYPE, LVAL, IER, IP, IZ, IS
      INTEGER I, J, L, M, N, P, X, Y, Z
      CHARACTER*32 PARAM, NAME
      CHARACTER*4  CVAL
      CHARACTER*4   PM_SHAPE, PM_MOTH, PM_PTYP
      INTEGER PM_MED, PM_RM, PM_CN
      REAL    PM_XC(3), PM_YC(3), PM_ZC(3)
      REAL    PMX(3), PMY(3), PMZ(3),PAR(3)
      CHARACTER*1 ZEE(2)
      EQUIVALENCE (VAL,IVAL)
C----------------------------------------------------------------------
      CALL EZGET('TILE',TILE,IER)
      CALL EZGET('ENCLOSURE',ENCLOSURE,IER)
      CALL EZGET('PMX',PMX,IER)
      CALL EZGET('PMY',PMY,IER)
      CALL EZGET('PMZ',PMZ,IER)
      SHIFT=1
C
C ****  CONVERT FROM ENGLISH TO METRIC
C
      DO 10 I = 1 ,3
        TILE(I)=TILE(I)*INCH_CM
        ENCLOSURE(I)=ENCLOSURE(I)*INCH_CM
        PMX(I)=PMX(I)*INCH_CM
        PMY(I)=PMY(I)*INCH_CM
        PMZ(I)=PMZ(I)*INCH_CM
   10 CONTINUE

      IP=1
      DO WHILE(IP.LT.8)
        CALL EZGET_NEXT_VALUE_TYPE('PMT',VAL,CVAL,TYPE,LVAL,IER,IP)
        IF(IP.EQ.2)  PM_SHAPE = CVAL(1:LVAL)
        IF(IP.EQ.3)  PM_MED   = IVAL
        IF(IP.EQ.4)  PM_MOTH  = CVAL(1:LVAL)
        IF(IP.EQ.5)  PM_PTYP  = CVAL(1:LVAL)
        IF(IP.EQ.6)  PM_RM    = IVAL
        IF(IP.EQ.7)  PM_CN    = IVAL
        IF(IP.EQ.8)  PM_NP    = IVAL
      ENDDO
C
C ****  CALCULATE POSITIONS OF PMTS
C
      PM_XC(1)=(4.5*2.*TILE(1)+PMX(1))
      PM_YC(1)=-1.*(2.5*2.*TILE(2)+2*ENCLOSURE(1))
      PM_ZC(1)=2.*ENCLOSURE(2)
      PM_XC(2)=-PM_XC(1)
      PM_YC(2)=PM_YC(1)
      PM_ZC(2)=PM_ZC(1)
      PM_XC(3)=(4.5*2.*TILE(1)+PMX(1)+ENCLOSURE(1))
      PM_YC(3)=-2.*ENCLOSURE(1)
      PM_ZC(3)=PM_ZC(1)
C
C ****  WRITE STP FILE
C
      CALL SWORDS(PM_MOTH,X,Y,Z)
      DO 20 IZ = 1 ,2   !+/- Z
        DO 30 IS =1 ,3  !THREE SIMULATIONS OF PMTS
          CALL EZGETS('PMT_TYPE',IS,PARAM,LVAL,IER)
          PARAM=PARAM(1:LVAL)
          CALL SWORDS(PARAM,I,J,L)
          CALL EZGETS('PMT_NAME',IS,NAME,LVAL,IER)
          NAME=NAME(1:LVAL)
          CALL SWORDS(NAME,M,N,P)
          PAR(1) = PMX(IS)
          PAR(2) = PMY(IS)
          PAR(3) = PMZ(IS)
          CALL MAKE_LV0_WRITE(LUN,PARAM(I:J)//ZEE(IZ)//'1',
     &      NAME(M:N)//ZEE(IZ),
     &      PM_SHAPE,PM_MED,PM_MOTH(X:Y)//ZEE(IZ),PM_PTYP,
     &      PM_RM, PM_CN,PM_XC(IS),PM_YC(IS),PM_ZC(IS),PM_NP,
     &      PAR,SHIFT)
   30   CONTINUE
   20 CONTINUE
  999 RETURN
      END
