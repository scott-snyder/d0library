      SUBROUTINE MAKE_LV0_COVER(INCH_CM,ZEE,LUN)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :WRITE OUT BOX PARAMETERS FOR GEOLV0
C-
C-   Inputs  : INCH TO CM CONVERSION FACTOR, +/- Z,
C-             LUN [I] UNIT NUMBER TO WRITE OUT SRCP FILE
C-
C-   Outputs : NONE
C-   Controls: SRCP_RAW_LV0.DAT
C-
C-   Created  20-FEB-1992   Freedy Nang
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL    INCH_CM, TILE(3), ENCLOSURE(10)
      INTEGER I, J, L, M, N, P, LUN, IP, X, Y, Z
      CHARACTER*32 PARAM, NAME
      CHARACTER*4  CVAL, BO_SHAPE, TR_SHAPE, TB_SHAPE
      CHARACTER*4  BO_MOTH, BO_PTYP
      REAL    BO_XC(6), BO_YC(6), BO_ZC(6)
      REAL    BOX(6), BOY(6), BOZ(6), PMX(3)
      REAL    TRD1(3), TR_XC(4), TR_YC(4), TR_ZC(4)
      REAL    TOP_BOT_DIM(10), TB_XYZ(3,2), PAR(3)
      INTEGER VAL, IVAL, BO_NP, TR_NP, TB_NP
      INTEGER TYPE, LVAL, IER, BO_MED, BO_RM, BO_CN
      INTEGER IZ, IS, SHIFT
      CHARACTER*1 ZEE(2)
      EQUIVALENCE (VAL,IVAL)
C----------------------------------------------------------------------
      CALL EZGET('TILE',TILE,IER)
      CALL EZGET('ENCLOSURE',ENCLOSURE,IER)
      CALL EZGET('PMX',PMX,IER)
      CALL EZGET('BOX',BOX,IER)
      CALL EZGET('BOY',BOY,IER)
      CALL EZGET('BOZ',BOZ,IER)
      CALL EZGET('TRD1',TRD1,IER)
      CALL EZGET('TOP_BOT_DIM',TOP_BOT_DIM,IER)
      CALL EZGET('TB_XYZ',TB_XYZ,IER)
C
C ****  CONVERT FROM ENGLISH TO METRIC
C
      DO 10 I = 1 ,3
        TILE(I)=TILE(I)*INCH_CM
        PMX(I)=PMX(I)*INCH_CM
        DO J = 1 ,2
          TB_XYZ(I,J)=TB_XYZ(I,J)*INCH_CM
        ENDDO
   10 CONTINUE
      DO I = 1 , 10
        ENCLOSURE(I)=ENCLOSURE(I)*INCH_CM
      ENDDO
      DO I = 1 ,6
        BOX(I)=BOX(I)*INCH_CM
        BOY(I)=BOY(I)*INCH_CM
        BOZ(I)=BOZ(I)*INCH_CM
      ENDDO
      DO I = 1 ,3
        TRD1(I)=TRD1(I)*INCH_CM
      ENDDO
      DO I = 5 ,10
        TOP_BOT_DIM(I)= TOP_BOT_DIM(I)*INCH_CM
      ENDDO
      IP=1
      DO WHILE(IP.LT.12)
        CALL EZGET_NEXT_VALUE_TYPE('CAJA',VAL,CVAL,TYPE,LVAL,IER,IP)
        IF(IP.EQ.2)  BO_SHAPE = CVAL(1:LVAL)
        IF(IP.EQ.3)  TR_SHAPE = CVAL(1:LVAL)
        IF(IP.EQ.4)  TB_SHAPE = CVAL(1:LVAL)
        IF(IP.EQ.5)  BO_MED   = IVAL
        IF(IP.EQ.6)  BO_MOTH  = CVAL(1:LVAL)
        IF(IP.EQ.7)  BO_PTYP  = CVAL(1:LVAL)
        IF(IP.EQ.8)  BO_RM    = IVAL
        IF(IP.EQ.9)  BO_CN    = IVAL
        IF(IP.EQ.10) BO_NP    = IVAL
        IF(IP.EQ.11) TR_NP    = IVAL
        IF(IP.EQ.12) TB_NP    = IVAL
      END DO
C
C ****  CALCULATE POSITIONS OF BOXES
C ****  BO_ZC(5) CALCULATED BY TRIAL AND ERROR, THERE IS NO EXPLANATION
C ****  FOR THE FACTOR OF 1.3 OTHER THAN IT WORKS!
C
      BO_XC(1)=0.
      BO_YC(1)=-1.*(4.5*2.*TILE(2)+1.5*2.*ENCLOSURE(1))
      BO_ZC(1)=2.*ENCLOSURE(2)
      BO_XC(2)=-(ENCLOSURE(5)+ENCLOSURE(10)-TILE(1))
      BO_YC(2)=-(TILE(2)+(2.-1.)*ENCLOSURE(1))
      BO_ZC(2)=BO_ZC(1)
      BO_XC(3)=1.*(TILE(1)+ENCLOSURE(1))
      BO_YC(3)=-2.*ENCLOSURE(1)
      BO_ZC(3)=BO_ZC(1)
      BO_XC(4)=1.*(ENCLOSURE(4)+ENCLOSURE(10)+TILE(1))
      BO_YC(4)=(TILE(2)-(2.-1.)*ENCLOSURE(1))
      BO_ZC(4)=BO_ZC(1)
      BO_XC(5)=-(4.5*2.*TILE(1)+2.*PMX(1)+ENCLOSURE(1))
      BO_YC(5)=-(TILE(2)+2.*ENCLOSURE(1)+2.*2.*TILE(2))
      BO_ZC(5)= (BOZ(6)-1.3*ENCLOSURE(2))
      BO_XC(6)=-BO_XC(5)
      BO_YC(6)=-(5.*TILE(2)+2.*ENCLOSURE(1)-TILE(2))
      BO_ZC(6)=BO_ZC(5)

      TR_XC(1)=(BOX(1)+TRD1(1))
      TR_YC(1)=-1.*(4.5*2.*TILE(2)+3.*ENCLOSURE(1))
      TR_ZC(1)=ENCLOSURE(8)
      TR_XC(2)=-TR_XC(1)
      TR_YC(2)=TR_YC(1)
      TR_ZC(2)=TR_ZC(1)
      TR_XC(3)=-TR_XC(1)
      TR_YC(3)=-(TILE(2)+(2.-1.)*ENCLOSURE(1))
      TR_ZC(3)=TR_ZC(1)
      TR_XC(4)=TR_XC(1)
      TR_YC(4)=(TILE(2)-(2.-1.)*ENCLOSURE(1))
      TR_ZC(4)=TR_ZC(1)

C
C ****  WRITE STP FILE
C
      CALL SWORDS(BO_MOTH,X,Y,Z)
      DO IZ = 1 ,2    !+/-Z
        DO IS = 1 ,1      ! LUMP TOP AND BOT INTO ONE
          CALL EZGETS('TOP_BOT_TYPE',IS,PARAM,LVAL,IER)
          PARAM=PARAM(1:LVAL)
          CALL SWORDS(PARAM,I,J,L)
          CALL EZGETS('TOP_BOT_NAME',IS,NAME,LVAL,IER)
          NAME=NAME(1:LVAL)
          CALL SWORDS(NAME,M,N,P)
          WRITE(LUN,1)PARAM(I:J)//ZEE(IZ)//'1',
     &        NAME(M:N)//ZEE(IZ),
     &        TB_SHAPE,BO_MED,BO_MOTH(X:Y)//ZEE(IZ),BO_PTYP,
     &        BO_RM,BO_CN,TB_XYZ(1,IZ),TB_XYZ(2,IZ),TB_XYZ(3,IZ),TB_NP,
     &        TOP_BOT_DIM
        END DO
        DO 30 IS = 1 ,6   !TOTAL OF SIX BOXES
          CALL EZGETS('BOX_TYPE',IS,PARAM,LVAL,IER)
          PARAM=PARAM(1:LVAL)
          CALL SWORDS(PARAM,I,J,L)
          CALL EZGETS('BOX_NAME',IS,NAME,LVAL,IER)
          NAME=NAME(1:LVAL)
          CALL SWORDS(NAME,M,N,P)
          PAR(1)=BOX(IS)
          PAR(2)=BOY(IS)
          PAR(3)=BOZ(IS)
          IF(IS.LT.5) THEN
            SHIFT=1
          ELSE
            SHIFT=0
          ENDIF
          CALL MAKE_LV0_WRITE(LUN,PARAM(I:J)//ZEE(IZ)//'1',
     &        NAME(M:N)//ZEE(IZ),
     &        BO_SHAPE,BO_MED,BO_MOTH(X:Y)//ZEE(IZ),BO_PTYP,
     &        BO_RM,BO_CN,BO_XC(IS),BO_YC(IS),BO_ZC(IS),BO_NP,
     &        PAR,SHIFT)
   30   CONTINUE
        SHIFT=0
        DO IS = 1 ,4      !TOTAL OF FOUR TRAPEZOIDS
          CALL EZGETS('TRD1_TYPE',IS,PARAM,LVAL,IER)
          PARAM=PARAM(1:LVAL)
          CALL SWORDS(PARAM,I,J,L)
          CALL EZGETS('TRD1_NAME',IS,NAME,LVAL,IER)
          NAME=NAME(1:LVAL)
          CALL SWORDS(NAME,M,N,P)
          CALL MAKE_LV0_WRITE(LUN,PARAM(I:J)//ZEE(IZ)//'1',
     &      NAME(M:N)//ZEE(IZ),
     &      TR_SHAPE,BO_MED,BO_MOTH(X:Y)//ZEE(IZ),BO_PTYP,
     &      BO_RM,BO_CN,TR_XC(IS),TR_YC(IS),TR_ZC(IS),
     &      TR_NP,
     &      TRD1,SHIFT)
        ENDDO
      ENDDO
    1 FORMAT(1X,'\ARRAY  ',A32,
     &  /1X,'''',A4,'''  ''',A4,'''',5X,I3,5X,'''',A4,''' ''',A4,'''',
     &  /1X,2I8,3F12.4,I8,
     &  2(/1X,5F12.4),
     &  /1X,'\END')
  999 RETURN
      END
