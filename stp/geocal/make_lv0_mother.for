      SUBROUTINE MAKE_LV0_MOTHER(INCH_CM,ZEE,LUN)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : WRITE OUT MOTHER VOLUME FOR GEOLV0
C-
C-   Inputs  : INCH TO CM CONVERSION FACTOR, +/- Z,
C-             LUN [I] UNIT NUMBER TO WRITE OUT SRCP FILE
C-
C-   Outputs : NONE
C-   Controls: SRCP_RAW_LV0.DAT
C-
C-   Created  22-FEB-1992   Freedy Nang
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL    MOTHER(19), INCH_CM, LV0XYZ(3,2)
      INTEGER I, J, L, M, N, P, IZ, IP, IS ,LUN
      CHARACTER*1 ZEE(2)
      CHARACTER*32 PARAM, NAME
      CHARACTER*4 CVAL
      CHARACTER*4 MO_SHAPE, MO_MOTH, MO_PTYP
      INTEGER TYPE, VAL, IVAL, LVAL, IER
      INTEGER MO_MED,MO_RM, MO_CN, MO_NP, ID(5)
      EQUIVALENCE (VAL, IVAL)
C----------------------------------------------------------------------
      CALL EZGET('MOTHER',MOTHER,IER)
      CALL EZGET('LV0XYZ',LV0XYZ,IER)
      CALL EZGET('LV0_ROTATION_ID',ID,IER)
C
C ****  CONVERT FROM ENGLISH TO METRIC
C
      DO I= 5,19
        MOTHER(I)=MOTHER(I)*INCH_CM
      ENDDO
      DO I= 1,3
        DO J= 1,2
          LV0XYZ(I,J)=LV0XYZ(I,J)*INCH_CM
        ENDDO
      ENDDO
      IP=1
      DO WHILE (IP.LT.8)
        CALL EZGET_NEXT_VALUE_TYPE('MOM',VAL,CVAL,TYPE,LVAL,IER,IP)
        IF(IP.EQ.2)  MO_SHAPE = CVAL(1:LVAL)
        IF(IP.EQ.3)  MO_MED   = IVAL
        IF(IP.EQ.4)  MO_MOTH  = CVAL(1:LVAL)
        IF(IP.EQ.5)  MO_PTYP  = CVAL(1:LVAL)
        IF(IP.EQ.6)  MO_RM    = IVAL
        IF(IP.EQ.7)  MO_CN    = IVAL
        IF(IP.EQ.8)  MO_NP    = IVAL
      ENDDO
C
C ****  LOOP OVER PLUS AND MINUS Z
C
      DO IZ= 1,2  !+/- Z
C
C ****  NEGATIVE Z ROTATION MATRIX IS FROM LV0_ROTATION_MATRICES(5)
C
         IF(IZ.EQ.2) MO_RM = ID(5)
C
C ****  WRITE STP FILE FOR LEVEL 0 MOTHER VOLUME
C
        DO IS = 1 ,1
          CALL EZGETS('MOTHER_TYPE',IS,PARAM,LVAL,IER)
          PARAM=PARAM(1:LVAL)
          CALL SWORDS(PARAM,I,J,L)
          CALL EZGETS('MOTHER_NAME',IS,NAME,LVAL,IER)
          NAME=NAME(1:LVAL)
          CALL SWORDS(NAME,M,N,P)
          WRITE(LUN,1)PARAM(I:J)//ZEE(IZ)//'1',
     &      NAME(M:N)//ZEE(IZ),
     &      MO_SHAPE,MO_MED,MO_MOTH,MO_PTYP,
     &      MO_RM,MO_CN,LV0XYZ(1,IZ),LV0XYZ(2,IZ),LV0XYZ(3,IZ),MO_NP,
     &      MOTHER
        ENDDO
      ENDDO
    1 FORMAT(1X,'\ARRAY  ',A32,
     &  /1X,'''',A4,'''  ''',A4,'''',5X,I3,5X,'''',A4,''' ''',A4,'''',
     &  /1X,2I8,3F12.4,I8,
     &  2(/1X,8F12.4),
     &  /1X,3F12.4,
     &  /1X,'\END')
  999 RETURN
      END
