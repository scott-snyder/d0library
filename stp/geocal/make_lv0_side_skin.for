      SUBROUTINE MAKE_LV0_SIDE_SKIN(LUN)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : WRITE OUT SIDE SKIN PARAMETERS FOR GEOLV0
C-
C-   Inputs  : LUN [I] UNIT NUMBER TO WRITE OUT SRCP FILE
C-   Outputs : NONE
C-   Controls: SRCP_RAW_LV0
C-
C-   Created  14-FEB-1992   Freedy Nang
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL SST,VAL,TS_XC,TS_YC,TS_ZC,TS_PAR(3),INCH_CM
      INTEGER TS_MED,TS_RM,TS_CN,TS_NP,LUN,IC
      INTEGER TYPE,LVAL,IVAL,IER,IS,IP,JP,I,J,L,IZ
      CHARACTER CVAL*4,ZEE(2)*1,TS_NAME*4,TS_SHAPE*4,TS_MOTH*4,TS_PTYP*4
      CHARACTER PARAM*32, COPY_NAME*1
      EQUIVALENCE (VAL,IVAL)
      DATA ZEE/'+','-'/
C----------------------------------------------------------------------
      CALL EZGET('SIDE_SKIN_THICKNESS',SST,IER)
      CALL EZGET('INCH_CM',INCH_CM,IER)
      SST = SST*INCH_CM
      IC=1
      WRITE(COPY_NAME,2)IC
      DO IZ = 1,1   !+/- Z, JUST + FOR NOW
        DO IS = 1, 6
          CALL EZGETS('TILE_SKIN_BOX_NAMES',IS,PARAM,LVAL,IER)
          PARAM = PARAM(1:LVAL)
          IP = 1
          DO WHILE(IP.LT.12)
            CALL EZGET_NEXT_VALUE_TYPE(PARAM,VAL,CVAL,TYPE,LVAL,IER,IP)
C 'TB'  'BOX' 9 'L0M'    'POSP' 1     1     5.6875 -1.3125  0.875    3
C  7.0625  0.0625  0.4375
            IF(IP.EQ.2)TS_NAME  =  CVAL(1:LVAL)//COPY_NAME//ZEE(IZ)
            IF(IP.EQ.3)TS_SHAPE =  CVAL(1:LVAL)
            IF(IP.EQ.4)TS_MED   =  IVAL
            IF(IP.EQ.5)TS_MOTH  =  CVAL(1:LVAL)//ZEE(IZ)
            IF(IP.EQ.6)TS_PTYP  =  CVAL(1:LVAL)
            IF(IP.EQ.7)TS_RM    =  IVAL
            IF(IP.EQ.8)TS_CN    =  IVAL
            IF(IP.EQ.9)TS_XC    =  VAL*INCH_CM
            IF(IP.EQ.10)TS_YC   =  VAL*INCH_CM
            IF(IP.EQ.11)TS_ZC   =  VAL*INCH_CM
            IF(IP.EQ.12) THEN
              TS_NP  =  IVAL
              DO JP = 1, TS_NP
                CALL EZGET_NEXT_VALUE_TYPE(PARAM,VAL,CVAL,TYPE,LVAL,IER,
     &            IP)
                TS_PAR(JP) = VAL*INCH_CM
              END DO
            ENDIF
          END DO
          CALL SWORDS(PARAM,I,J,L)
          WRITE(LUN,1)PARAM(I:J)//ZEE(IZ)//'1',
     &        TS_NAME,TS_SHAPE,TS_MED,TS_MOTH,TS_PTYP,
     &        TS_RM,TS_CN,TS_XC,TS_YC,TS_ZC,TS_NP,TS_PAR
        END DO
      END DO
    1 FORMAT(1X,'\ARRAY  ',A32,
     &  /1X,'''',A4,'''  ''',A4,'''',5X,I3,5X,'''',A4,'''  ''',A4,'''',
     &  /1X,2I8,3F12.4,I8,
     &  /1X,3F12.4,
     &  /1X,'\END')
    2 FORMAT(I1)
  999 RETURN
      END
