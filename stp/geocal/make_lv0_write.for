      SUBROUTINE MAKE_LV0_WRITE(LUN,PARAM,NAME,SHAPE,MED,MOTH,PTYP,
     &      RM, CN,XC,YC,ZC,NP,PAR,SHIFT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :GET ROTATION MATRICES AND WRITE TO GEOLV0
C-
C-   Inputs  :
C-   Outputs :NONE
C-   Controls:SRCP_LV0_RAW.DAT
C-
C-   Created  24-FEB-1992   Freedy Nang
C-   Updated  19-APR-1994   Adam L. Lyon: Fixed to compile under IBM/AIX
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER LUN,MED,RM,CN,NP
      CHARACTER*(*) PARAM, NAME,SHAPE,MOTH,PTYP
      REAL XC,YC,ZC,PAR(NP),INCH_CM
      INCLUDE 'D0$INC:MATRIX.INC'
      INCLUDE 'D0$INC:WRITE_UNIT.INC'
      INCLUDE 'D0$INC:MATRIX_LIST.INC'
      INCLUDE 'D0$INC:pi.def'
      LOGICAL FIRST
      REAL    ANGLE(4),ZOFF(4),XC1,YC1,ZC1
      INTEGER ID(5),IER,IR,I,J,K,TRULEN
      INTEGER I1,J1,K1,SHIFT
      DATA FIRST /.TRUE./
      CHARACTER*255 FMT
C----------------------------------------------------------------------
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        CALL EZGET('LV0_COPY_ANGLES',ANGLE,IER)
        CALL EZGET('LV0_COPY_Z_OFFSETS',ZOFF,IER)
        CALL EZGET('LV0_ROTATION_ID',ID,IER)
        CALL EZGET('CONVERSION_FACTOR',INCH_CM,IER)
C----------------------------------------------------------------------
        CALL ZERO_MATRICES
        MATRIX_LIST_SRCP_LABEL='LV0_ROTATION_MATRICES'
        CALL VZERO(VAL_MATRIX(5),2)
C
C ****  ROTATION FOR -z MOTHER VOLUME
C
        ID_MATRIX = ID(5)
        VAL_MATRIX(1) = 90.
        VAL_MATRIX(2) = 180.
        VAL_MATRIX(3) = 90.
        VAL_MATRIX(4) = 90.
        VAL_MATRIX(5) = 180.
        VAL_MATRIX(6) = 0.
        CALL STORE_MATRIX
C
C ****  ROTATION MATRICES FOR FLIPPING TILES
C
        CALL VZERO(VAL_MATRIX(5),2)
        DO IR=1,4
          ZOFF(IR)=ZOFF(IR)*INCH_CM
          ID_MATRIX = ID(IR)
          VAL_MATRIX(1) = 90.
          VAL_MATRIX(2) = ANGLE(IR)
          VAL_MATRIX(3) = 90.
          VAL_MATRIX(4) = 90.+ ANGLE(IR)
          CALL STORE_MATRIX
        ENDDO
        OUT_VOL = LUN
        CALL WRITE_MATRICES
      ENDIF
      DO IR = 1,4
        XC1 = XC*COS(ANGLE(IR)*pi/180) + YC*SIN(ANGLE(IR)*pi/180)
        YC1 = XC*(-SIN(ANGLE(IR)*pi/180)) + YC*COS(ANGLE(IR)*pi/180)
        IF (SHIFT.EQ.1 ) THEN
          ZC1 = ZC - ZOFF(IR)
        ELSE
          ZC1 = ZC
        ENDIF
        CALL SWORDS(PARAM,I,J,K)
        WRITE(FMT,'(''('',I1,''F12.4)'')')NP
        CALL SWORDS(FMT,I1,J1,K1)
        WRITE(PARAM(J:J),'(I1)')IR
        WRITE(LUN,1)PARAM(I:J), NAME(1:TRULEN(NAME)),
     &      SHAPE,MED,MOTH,PTYP,
     &      ID(IR),IR,XC1,YC1,ZC1,NP
        WRITE(LUN,FMT(I1:J1))(PAR(I),I= 1, NP)
        WRITE(LUN,2)
    1   FORMAT(1X,'\ARRAY  ',A32,
     &    /1X,'''',A4,'''  ''',A4,'''',5X,I3,5X,'''',A4,''' ''',A4,'''',
     &    /1X,2I8,3F12.4,I8)
    2   FORMAT(/1X,'\END')
      END DO
  999 RETURN
      END
