      SUBROUTINE MAKE_LV0_SET(ZEE,LUN)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : WRITE OUT DETECTOR SETS
C-
C-   Inputs  : +/- Z,
C-             LUN [I] UNIT NUMBER TO WRITE OUT SRCP FILE
C-   Outputs : NONE
C-   Controls: SRCP_RAW_LV0.DAT
C-
C-   Created  17-MAR-1992   Freedy Nang
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IP, IVAL, TYPE,LVAL, IER
      INTEGER M, N, P
      CHARACTER*32 NAME
      CHARACTER*4 CVAL
      REAL    VAL
      INTEGER LUN, ID, IX, IS, IZ, NSLAT
      CHARACTER*1 ZEE(2)
      CHARACTER*32 DETNAME
      CHARACTER*4 IUSET
      CHARACTER*4 TILEDN
      CHARACTER*4 NAMESH(2), NAMESD(2)
      CHARACTER*4 SUPPDN
      CHARACTER*4 PMTDN 
      INTEGER NSUPP, NPMT
      INTEGER NWHI, NWDI,ITRS, NHMAX, IDTYPE
      INTEGER NV, NBITSV, NH, NBITSH(2), ND
      INTEGER NBITSD(2)
      REAL    ORIG(2), FACT(2)
      EQUIVALENCE (VAL,IVAL)
C----------------------------------------------------------------------
      IP=1
      DO WHILE(IP.LT.12)
        CALL EZGET_NEXT_VALUE_TYPE('DETECTOR_SET',VAL,CVAL,TYPE,LVAL,
     &    IER,IP)
        IF(IP.EQ.2)  IUSET   = CVAL(1:LVAL)
        IF(IP.EQ.3)  NWHI    = IVAL
        IF(IP.EQ.4)  NWDI    = IVAL
        IF(IP.EQ.5)  ITRS    = IVAL
        IF(IP.EQ.6)  NHMAX   = IVAL
        IF(IP.EQ.7)  IDTYPE  = IVAL
        IF(IP.EQ.8)  NV      = IVAL
        IF(IP.EQ.9)  NBITSV  = IVAL
        IF(IP.EQ.10) NSLAT   = IVAL
        IF(IP.EQ.11) NSUPP   = IVAL
        IF(IP.EQ.12) NPMT    = IVAL
      ENDDO
      IP=1
      DO WHILE(IP.LT.4)
        CALL EZGET_NEXT_VALUE_TYPE('CHARAC',VAL,CVAL,TYPE,LVAL,IER,IP)
        IF(IP.EQ.2)  TILEDN   = CVAL(1:LVAL)
        IF(IP.EQ.3)  SUPPDN   = CVAL(1:LVAL)
        IF(IP.EQ.4)  PMTDN    = CVAL(1:LVAL)
      ENDDO
      IP=1
      DO WHILE(IP.LT.15)
        CALL EZGET_NEXT_VALUE_TYPE('DIGITIZE',VAL,CVAL,TYPE,LVAL,IER,IP)
        IF(IP.EQ.2)  NH         = IVAL
        IF(IP.EQ.3)  NAMESH(1)  = CVAL(1:LVAL)
        IF(IP.EQ.4)  NAMESH(2)  = CVAL(1:LVAL)
        IF(IP.EQ.5)  NBITSH(1)  = IVAL
        IF(IP.EQ.6)  NBITSH(2)  = IVAL
        IF(IP.EQ.7)  ORIG(1)    = VAL
        IF(IP.EQ.8)  ORIG(2)    = VAL
        IF(IP.EQ.9)  FACT(1)    = VAL
        IF(IP.EQ.10) FACT(2)    = VAL
        IF(IP.EQ.11) ND         = IVAL
        IF(IP.EQ.12) NAMESD(1)  = CVAL(1:LVAL)
        IF(IP.EQ.13) NAMESD(2)  = CVAL(1:LVAL)
        IF(IP.EQ.14) NBITSD(1)  = IVAL
        IF(IP.EQ.15) NBITSD(2)  = IVAL
      ENDDO
C
C ****  LOOP OVER PLUS AND MINUS Z
C
      DO IZ = 1,2   !+/- Z
C
C ****  DETECTOR SETS TILES
C
        ID=IDTYPE
        WRITE(DETNAME,111)ZEE(IZ)
        IUSET=TILEDN(1:3)//ZEE(IZ)
        WRITE(LUN,1)DETNAME,
     &    IUSET,NWHI,NWDI,ITRS,NHMAX,NSLAT
        DO IX = 1,NSLAT
          CALL EZGETS('TILE_NAME',IX,NAME,LVAL,IER)
          NAME=NAME(1:LVAL)
          CALL SWORDS(NAME,M,N,P)
          WRITE(LUN,2)NAME(M:N)//ZEE(IZ),ID,NV,NAME(M:N)//ZEE(IZ), 
     &        NBITSV,NH,NAMESH,NBITSH,
     &        ORIG,FACT,ND,NAMESD,NBITSD
        ENDDO
        WRITE(LUN,3)
C
C ****  DETECTOR SETS SUPPORTS
C
        ID = IDTYPE + 1
        WRITE(DETNAME,121)ZEE(IZ)
        IUSET = SUPPDN(1:3)//ZEE(IZ)
        WRITE(LUN,1)DETNAME,
     &    IUSET,NWHI,NWDI,ITRS,NHMAX,NSUPP
        DO IS = 1,NSUPP
          IF (IS.LT.7 ) THEN
            CALL EZGETS('BOX_NAME',IS,NAME,LVAL,IER)
          ELSE
            IX =(IS -6)
            CALL EZGETS('TRD1_NAME',IX,NAME,LVAL,IER)
          ENDIF
          NAME=NAME(1:LVAL)
          CALL SWORDS(NAME,M,N,P)
          WRITE(LUN,2)NAME(M:N)//ZEE(IZ),ID,NV,NAME(M:N)//ZEE(IZ),
     &      NBITSV,NH,NAMESH,NBITSH,
     &      ORIG,FACT,ND,NAMESD,NBITSD
        ENDDO
        WRITE(LUN,3)
C
C ****  DETECTOR SETS PMTS
C
        ID = IDTYPE + 2
        WRITE(DETNAME,131)ZEE(IZ)
        IUSET=PMTDN(1:3)//ZEE(IZ)
        WRITE(LUN,1)DETNAME,
     &    IUSET,NWHI,NWDI,ITRS,NHMAX,NPMT
        DO IX = 1, NPMT
          CALL EZGETS('PMT_NAME',IX,NAME,LVAL,IER)
          NAME=NAME(1:LVAL)
          CALL SWORDS(NAME,M,N,P)
          WRITE(LUN,2)NAME(M:N)//ZEE(IZ),ID,NV,NAME(M:N)//ZEE(IZ),
     &      NBITSV,NH,NAMESH,NBITSH,
     &      ORIG,FACT,ND,NAMESD,NBITSD
        ENDDO
        WRITE(LUN,3)
      ENDDO
    1 FORMAT(1X,'\ARRAY  ',A32,
     &  /1X,'''',A4,'''',2X,4I7,
     &  /1X,I5)
    2 FORMAT(1X,'''',A4,'''',2I5,' ''',A4,'''',I3,
     &  /I3,2(' ''',A4,''''),2I4,2F4.1,2E8.1,I4,2(' ''',A4,''''),2I4)
    3 FORMAT(1X,'\END')
  111 FORMAT('IUSET_LV0_TILES',A1)
  121 FORMAT('IUSET_LV0_SUPPORT',A1)
  131 FORMAT('IUSET_LV0_PMT',A1)
  999 RETURN
      END
