      SUBROUTINE MAKE_LV0_MIXTURE(LUN)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : WRITE OUT CORRECT MIXTURES FOR LEVEL 0
C-
C-   Inputs  : LUN [I] UNIT NUMBER TO WRITE OUT SRCP FILE
C-   Outputs : NONE
C-   Controls: SRCP_RAW_LV0.DAT
C-
C-   Created   4-MAR-1992   Freedy Nang
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER TYPE, LVAL, IER
      INTEGER ISVOL, IFIELD, IP, NMIX, VAL, LUN
      INTEGER SCIN_MED, SCIN_NPART, SCIN_N1, SCIN_N2
      REAL    SCIN_MIX1, SCIN_MIX2, SCIN_DEN, RVAL
      REAL    PMT_MIX1, PMT_MIX2, PMT_DEN
      INTEGER PMT_MED, PMT_NPART, PMT_N1, PMT_N2 
      REAL    FIELDM, TMAXFD, DMXMS, DEEMAX, EPSIL, STMIN
      CHARACTER*4 CVAL, SCIN_NAM1, SCIN_NAM2, SCIN_NAM3 
      CHARACTER*4 PMT_NAM1, PMT_NAM2, PMT_NAM3
      EQUIVALENCE (RVAL,VAL)
C----------------------------------------------------------------------
      IP=1
      DO WHILE(IP.LT.9)
        CALL EZGET_NEXT_VALUE_TYPE('MEDIA_PARAM',VAL,CVAL,TYPE,LVAL,IER,
     &    IP)
        IF(IP.EQ.2) ISVOL   = VAL
        IF(IP.EQ.3) IFIELD  = VAL
        IF(IP.EQ.4) FIELDM  = RVAL
        IF(IP.EQ.5) TMAXFD  = RVAL
        IF(IP.EQ.6) DMXMS   = RVAL
        IF(IP.EQ.7) DEEMAX  = RVAL
        IF(IP.EQ.8) EPSIL   = RVAL
        IF(IP.EQ.9) STMIN   = RVAL
      ENDDO
      IP=1
      DO WHILE(IP.LT.22)
        CALL EZGET_NEXT_VALUE_TYPE('MEDIA',VAL,CVAL,TYPE,LVAL,IER,IP)
        IF(IP.EQ.2)  SCIN_NAM1    = CVAL(1:LVAL)
        IF(IP.EQ.3)  SCIN_NAM2    = CVAL(1:LVAL)
        IF(IP.EQ.4)  SCIN_NAM3    = CVAL(1:LVAL)
        IF(IP.EQ.5)  SCIN_MED     = VAL
        IF(IP.EQ.6)  SCIN_NPART   = VAL
        IF(IP.EQ.7)  SCIN_N1      = VAL
        IF(IP.EQ.8)  SCIN_N2      = VAL
        IF(IP.EQ.9)  SCIN_MIX1    = RVAL
        IF(IP.EQ.10)  SCIN_MIX2    = RVAL
        IF(IP.EQ.11)  SCIN_DEN     = RVAL
        IF(IP.EQ.12)  PMT_NAM1     = CVAL(1:LVAL)
        IF(IP.EQ.13)  PMT_NAM2     = CVAL(1:LVAL)
        IF(IP.EQ.14)  PMT_NAM3     = CVAL(1:LVAL)
        IF(IP.EQ.15)  PMT_MED      = VAL
        IF(IP.EQ.16)  PMT_NPART    = VAL
        IF(IP.EQ.17)  PMT_N1       = VAL
        IF(IP.EQ.18)  PMT_N2       = VAL
        IF(IP.EQ.19)  PMT_MIX1     = RVAL
        IF(IP.EQ.20)  PMT_MIX2     = RVAL
        IF(IP.EQ.21)  PMT_DEN      = RVAL
        IF(IP.EQ.22)  NMIX         = VAL
      ENDDO
C
C ****  MIXTURE FOR LV0
C
      WRITE(LUN,1)'LV0_MIXTURES',NMIX,
     &   SCIN_NAM1,SCIN_NAM2,SCIN_NAM3,
     &   SCIN_MED,SCIN_NPART,SCIN_N1,SCIN_N2,
     &   SCIN_MIX1,SCIN_MIX2,SCIN_DEN
      WRITE(LUN,2)PMT_NAM1,PMT_NAM2,PMT_NAM3,
     &   PMT_MED,PMT_NPART,PMT_N1,PMT_N2,
     &   PMT_MIX1,PMT_MIX2,PMT_DEN
C
C ****  MEDIUM PARAMETERS FOR LV0
C
      WRITE(LUN,'(1X,A16,I5)')'MXLV0_ISVOL'   ,ISVOL
      WRITE(LUN,'(1X,A16,I5)')'MXLV0_IFIELD'  ,IFIELD
      WRITE(LUN,'(1X,A16,F8.3)')'MXLV0_FIELDM'  ,FIELDM
      WRITE(LUN,'(1X,A16,F8.3)')'MXLV0_TMAXFD'  ,TMAXFD
      WRITE(LUN,'(1X,A16,F8.3)')'MXLV0_DMXMS'   ,DMXMS
      WRITE(LUN,'(1X,A16,F8.3)')'MXLV0_DEEMAX'  ,DEEMAX
      WRITE(LUN,'(1X,A16,F8.3)')'MXLV0_EPSIL'   ,EPSIL
      WRITE(LUN,'(1X,A16,F8.3)')'MXLV0_STMIN'   ,STMIN

    1 FORMAT(1X,'\ARRAY  ',A16,
     &  /1X,I10,3(2X,'''',A4,''''),5X,2I10,
     &  /1X,2I10,3F12.4)
    2 FORMAT(/1X,3(2X,'''',A4,''''),5X,2I10,
     &  /1X,2I10,3F12.4,
     &  /1X,'\END')
  999 RETURN
      END
