      FUNCTION VTZERO_EVT()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : get drift time distributions from VTPULS
C-                         for TZERO offline calculation
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  27-MAY-1992             V. D. Elvira
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      LOGICAL VTZERO_EVT
      LOGICAL ZFLAG
C----------------------------------------------------------------------
      REAL TMIN,RAW_TIME
C----------------------------------------------------------------------
      INTEGER LAYER,SECTOR,WIRE,IEND,LVWDA,GZVWDA,NEL,NWORDS,POINT
      INTEGER NLRS,NSCT(3),NWRE,LRS(3),SCTS0(16),SCTS1(32),WRE(8)
      INTEGER IL,IS,IW,HIT,NPUL,ID,IER
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$INC:ZEBSTP.INC'
C----------------------------------------------------------------------
      DATA LRS/0,1,2/
      DATA SCTS0/0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15/
      DATA SCTS1/0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,
     &  21,22,23,24,25,26,27,28,29,30,31/
      DATA WRE/0,1,2,3,4,5,6,7/
C----------------------------------------------------------------------
C
      NLRS=3
      NSCT(1)=16
      NSCT(2)=32
      NSCT(3)=32
      NWRE=8
C----------------------------------------------------------------------
      VTZERO_EVT=.TRUE.
      ZFLAG=.FALSE.
C----------------------------------------------------------------------
C-  Loop over all channels geting the earliest hit and filling drift
C-  time distributions
C----------------------------------------------------------------------
      DO 1 IL=1,NLRS
        LAYER=LRS(IL)
        DO 2 IS=1,NSCT(IL)
          IF (LAYER.EQ.0) THEN
            SECTOR=SCTS0(IS)
          ELSE
            SECTOR=SCTS1(IS)
          ENDIF
C----------------------------------------------------------------------
          LVWDA=GZVWDA(LAYER,SECTOR)
          IF (LVWDA.EQ.0) GOTO 2
          NEL=IQ(LVWDA+2)
          NWORDS=IQ(LVWDA+3)
          DO 3 IW=1,NWRE
            WIRE=WRE(IW)
            DO 4 IEND=0,1
              NPUL=IQ(LVWDA+2*WIRE+4+IEND)
              IF (NPUL.EQ.0) GOTO 4
              POINT=IQ(LVWDA+NEL+2*WIRE+4+IEND)
              TMIN=99999.
              DO 5 HIT=1,NPUL
                RAW_TIME = Q(LVWDA+POINT+(HIT-1)*NWORDS+1)
                TMIN=AMIN1(TMIN,RAW_TIME)
    5         CONTINUE
              ID=10000+512*LAYER+16*SECTOR+2*WIRE+IEND
              CALL HF1(ID,TMIN,1.)
    4       CONTINUE
    3     CONTINUE
    2   CONTINUE
    1 CONTINUE
  999 RETURN
      END
