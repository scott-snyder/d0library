      SUBROUTINE MUHITA(ITRAK,NPTRAK,
     1           NTOT,NX,NY,XHIT,YHIT,ZHIT,SINE,COSINE,WT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : fill arrays of hit coordinates and weights
C-   for the BC segment of a WAMUS track 
C-
C-   Inputs  : ITRAK  track number
C-             NPTRAK number of hits on track
C-   Outputs : ITOT  number of good A layer measurements
C-             XHIT,YHIT,ZHIT coordinates of a point
C-             SINE,COSINE    direction of measurement wrt to x axis
C-             WT  weight of a measumement
C-
C-   Created  31-AUG-1992   Daria Zieminska
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:PI.DEF'
      REAL XHIT(*),YHIT(*),ZHIT(*),WT(*),SINE(*),COSINE(*)
      REAL DRIFT,WIRE,PADREP,WAVELEN,WTWIRE,WTDRIFT
      INTEGER ORENT,NTOT,NX,NY,ITOT,ITRAK,NPTRAK,NT,NHIT,NVER
      INTEGER IHT,NHT,IWADD,IHMUOH,TIMSIN,LMUOH,GZMUOH,IPAD,IDELT
      INTEGER IMOD,IPLN,IWIR,IERR,MULAYR
      DATA WTDRIFT,WTWIRE/25.,0.5/
      DATA PADREP/60.96/
      LMUOH = GZMUOH(0)
      ITOT=0
      NX=0
      NY=0
      DO 40 IHT = 1,NPTRAK
        CALL GTMHTT(ITRAK,IHT,IWADD,IHMUOH,TIMSIN,IDELT,IPAD)
        NHT=LMUOH+28*(IHMUOH-1)
        CALL MUADD(IWADD,IMOD,IPLN,IWIR,IERR)
        IF (MULAYR(IMOD).NE.1) GO TO 40 ! skip BC layer hits
C        IF (ABS(NHT).GT.500.) GO TO 40  ! skip BC layer hits
        ORENT=IABS(IQ(NHT+5))
        IF(ORENT.EQ.3) THEN
          IF(TIMSIN.NE.0) THEN
            NT=IABS(TIMSIN)
            DRIFT=TIMSIN/NT*Q(NHT+14+NT)
            ITOT=ITOT+1
            XHIT(ITOT)=Q(NHT+21)+DRIFT
            YHIT(ITOT)=Q(NHT+22) 
            ZHIT(ITOT)=Q(NHT+23)
            SINE(ITOT)=0.
            COSINE(ITOT)=1.
            NX=NX+1
            WT(ITOT)=WTDRIFT
          END IF
          IF(IPAD.NE.0) THEN
            ITOT=ITOT+1
            IF (IPAD.LT.0) NVER=1
            IF (IPAD.GT.0) NVER=2
            WAVELEN=PADREP*(IABS(IPAD)-1)
            WIRE=Q(NHT+18+NVER)+WAVELEN
            XHIT(ITOT)=Q(NHT+21)
            YHIT(ITOT)=Q(NHT+22)+WIRE
            ZHIT(ITOT)=Q(NHT+23)
            SINE(ITOT)=1.
            COSINE(ITOT)=0.
            NY=NY+1
            WT(ITOT)=WTWIRE
          END IF
        ELSE IF(ORENT.EQ.4) THEN
          IF(TIMSIN.NE.0) THEN
            NT=IABS(TIMSIN)
            DRIFT=TIMSIN/NT*Q(NHT+14+NT)
            ITOT=ITOT+1
            XHIT(ITOT)=Q(NHT+21) 
            YHIT(ITOT)=Q(NHT+22)+DRIFT
            ZHIT(ITOT)=Q(NHT+23)
            SINE(ITOT)=1.
            COSINE(ITOT)=0.
            NY=NY+1
            WT(ITOT)=WTDRIFT
          END IF
          IF(IPAD.NE.0) THEN
            ITOT=ITOT+1
            IF (IPAD.LT.0) NVER=1
            IF (IPAD.GT.0) NVER=2
            WAVELEN=PADREP*(IABS(IPAD)-1)
            WIRE=Q(NHT+18+NVER)+WAVELEN
            XHIT(ITOT)=Q(NHT+21)+WIRE
            YHIT(ITOT)=Q(NHT+22)
            ZHIT(ITOT)=Q(NHT+23)
            SINE(ITOT)=0.
            COSINE(ITOT)=1.
            NX=NX+1
            WT(ITOT)=WTWIRE
          END IF
        ENDIF
   40 CONTINUE
      NTOT=ITOT  
  999 RETURN
      END
