      SUBROUTINE CALICD
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Apply ICD corrections to calorimeter data.
C-            This version uses the same correction constants for all
C-            events. There is one correction constant for each eta
C-            bin in the CC/EC region.
C-   Inputs  : ICD data in CAEP bank
C-   Outputs : ICD energy corrections in the CAEC bank
C-   Controls: 
C-
C-   Created  14-SEP-1989   A.P.White
C-   Updated  12-APR-1990   Serban D. Protopopescu   
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL FIRST
      INTEGER LCAEC,LCAEP,IPSAVE,NPHI,IPOINT,NR,IP,NCH,I,J,IER
      INTEGER GZCAEC,GZCAEP
      INTEGER JJ,KK,ADDR,K
      REAL SLOPE(7,3),DATA,ECOR
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$INC:PTCAEP.INC'
C
      DATA FIRST/.TRUE./
C
C--- Get the correction (slope) constants from RCP file
C
      IF(FIRST) THEN
        CALL EZPICK('CALICD_RCP')
        CALL EZGET('CC_MSGP_SLOPES',SLOPE(1,1),IER)
        CALL EZGET('ICD_SLOPES',SLOPE(1,2),IER)
        CALL EZGET('EC_MSGP_SLOPES',SLOPE(1,3),IER)
        IF(IER.NE.0) GO TO 999
        CALL EZRSET
        FIRST=.FALSE.
      ENDIF
C
C--- Loop over ICD and massless gaps to find number of channels on
C
      NCH=0
      DO 15 K=8,10
      DO 15 I=1,NPHIL
        DO 11 J=-MXETAMGICD,-MNETAMGICD
          IP=PTCAEP(J,I,K)
          IF(IP.NE.0) NCH=NCH+1
   11   CONTINUE
        DO 12 J=MNETAMGICD,MXETAMGICD
          IP=PTCAEP(J,I,K)
          IF(IP.NE.0) NCH=NCH+1
   12   CONTINUE
   15 CONTINUE
C
C--- Book the CAEC bank to save corrections.
C
      CALL BKCAEC(NCH,LCAEC)
      IPSAVE=LCAEC
      LCAEP=GZCAEP()
      NR=IQ(LCAEP+2)
      IQ(LCAEC+3)=NCH
C
C--- Loop over ICD and massless gaps to calculate corrections
C
      DO 50 K=8,10
      DO 50 I=1,NPHIL
C                negative eta loop
        DO 20 J=-MXETAMGICD,-MNETAMGICD
          IP=PTCAEP(J,I,K)
          IF(IP.NE.0) THEN
            IPOINT=LCAEP+(IP-1)*NR
            DATA=Q(IPOINT+5)
C--- Optional cuts on ICD data quality etc. should go here.
            ECOR=DATA/SLOPE(-J-7,K-7)
            IQ(IPSAVE+4)=IQ(IPOINT+4)
            Q(IPSAVE+5)=ECOR
            IPSAVE=IPSAVE+2
          ENDIF
   20   CONTINUE
C                positive eta loop
        DO 30 J=MNETAMGICD,MXETAMGICD
          IP=PTCAEP(J,I,K)
          IF(IP.NE.0) THEN
            IPOINT=LCAEP+(IP-1)*NR
            DATA=Q(IPOINT+5)
C--- Optional cuts on ICD data quality etc. should go here.
            ECOR=DATA/SLOPE(J-7,K-7)
            IQ(IPSAVE+4)=IQ(IPOINT+4)
            Q(IPSAVE+5)=ECOR
            IPSAVE=IPSAVE+2
          ENDIF
   30   CONTINUE
   50 CONTINUE
C----------------------------------------------------------------------
  999 RETURN
      END
