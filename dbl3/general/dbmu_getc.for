C----------------------------------------------------------------------
      SUBROUTINE DBMU_GETC (ICRA,IDAT,ITIM,CHOP,DNAM,DVAL,NVAL,XHOU)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Will, for a given calorimeter crate address
C-    and time, return corresponding temperatures found in DBMON database.
C-    Passed array DNAM and DVAL, should at least have the length of 
C-    N_NEM + N_DEX.
C-    
C-   Inputs  : ICRA (I)    ADC crate number
C-             IDAT (I)    Search date (YYMMDD)
C-             ITIM (I)    Search time (HHMMSS)
C-             CHOP (I)    Not used, but just in case ...
C-
C-   Outputs : DNAM (C*8)  List of names for the returned values
C-             DVAL (R)    List of read values,  (-999. = invalid data)
C-             NVAL (I)    Number of returned names and values
C-                         0 = nothing found for specified time
C-                        -1 = forget it, somthing bad happend
C-             XHOU (R)    Time difference (in hours) between requested
C-                         time and found time.
C-   Controls: 
C-
C-   Created   3-SEP-1992   Lars O. Rasmussen
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INTEGER ICRA,IDAT,ITIM,NVAL
      REAL DVAL(*),XHOU
      CHARACTER*8 DNAM(*)
      CHARACTER*(*) CHOP
C
      INTEGER I,J,K,IRET,VTIM(2),KTIM
      INTEGER DO,TO,ENDV,IBOX,NALL
      REAL XN
      CHARACTER*8 CHLP
C
C.................... and let's hardwire the stuff ....................
C
      INTEGER*2  N_CRA,N_DEV,N_MEN,N_MXP,N_DEX
      REAL XDTH
      PARAMETER (N_CRA=12,    ! number of crates
     &           N_DEV=20,    ! number of devices pr. crate
     &           N_MEN=6,     ! number of mean's to calculate pr. crate
     &           N_DEX=4,     ! number of extra devices (max 20)
     &           N_MXP=13,    ! max+1 number of devices pr. mean
     &           XDTH=2.0)    ! maximum difference between requested time
                              ! and found time, in hours
C
C- Calorimeter devices, CALDEV(1:N_DEV,N_CRA) is used for extra 
C- devices (eg. pulser rack temperatures)
C
      CHARACTER*17 CALDEV(N_DEV,0:N_CRA) 
      DATA (CALDEV(I,0),I=1,N_DEV)/
     & 'CALN_TMP0.PBRT','CALN_TMP0.PBRB','CALN_TMP0.PBLT',
     & 'CALN_TMP0.PBLB','CALN_TMP0.PORT','CALN_TMP0.FBRD',
     & 'CALN_CRB0370.TS02','CALN_CRB0370.TS19','CALN_CRB0371.TS02',
     & 'CALN_CRB0371.TS19','CALN_CRB0372.TS02','CALN_CRB0372.TS19',
     & 'CALN_CRB0373.TS02','CALN_CRB0373.TS19','CALN_CRB0374.TS02',
     & 'CALN_CRB0374.TS19','CALN_CRB0375.TS02','CALN_CRB0375.TS19',
     & 'CALN_CRA037.TE02','CALN_CRA037.TE11'/
      DATA (CALDEV(I,1),I=1,N_DEV)/
     & 'CALN_TMP1.PBRT', 'CALN_TMP1.PBRB','CALN_TMP1.PBLT',
     & 'CALN_TMP1.PBLB', 'CALN_TMP1.PORT','CALN_TMP1.FBRD',
     & 'CALN_CRB0270.TS02','CALN_CRB0270.TS19','CALN_CRB0271.TS02',
     & 'CALN_CRB0271.TS19','CALN_CRB0272.TS02','CALN_CRB0272.TS19',
     & 'CALN_CRB0273.TS02','CALN_CRB0273.TS19','CALN_CRB0274.TS02',
     & 'CALN_CRB0274.TS19','CALN_CRB0275.TS02','CALN_CRB0275.TS19',
     & 'CALN_CRA027.TE02','CALN_CRA027.TE11'/
      DATA (CALDEV(I,2),I=1,N_DEV)/
     & 'CALC_TMP2.PBRT','CALC_TMP2.PBRB','CALC_TMP2.PBLT',
     & 'CALC_TMP2.PBLB','CALC_TMP2.PORT','CALC_TMP2.FBRD',
     & 'CALC_CRB0070.TS02','CALC_CRB0070.TS19','CALC_CRB0071.TS02',
     & 'CALC_CRB0071.TS19','CALC_CRB0072.TS02','CALC_CRB0072.TS19',
     & 'CALC_CRB0073.TS02','CALC_CRB0073.TS19','CALC_CRB0074.TS02',
     & 'CALC_CRB0074.TS19','CALC_CRB0075.TS02','CALC_CRB0075.TS19',
     & 'CALC_CRA007.TE02','CALC_CRA007.TE11'/
      DATA (CALDEV(I,3),I=1,N_DEV)/
     & 'CALC_TMP3.PBRT','CALC_TMP3.PBRB','CALC_TMP3.PBLT',
     & 'CALC_TMP3.PBLB','CALC_TMP3.PORT','CALC_TMP3.FBRD',
     & 'CALC_CRB0180.TS02','CALC_CRB0180.TS19','CALC_CRB0181.TS02',
     & 'CALC_CRB0181.TS19','CALC_CRB0182.TS02','CALC_CRB0182.TS19',
     & 'CALC_CRB0183.TS02','CALC_CRB0183.TS19','CALC_CRB0184.TS02',
     & 'CALC_CRB0184.TS19','CALC_CRB0185.TS02','CALC_CRB0185.TS19',
     & 'CALC_CRA018.TE02','CALC_CRA018.TE11'/
      DATA (CALDEV(I,4),I=1,N_DEV)/
     & 'CALS_TMP4.PBRT','CALS_TMP4.PBRB','CALS_TMP4.PBLT',
     & 'CALS_TMP4.PBLB','CALS_TMP4.PORT','CALS_TMP4.FBRD',
     & 'CALS_CRB0580.TS02','CALS_CRB0580.TS19','CALS_CRB0581.TS02',
     & 'CALS_CRB0581.TS19','CALS_CRB0582.TS02','CALS_CRB0582.TS19',
     & 'CALS_CRB0583.TS02','CALS_CRB0583.TS19','CALS_CRB0584.TS02',
     & 'CALS_CRB0584.TS19','CALS_CRB0585.TS02','CALS_CRB0585.TS19',
     & 'CALS_CRA058.TE02','CALS_CRA058.TE11'/
      DATA (CALDEV(I,5),I=1,N_DEV)/
     & 'CALS_TMP5.PBRT','CALS_TMP5.PBRB','CALS_TMP5.PBLT',
     & 'CALS_TMP5.PBLB','CALS_TMP5.PORT','CALS_TMP5.FBRD',
     & 'CALS_CRB0480.TS02','CALS_CRB0480.TS19','CALS_CRB0481.TS02',
     & 'CALS_CRB0481.TS19','CALS_CRB0482.TS02','CALS_CRB0482.TS19',
     & 'CALS_CRB0483.TS02','CALS_CRB0483.TS19','CALS_CRB0484.TS02',
     & 'CALS_CRB0484.TS19','CALS_CRB0485.TS02','CALS_CRB0485.TS19',
     & 'CALS_CRA048.TE02','CALS_CRA048.TE11'/
      DATA (CALDEV(I,6),I=1,N_DEV)/
     & 'CALS_TMP6.PBRT','CALS_TMP6.PBRB','CALS_TMP6.PBLT',
     & 'CALS_TMP6.PBLB','CALS_TMP6.PORT','CALS_TMP6.FBRD',
     & 'CALS_CRB0380.TS02','CALS_CRB0380.TS19','CALS_CRB0381.TS02',
     & 'CALS_CRB0381.TS19','CALS_CRB0382.TS02','CALS_CRB0382.TS19',
     & 'CALS_CRB0383.TS02','CALS_CRB0383.TS19','CALS_CRB0384.TS02',
     & 'CALS_CRB0384.TS19','CALS_CRB0385.TS02','CALS_CRB0385.TS19',
     & 'CALS_CRA038.TE02','CALS_CRA038.TE11'/
      DATA (CALDEV(I,7),I=1,N_DEV)/
     & 'CALS_TMP7.PBRT','CALS_TMP7.PBRB','CALS_TMP7.PBLT',
     & 'CALS_TMP7.PBLB','CALS_TMP7.PORT','CALS_TMP7.FBRD',
     & 'CALS_CRB0280.TS02','CALS_CRB0280.TS19','CALS_CRB0281.TS02',
     & 'CALS_CRB0281.TS19','CALS_CRB0282.TS02','CALS_CRB0282.TS19',
     & 'CALS_CRB0283.TS02','CALS_CRB0283.TS19','CALS_CRB0284.TS02',
     & 'CALS_CRB0284.TS19','CALS_CRB0285.TS02','CALS_CRB0285.TS19',
     & 'CALS_CRA028.TE02','CALS_CRA028.TE11'/
      DATA (CALDEV(I,8),I=1,N_DEV)/
     & 'CALC_TMP8.PBRT','CALC_TMP8.PBRB','CALC_TMP8.PBLT',
     & 'CALC_TMP8.PBLB','CALC_TMP8.PORT','CALC_TMP8.FBRD',
     & 'CALC_CRB0080.TS02','CALC_CRB0080.TS19','CALC_CRB0081.TS02',
     & 'CALC_CRB0081.TS19','CALC_CRB0082.TS02','CALC_CRB0082.TS19',
     & 'CALC_CRB0083.TS02','CALC_CRB0083.TS19','CALC_CRB0084.TS02',
     & 'CALC_CRB0084.TS19','CALC_CRB0085.TS02','CALC_CRB0085.TS19',
     & 'CALC_CRA008.TE02','CALC_CRA008.TE11'/
      DATA (CALDEV(I,9),I=1,N_DEV)/
     & 'CALC_TMP9.PBRT','CALC_TMP9.PBRB','CALC_TMP9.PBLT',
     & 'CALC_TMP9.PBLB','CALC_TMP9.PORT','CALC_TMP9.FBRD',
     & 'CALC_CRB0170.TS02','CALC_CRB0170.TS19','CALC_CRB0171.TS02',
     & 'CALC_CRB0171.TS19','CALC_CRB0172.TS02','CALC_CRB0172.TS19',
     & 'CALC_CRB0173.TS02','CALC_CRB0173.TS19','CALC_CRB0174.TS02',
     & 'CALC_CRB0174.TS19','CALC_CRB0175.TS02','CALC_CRB0175.TS19',
     & 'CALC_CRA017.TE02','CALC_CRA017.TE11'/
      DATA (CALDEV(I,10),I=1,N_DEV)/
     & 'CALN_TMPA.PBRT','CALN_TMPA.PBRB','CALN_TMPA.PBLT',
     & 'CALN_TMPA.PBLB','CALN_TMPA.PORT','CALN_TMPA.FBRD',
     & 'CALN_CRB0570.TS02','CALN_CRB0570.TS19','CALN_CRB0571.TS02',
     & 'CALN_CRB0571.TS19','CALN_CRB0572.TS02','CALN_CRB0572.TS19',
     & 'CALN_CRB0573.TS02','CALN_CRB0573.TS19','CALN_CRB0574.TS02',
     & 'CALN_CRB0574.TS19','CALN_CRB0575.TS02','CALN_CRB0575.TS19',
     & 'CALN_CRA057.TE02','CALN_CRA057.TE11'/
      DATA (CALDEV(I,11),I=1,N_DEV)/
     & 'CALN_TMPB.PBRT','CALN_TMPB.PBRB','CALN_TMPB.PBLT',
     & 'CALN_TMPB.PBLB','CALN_TMPB.PORT','CALN_TMPB.FBRD',
     & 'CALN_CRB0470.TS02','CALN_CRB0470.TS19','CALN_CRB0471.TS02',
     & 'CALN_CRB0471.TS19','CALN_CRB0472.TS02','CALN_CRB0472.TS19',
     & 'CALN_CRB0473.TS02','CALN_CRB0473.TS19','CALN_CRB0474.TS02',
     & 'CALN_CRB0474.TS19','CALN_CRB0475.TS02','CALN_CRB0475.TS19',
     & 'CALN_CRA047.TE02','CALN_CRA047.TE11'/
      DATA (CALDEV(I,12),I=1,N_DEV)/
     & 'CAL_PLS_TEMP.M03T','CAL_PLS_TEMP.M03B',
     & 'CAL_PLS_TEMP.M04T','CAL_PLS_TEMP.M04B',
     & 16*' '/
C
C- Tag names for calculated means, pr. crate
C
      CHARACTER*5  CALMEN(N_MEN) 
      DATA CALMEN /'PBOXR','PBOXL','PBOXP','PBOXF','BLSCR','ADCCR'/
C
C- Pointers to devices for each mean for each crate
C
      INTEGER*2 PMEN(N_MXP,N_MEN)
      DATA PMEN  /1,2,0,0,0,0,0,0,0,0,0,0,0,
     &            3,4,0,0,0,0,0,0,0,0,0,0,0,
     &            5,0,0,0,0,0,0,0,0,0,0,0,0,
     &            6,0,0,0,0,0,0,0,0,0,0,0,0,
     &            7,8,9,10,11,12,13,14,15,16,17,18,0,
     &            19,20,0,0,0,0,0,0,0,0,0,0,0/
C
C- Tag names for extra devices
C
      CHARACTER*8 CALMENX(N_DEX)
      DATA CALMENX /'PLS_CR3T','PLS_CR3B','PLS_CR4T','PLS_CR4B'/
C
C- Translation between preamp box and adc crate
C
      INTEGER*2 BXTOCR(0:N_CRA-1) 
      DATA BXTOCR /37,27,7,18,58,48,38,28,8,17,57,47/
C......................................................................
C
C- If you wannall, here is a common block
C
      REAL VDEV (N_DEV+N_DEX)
      CHARACTER*17 CDEV(N_DEV+N_DEX)
      COMMON /DBMUGETC/ VDEV,CDEV
C----------------------------------------------------------------------
      NVAL = 0
      DO I = 1,N_DEV+N_DEX
         DVAL(I) = -999.
      END DO
C
C- Get pointer to devices (or preamp box)
C
      DO I = 0,N_CRA-1
         IF (BXTOCR(I) .EQ. ICRA) THEN
            IBOX = I
            GOTO 5
         END IF
      END DO
5     DO I = 1,N_DEV
         CDEV(I) = CALDEV(I,IBOX)
      END DO
      DO I = 1,N_DEX
         CDEV(N_DEV+I) = CALDEV(I,N_CRA)
      END DO
      NALL = N_DEV+N_DEX
C
C- Get the values
C
      CALL DBPKTS(IDAT,ITIM,KTIM)
      CALL D3UUT(VTIM,KTIM)
      CALL DBMU_GETDM ('DBM_CAL',CDEV,NALL,VTIM,1,' ',VDEV,XHOU,IRET)
      IF (IRET .LE. 0) THEN
         NVAL = IRET
         RETURN
      END IF
C
C- Compress the data to some avarage numbers
C
      DO 10 I = 1,N_MEN
         WRITE(CHLP,'(A5,A1,I2.2)') CALMEN(I),'_',ICRA
         DNAM (I) = CHLP
         XN = 0.
         DO J = 1,N_MXP
            IF (PMEN(J,I) .LE. 0) GOTO 10
            XN = XN + 1.
            DVAL(I) = DVAL(I) + (VDEV(PMEN(J,I)) - DVAL(I))/XN
         END DO
10    CONTINUE
      NVAL = N_MEN
      DO I = 1,N_DEX
         J = N_MEN+I
         DNAM (J) = CALMENX(I)
         DVAL(J) = VDEV(N_DEV+I)
      END DO
      NVAL = NVAL + N_DEX
C
999   RETURN
      END
