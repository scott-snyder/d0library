      SUBROUTINE FDFADC(HALF,UNIT,QUAD,SECTOR,WIRE,
     &                  EXPDAT,NUMCLU,LOCCLU,LENCLU)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return the data for channel HALF,UNIT,QUAD,SECTOR,
C-   WIRE in the array EXPDAT. Simplified version that does not do 
C-   crosstalk corrections (for greater speed).
C-   
C-
C-   Inputs  : HALF,UNIT,QUAD,SECTOR,WIRE       Channel to be unpacked.
C-   Outputs :  EXPDAT(bin)     Array containing data (spread out).
C-              NUMCLU          Number of clusters in channel.
C-              LOCCLU(cluster) Location in EXPDAT of cluster.
C-              LENCLU(cluster) Number of bins for cluster.     
C-
C-   Created  14-JUL-1991   Robert E. Avery
C-   Updated  15-MAY-1992   Susan K. Blessing  Remove FDEVNT.INC. 
C-   Updated  22-JUN-1992   Susan K. Blessing  Stop doubling DL FADC
C-    values.
C-   Updated   1-FEB-1993   Robert E. Avery  Special call to  
C-              FCODER_MCFIX for corrupted MC data.
C-              (generated by D0GEANT, circa  Nov. 1992)
C-   Updated  28-OCT-1993   Robert E. Avery  Non-Crosstalk version. 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$PARAMS:FDPARA.PARAMS/LIST'
C Input:
      INTEGER HALF,UNIT,QUAD,SECTOR,WIRE
C Output:
      INTEGER EXPDAT(0:LFADC-1)
      INTEGER NUMCLU
      INTEGER LOCCLU(*),LENCLU(*)
C Local:
      INTEGER I,BIN,IPEV 
      INTEGER IPREAD 
      INTEGER CLUS_NUM
      INTEGER IER
      INTEGER TMPADC
      INTEGER LOGCHA
      INTEGER RUNTYPE
      INTEGER MAP(0:255)
      INTEGER IPEDEST
      INTEGER EXPDAT_IN(0:LFADC-1)
C
      REAL BILIRT,BILIPT
      REAL PEDEST
      REAL PEDSIG
C
      LOGICAL FADCHK
      LOGICAL FIRST
C
      SAVE FADCHK
      SAVE BILIPT,BILIRT,RUNTYPE
      LOGICAL DONE
C
      DATA FADCHK /.FALSE./
      DATA FIRST /.TRUE./
C----------------------------------------------------------------------
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        CALL EZPICK('FTRAKS_RCP')
        CALL EZGET('RUNTYPE',RUNTYPE,IER)
        CALL EZGET('BILIPT',BILIPT,IER)
        CALL EZGET('BILIRT',BILIRT,IER)
        CALL EZGET('MAP(1)',MAP(0),IER)
        CALL EZRSET
        IF (RUNTYPE.LE.0) THEN
          DO BIN=0,BILIPT
            MAP(BIN) = BIN
          ENDDO
          DO BIN=BILIPT+1,255
            MAP(BIN) = (BIN-BILIPT) * BILIRT + BILIPT
          ENDDO
        ENDIF
      ENDIF
C
C ****  Unpack entire cell in order to do crosstalk correction
C ****  Get CDDn bank FADC data for each wire
C
      IF (RUNTYPE.NE.-1) THEN 
        CALL FCODER(LOGCHA,HALF,UNIT,QUAD,SECTOR,WIRE,0,2)
      ELSE
        CALL FCODER_MCFIX(LOGCHA,HALF,UNIT,QUAD,SECTOR,WIRE)
      ENDIF
C      IF (FADCHK) CALL VZERO( EXPDAT, LFADC )
      CALL FDUNPK( LOGCHA, EXPDAT_IN, FADCHK )
      IF ( .NOT. FADCHK ) THEN
        NUMCLU = 0
      ELSE
        CALL FGTLPD(HALF,UNIT,QUAD,SECTOR,WIRE,PEDEST,PEDSIG)
        IPEDEST = NINT(PEDEST)
C                                                  
        IPREAD = 0
        CLUS_NUM = 0
        DONE = .FALSE.
        DO WHILE (.NOT. DONE)
          CLUS_NUM = CLUS_NUM + 1
          LENCLU(CLUS_NUM) = EXPDAT_IN(IPREAD)
          IF (LENCLU(CLUS_NUM) .EQ. 0) THEN
            LENCLU(CLUS_NUM) = 0
            CLUS_NUM = CLUS_NUM - 1
            DONE = .TRUE.
          ELSEIF ( CLUS_NUM .GE. MAX_CLUS ) THEN
            LENCLU(CLUS_NUM) = 0
            CLUS_NUM = MAX_CLUS 
            DONE = .TRUE.
            CALL ERRMSG('FDPULS-MAXCLUS','FDPULS',
     &            'Too many clusters on wire','I')
          ELSE
            LOCCLU(CLUS_NUM)  = EXPDAT_IN(IPREAD+1)
            IPEV = IPREAD + 1
C
C ****  Perform bilinear conversion
C
            BIN = LOCCLU(CLUS_NUM)-1
            DO I = 1, LENCLU(CLUS_NUM)
              TMPADC = EXPDAT_IN(IPEV+I) - IPEDEST
              IF ( TMPADC.GT.0 )
     &              TMPADC = MAP(TMPADC)
              BIN = BIN + 1
              IF ( BIN .LE. (LFADC-1)  ) THEN
                EXPDAT(BIN) = TMPADC + IPEDEST
              ELSE
                DONE = .TRUE.
              ENDIF
            END DO
            IPREAD = IPREAD + LENCLU(CLUS_NUM) +2
            IF ( IPREAD .GE. LFADC ) THEN
              IPREAD = LFADC
              DONE = .TRUE.
            ENDIF
          ENDIF
        END DO
        NUMCLU = CLUS_NUM 
      ENDIF
C
  999 CONTINUE
      RETURN
      END