C VAX/DEC CMS REPLACEMENT HISTORY, Element FDFADC_CT.FOR
C *1     4-NOV-1993 10:53:26 AVERY "FDC changes for v12 RECO"
C VAX/DEC CMS REPLACEMENT HISTORY, Element FDFADC_CT.FOR
      SUBROUTINE FDFADC_CT(HALF,UNIT,QUAD,SECTOR,WIRE,
     &                  EXPDAT,NUMCLU,LOCCLU,LENCLU)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return the data for channel HALF,UNIT,QUAD,SECTOR,
C-   WIRE in the array EXPDAT, with Crosstalk corrections, and 
C-   bilinear conversion. Must be called for wire 0 first.
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
      INTEGER W,I,BIN,IPEV 
      INTEGER IPREAD 
      INTEGER CLUS_NUM
      INTEGER IER
      INTEGER MXWIRE,MXCHAN 
      INTEGER TMPADC
      INTEGER LOGCHA
      INTEGER CWIRE,DWIRE
      INTEGER RUNTYPE
      INTEGER NUM_CLUS(0:NBPSEN-1)
      INTEGER LOC_CLUS(MAX_CLUS,0:NBPSEN-1)
      INTEGER LEN_CLUS(MAX_CLUS,0:NBPSEN-1)
      INTEGER MAP(0:255)
      INTEGER FADC(0:LFADC-1,0:NBPSEN-1)
      INTEGER IPEDEST(0:NBPSEN-1) 
C
      REAL BILIRT,BILIPT
      REAL PEDEST
      REAL PEDSIG(0:NBPSEN-1) 
C      REAL CT_COEF(0:NBPSEN-1) 
      REAL THR_CT_RCP, THR_CT
      REAL CROSSTALKT,CROSSTALKP
      REAL CROSSTALK
C
      LOGICAL BILCON
      LOGICAL FADCHK(0:NBPSEN-1)
      LOGICAL FIRST
C
      SAVE CROSSTALKT,CROSSTALKP,CROSSTALK
      SAVE FADC,LEN_CLUS,LOC_CLUS,NUM_CLUS
      SAVE THR_CT_RCP
      SAVE FADCHK
      SAVE BILIPT,BILIRT,RUNTYPE
      SAVE MXWIRE,MXCHAN 
C      SAVE CT_COEF
      LOGICAL DONE
C
C      DATA CT_COEF /0.0,1.0, 14*0.0/
      DATA FADCHK/NBPSEN*.FALSE./
      DATA THR_CT_RCP /10./
      DATA FIRST /.TRUE./
C----------------------------------------------------------------------
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        CALL EZPICK('FTRAKS_RCP')
        CALL EZGET_i('RUNTYPE',RUNTYPE,IER)
        CALL EZGET('BILIPT',BILIPT,IER)
        CALL EZGET('BILIRT',BILIRT,IER)
        CALL EZGET_iarr('MAP(1)',MAP(0),IER)
        CALL EZGET('CROSSTALKP',CROSSTALKP,IER)
        CALL EZGET('CROSSTALKT',CROSSTALKT,IER)
        CALL EZGET('THR_CT',THR_CT_RCP,IER)
C        CALL EZGET('CT_COEF',CT_COEF,IER)
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
      IF ( WIRE .EQ. 0 ) THEN
        IF ( UNIT .EQ.0 ) THEN
          MXCHAN = MXWIRT+2
          MXWIRE = MXWIRT
          CROSSTALK = CROSSTALKT
        ELSE
          MXCHAN = MXWIRP
          MXWIRE = MXWIRP
          CROSSTALK = CROSSTALKP
        ENDIF
        DO W =  0, MXCHAN
          IF (RUNTYPE.NE.-1) THEN 
            CALL FCODER(LOGCHA,HALF,UNIT,QUAD,SECTOR,W,0,2)
          ELSE
            CALL FCODER_MCFIX(LOGCHA,HALF,UNIT,QUAD,SECTOR,W)
          ENDIF
          IF (FADCHK(W)) CALL VZERO( FADC(0,W), LFADC )
          CALL FDUNPK( LOGCHA, EXPDAT(0), FADCHK(W) )
          IF ( .NOT. FADCHK(W) ) THEN
            NUM_CLUS(W) = 0
          ELSE
            CALL FGTLPD(HALF,UNIT,QUAD,SECTOR,W,PEDEST,PEDSIG(W))
            IPEDEST(W) = NINT(PEDEST)
C                                                  
            BILCON = .TRUE.
C
C **** D0 Hall data using bilinear amps on Theta SW only
            IF (RUNTYPE.EQ.1) THEN
              IF( W .GT. MXWIRT.OR.UNIT.EQ.1 ) BILCON = .FALSE.
            ENDIF
C
            IPREAD = 0
            CLUS_NUM = 0
            DONE = .FALSE.
            DO WHILE (.NOT. DONE)
              CLUS_NUM = CLUS_NUM + 1
              LEN_CLUS(CLUS_NUM,W) = EXPDAT(IPREAD)
              IF (LEN_CLUS(CLUS_NUM,W) .EQ. 0) THEN
                LEN_CLUS(CLUS_NUM,W) = 0
                CLUS_NUM = CLUS_NUM - 1
                DONE = .TRUE.
              ELSEIF ( CLUS_NUM .GE. MAX_CLUS ) THEN
                LEN_CLUS(CLUS_NUM,W) = 0
                CLUS_NUM = MAX_CLUS 
                DONE = .TRUE.
                CALL ERRMSG('FDPULS-MAXCLUS','FDPULS',
     &            'Too many clusters on wire','I')
              ELSE
                LOC_CLUS(CLUS_NUM,W)  = EXPDAT(IPREAD+1)
                IPEV = IPREAD + 1
C
C ****  Perform bilinear conversion
C
                BIN = LOC_CLUS(CLUS_NUM,W)-1
                DO I = 1, LEN_CLUS(CLUS_NUM,W)
                  TMPADC = EXPDAT(IPEV+I) - IPEDEST(W)
                  IF ( BILCON .AND. (TMPADC.GT.0) )
     &              TMPADC = MAP(TMPADC)
                  BIN = BIN + 1
                  IF ( BIN .LE. (LFADC-1)  ) THEN
                    FADC(BIN,W) = TMPADC
                  ELSE
                    DONE = .TRUE.
                  ENDIF
                END DO
                IPREAD = IPREAD + LEN_CLUS(CLUS_NUM,W) +2
                IF ( IPREAD .GE. LFADC ) THEN
                  IPREAD = LFADC
                  DONE = .TRUE.
                ENDIF
              ENDIF
            END DO
            NUM_CLUS(W) = CLUS_NUM 
          ENDIF
        ENDDO
      ENDIF
C
      NUMCLU = NUM_CLUS(WIRE) 
      IF ( NUMCLU .EQ. 0) GOTO 999
C
      IF ( CROSSTALK .NE. 0. ) THEN
        THR_CT = MAX( THR_CT_RCP*PEDSIG(WIRE), THR_CT_RCP ) 
      ENDIF
C
      DO CLUS_NUM =  1, NUM_CLUS(WIRE) 
        LOCCLU(CLUS_NUM) = LOC_CLUS(CLUS_NUM,WIRE)
        LENCLU(CLUS_NUM) = LEN_CLUS(CLUS_NUM,WIRE)
        BIN = LOCCLU(CLUS_NUM)-1
        DO I = 1, LENCLU(CLUS_NUM) 
          BIN = BIN + 1
          EXPDAT(BIN) = FADC(BIN,WIRE) + IPEDEST(WIRE)
C
C ****  Perform CROSSTALK CORRECTION if necessary
C
          IF ( CROSSTALK .NE. 0. ) THEN
            IF ( (WIRE.GE.1)  .AND. (WIRE.LE. MXWIRE) ) THEN
              IF ( FADC(BIN, WIRE-1) .GT. THR_CT ) THEN
                EXPDAT(BIN) = EXPDAT(BIN)
     &       - NINT( CROSSTALK * FADC(BIN, WIRE-1) )
              ENDIF
            ENDIF
            IF ( WIRE.LE. MXWIRE - 1 ) THEN
              IF ( FADC(BIN, WIRE+1) .GT. THR_CT ) THEN
                EXPDAT(BIN) = EXPDAT(BIN)
     &       - NINT( CROSSTALK * FADC(BIN, WIRE+1) )
              ENDIF
            ENDIF
          ENDIF
C
C          IF ( CROSSTALK .NE. 0. ) THEN
C            DO  CWIRE =  0,MXWIRE
C              DWIRE = ABS(WIRE-CWIRE) 
C              IF ( CT_COEF(DWIRE) .NE. 0.0 ) THEN
C                IF ( FADC(BIN, CWIRE) .GT. THR_CT ) THEN
C                  EXPDAT(BIN) = EXPDAT(BIN)
C     &              - NINT( CROSSTALK * CT_COEF( DWIRE ) 
C     &                   *FADC(BIN, CWIRE)  )
C                ENDIF
C              ENDIF
C            END DO
C          ENDIF
C
        END DO
      ENDDO
C
  999 CONTINUE
      RETURN
      END
