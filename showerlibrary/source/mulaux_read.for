      SUBROUTINE MULAUX_READ(IUNIT_AUX,NREC,NTOT_REC,FILNAM,RCYCLES,
     &  NO_RCYCLES,NDC,NRCYCLES,NTOT_CYCLES,CYC)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : READ CYCLES ARRAY
C-   AND INPUT FILE NAME TO AUXILLIARY  FILE
C-
C-   Inputs  :IUNIT_AUX = Unit to read from
C-            NREC = Record number in Auxillary file
C-   Outputs :NTOT_REC = total number of records in auxillary file
C-            FILNAM = Input file name
C-            RCYCLES = Cycles array
C-            NDC     = length of RCYCLES
C-            NRCYCLES = CUMULATIVE CYCLE COUNT (TRACKS) IN RCYCLES
C-            NTOT_CYCLES = TOTAL NUMBER OF CYCLES
C-            CYC = .TRUE. , WILL READ CYCLES INFO
C-   Controls:
C-
C-   Created   1-MAY-1990   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C      INCLUDE 'D0$INC:SHLCON.INC'
      INCLUDE 'D0$INC:MULCON.INC'
      CHARACTER*(*) FILNAM
      INTEGER IUNIT_AUX
      INTEGER NREC,NTOT_REC,RCYCLES(*),NTOT_CYCLES
      INTEGER NO_RCYCLES(*)
      INTEGER PRTUN,SSUNIT
      INTEGER NDC,K,NRCYCLES
      INTEGER IOSTAT,IREC
      INTEGER SREC
      LOGICAL CYC
      LOGICAL FIRST
      DATA FIRST/.TRUE./
      INTEGER SRECL
      INTEGER NREAD_CYC,REC_LEFT
      INTEGER IER,KLO
C----------------------------------------------------------------------
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        CALL EZPICK('MUONLIBRARY_RCP')
        CALL EZGET('MUONLIBRARY_AUX_RECL',SRECL,IER)
        CALL EZRSET
        SRECL = 0.45*SRECL               ! USE ONLY 45% FOR MUON HIT LIB.
        NREAD_CYC = NDATA_CYCLES/SRECL
        REC_LEFT = NDATA_CYCLES - NREAD_CYC*SRECL
      ENDIF
      IREC = 1
      IF ( CYC ) THEN
        KLO = 1
        DO IREC = 1,NREAD_CYC
          READ(KEY=IREC,UNIT=IUNIT_AUX,ERR=998,IOSTAT=IOSTAT)
     &      SREC,(RCYCLES(KLO+K-1),NO_RCYCLES(KLO+K-1),K=1,SRECL)
          IF(IREC.NE.SREC)GO TO 997
          KLO = KLO + SRECL
        ENDDO
        IREC = NREAD_CYC
        IF ( REC_LEFT.GT.0 ) THEN
          IREC = IREC + 1
          READ(KEY=IREC,UNIT=IUNIT_AUX,ERR=998,IOSTAT=IOSTAT)
     &    SREC,(RCYCLES(KLO+K-1),NO_RCYCLES(KLO+K-1),K=1,REC_LEFT)
          IF(IREC.NE.SREC)GO TO 997
          KLO = KLO + REC_LEFT
        ENDIF
      ELSE
        IREC = NREAD_CYC
        IF(REC_LEFT.GT.0)IREC = IREC + 1
      ENDIF
C
      IREC = IREC + 1
      READ(KEY=IREC,UNIT=IUNIT_AUX,ERR=998,IOSTAT=IOSTAT)
     &  SREC,NTOT_REC,NTOT_CYCLES,NDC
      IF(IREC.NE.SREC)GO TO 997
C
      IREC = IREC+NREC
      READ(KEY=IREC,UNIT=IUNIT_AUX,ERR=998,IOSTAT=IOSTAT)
     &  SREC,NRCYCLES,FILNAM
      IF(IREC.NE.SREC)GO TO 997
C
      RETURN
  997 CONTINUE
      CALL ERRMSG('MUONLIBRARY','SHLAUX_READ',
     &  ' SREC DOES NOT MATCH IREC ','W')
      RETURN
  998 CALL ERRMSG('MUONLIBRARY','SHLAUX_READ',
     &  'ERROR READING AUXILLARY FILE','W')
C
  999 RETURN
      END
