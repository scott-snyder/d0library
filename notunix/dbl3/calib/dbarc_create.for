      SUBROUTINE DBARC_CREATE(DETECT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Main routine to create DBL3 calibration databases
C-                              and to insert a new path
C-
C-   Inputs  :
C-   Outputs : DETECT
C-   Controls:
C-
C-   Created   1-DEC-1990   Srini Rajagopalan
C-   Updated  15-FEB-1991   Srini Rajagopalan, Corrections for VTX and CDC
C-   Updated  06-MAR-1991   S. Abachi    Renamed from DBCLB_CREATE & modified.
C-   Updated  01-APR-1991   S. Abachi    Dummmy banks made only if asked.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:CALIB.DEF'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:LKCALIB.INC'
      INCLUDE 'D0$INC:DBSTP.INC'
      INCLUDE 'D0$INC:QUEST.INC'
C
      CHARACTER*1 ANS, ANS2
      CHARACTER*3 DETECT,DETARR(7)
      CHARACTER*4 BNKNAM
      CHARACTER*6 KEYNAM(NXKEY)
      CHARACTER*16 CALTYPE(6)
      CHARACTER*11 CALARR(3),CDARR(4),MUOARR(6),SAMARR(2),CLBTYP
      INTEGER MUNMOD
      CHARACTER*48 DBF,DBFIL
      CHARACTER*11 DBFILE
      CHARACTER*25 PATH
      CHARACTER*80 RESULT,MSG
C
      INTEGER OUTNUM,SPT
      INTEGER RETLEN
      INTEGER KEY(NKYS)
      INTEGER NTYPE,MXCRAT,BANKID
      INTEGER FST_TYPE,LST_TYPE,ITYPE,IPTH,LBANK,ICRT
      INTEGER LK,LD
C
      INTEGER DBLEN
      INTEGER ITMLST(4)
      INTEGER*2 LENCOD(2)
      INTEGER STR$TRIM,ISTAT
      EXTERNAL STR$TRIM

      EQUIVALENCE (ITMLST,LENCOD)
      LOGICAL IOK
      EQUIVALENCE (CALIB_LNK(1),LBANK)
C
      DATA KEYNAM /'CRATN','CREAT','QUALT','RUNNO','UPDATE',
     &  'SPARE1','SPARE2','SPARE3'/
      DATA DETARR/'VTX','TRD','CDC','FDC','CAL','MUO','SAM'/
      DATA CALARR/'ALL','PEDESTAL','GAINS'/
      DATA CDARR/'ALL','PEDESTAL','GAINS','TIMES'/
      DATA MUOARR/'ALL','PEDESTAL','GAINS','TIMES','DELTA TIMES',
     &  'DRIFT'/
      DATA SAMARR/'TIMES','MINTIME'/
      DATA CALTYPE/'PEDESTAL','GAINS','TIMES','DTIMES','DRIFT',
     &             'MINTIME'/
C
C&IF  VAXVMS
      INCLUDE '($LNMDEF)'
      INTEGER  SYS$TRNLNM,LIB$SET_LOGICAL
      EXTERNAL SYS$TRNLNM,LIB$SET_LOGICAL
C----------------------------------------------------------------------
C
      IF(CALL_DBEND) CALL DBCLB_FINISH
C
      LENCOD(1) = 80
      LENCOD(2) = LNM$_STRING
      ITMLST(2) = %LOC(RESULT)
      ITMLST(3) = %LOC(RETLEN)
      ITMLST(4) = 0
C&ENDIF
C
C  *** initialization >
C----
      IF(IDVSTP .EQ. 0) THEN
        CALL MZEBRA(0)                  ! Initialize Zebra
        CALL INZSTP                     ! Initialize /ZEBSTP/
        CALL CLBLNK                     ! Initialize Calib link area
      ENDIF
C
      ISTAT=SYS$TRNLNM(,'LNM$PROCESS_TABLE',DBFILE,,ITMLST)
      DBFIL = RESULT(:RETLEN)
      DBFILE = 'DBCALIB$'//DETECT(1:3)
C
C&IF  VAXVMS
      CALL STR$TRIM(DBFIL,DBFIL,DBLEN)
C&ELSE
C&     DBLEN = 48
C&ENDIF
      WRITE(MSG,300) DBFIL
  300 FORMAT(' Default database name (',A<DBLEN>,') > ')
      CALL GETPAR(1,MSG,'U',DBF)
      IF(DBF .NE. ' ') DBFIL = DBF
      ISTAT = LIB$SET_LOGICAL(DBFILE, DBFIL)
      ISTAT=SYS$TRNLNM(,'LNM$PROCESS_TABLE',DBFILE,,ITMLST)
      DBFIL = RESULT(:RETLEN)
      CALL INTMSG(' Database to be created is ')
      WRITE (MSG,100)DBFIL
  100 FORMAT(5X,A)
      CALL INTMSG(MSG)
C
      OUTNUM = 1
      IF (DETECT.EQ.'CAL') THEN
        CALL GETOPT(1,' Select calibration type ',3,CALARR,OUTNUM)
        CLBTYP = CALARR(OUTNUM)
        NTYPE = 2
        MXCRAT = 6
        BANKID = 7
      ELSEIF (DETECT.EQ.'MUO') THEN
        CALL GETOPT(1,' Select calibration type ',6,MUOARR,OUTNUM)
        CLBTYP = MUOARR(OUTNUM)
        NTYPE = 5
        MXCRAT = 164
      ELSEIF (DETECT.EQ.'SAM') THEN
        CALL GETOPT(1,' Select calibration type ',2,SAMARR,OUTNUM)
        IF (OUTNUM.EQ.0) THEN
          CALL INTMSG(' Attempt to create a database aborted ')
          GO TO 999
        ENDIF
        CLBTYP = SAMARR(OUTNUM)
        IF (OUTNUM.EQ.1) OUTNUM = 4
        IF (OUTNUM.EQ.2) OUTNUM = 7
        NTYPE = 2
        MXCRAT = 58
      ELSE
        CALL GETOPT(1,' Select calibration type ',4,CDARR,OUTNUM)
        IF (OUTNUM.EQ.0) THEN
          CALL INTMSG(' Attempt to create a database aborted ')
          GO TO 999
        ENDIF
        CLBTYP = CDARR(OUTNUM)
        NTYPE = 3
        IF ( DETECT .EQ. 'CDC') THEN
          MXCRAT = 6
          BANKID = 4
        ELSEIF ( DETECT .EQ. 'FDC') THEN
          MXCRAT = 12
          BANKID = 5
        ELSEIF ( DETECT .EQ. 'TRD') THEN
          MXCRAT = 8
          BANKID = 6
        ELSEIF ( DETECT .EQ. 'VTX') THEN
          MXCRAT = 10
          BANKID = 3
        ENDIF
      ENDIF
C
      CALL GETPAR(1,' Create a new database [Y] > ','U',ANS)
      IF (ANS.EQ.'N') THEN
        CALL DBCLB_INITIALIZE(DBFILE,'U',IOK)
      ELSE
        CALL GETPAR(1,' Create dummy banks in database [N] > ','U',ANS2)
        IF(ANS2 .EQ. ' ') ANS2 = 'N'
        CALL DBCLB_INITIALIZE(DBFILE,'UZ',IOK)
      ENDIF
      IF(.NOT.IOK) THEN
        CALL ERRDB('DBINIT')
        GO TO 999
      ENDIF
C
      IF (OUTNUM.NE.1) THEN
        FST_TYPE = OUTNUM - 1
        LST_TYPE = OUTNUM - 1
      ELSE
        FST_TYPE = 1
        LST_TYPE = NTYPE
      ENDIF
C
      DO ITYPE = FST_TYPE,LST_TYPE
        CALL DBCLB_PATH(CALTYPE(ITYPE),DETECT,PATH)
        WRITE(MSG,102)CALTYPE(ITYPE)(1:8)
  102   FORMAT(' Path selected is  ',A8)
        CALL INTMSG(MSG)
C
        IF (PATH.EQ.' ') THEN
          WRITE(MSG,103)PATH
  103     FORMAT(' Invalid Path Name, PATH = ',A25)
          CALL INTMSG(MSG)
          GO TO 998
        ENDIF
C
        DO IPTH = 1,2
          CALL RZCDIR(PATH,' ')
          IF (IQUEST(1).EQ.0) THEN
            WRITE(MSG,90)PATH
   90       FORMAT(' Path,',A,
     &        ' already exists in the database. ABORTED')
            CALL INTMSG(MSG)
            GO TO 900
          ENDIF
          CALL DBCRSD(PATH,NXKEY,'IIIIIIII',KEYNAM,0,' ')
          IF (IQUEST(1).NE.0) THEN
            CALL ERRDB('DBCRSD')
            CALL INTMSG(' Error in DBCRSD')
            GO TO 900
          ENDIF
          IF(ANS2 .EQ. 'Y') THEN
            ISTAT = STR$TRIM(PATH,PATH,SPT)
            BNKNAM = PATH(SPT-3:SPT)
            CALL BKDBLD(BNKNAM,LBANK)                ! Book DBLD bank
            WRITE(MSG,105)BNKNAM
  105       FORMAT(' Bank Being Booked is ',A4)
            CALL INTMSG(MSG)
            CALL VZERO(KEY,NKYS)
            KEY(3) = 1                        ! start validity
            KEY(4) = 999999999                ! end validity
            KEY(11) = 0                       ! Run number,not valid
            CALL DBPKTS((IC(LBANK+7)/100 + MOD(IC(LBANK+7),100)*10000),
     &             IC(LBANK+8), KEY(9))
            IF (IQUEST(1).NE.0) THEN
              CALL INTMSG(' DBPKTS:  Error in packing date and time')
              IOK = .FALSE.
              GO TO 999
            ENDIF
C
            DO 10 ICRT = 0,MXCRAT-1
              IF     ( DETECT .EQ. 'MUO' ) THEN
                KEY(8) = MUNMOD(1,ICRT+1)
              ELSEIF ( DETECT .EQ. 'SAM' ) THEN
                KEY(8) = ICRT+400
              ELSE
                KEY(8) = 10*ICRT + BANKID
              ENDIF
              WRITE(MSG,106)KEY(8)
  106         FORMAT(' Crate - ',I3,' being written')
              CALL INTMSG(MSG)
C
              IC(LBANK+9) = ICRT            ! Fill crate number
              CALL DBENTR(PATH,LK,LD,IDVSTP,LBANK,NKYS,KEY,0,'R')
              IF (IQUEST(1).NE.0) THEN
                MSG = ' DBENTR:  Error in entering data in data base'
                CALL INTMSG(MSG)
                GO TO 900
              ENDIF
              IF ( DETECT .EQ. 'CAL' ) THEN
                KEY(8) = KEY(8) + 1
                WRITE(MSG,106)KEY(8)
                CALL INTMSG(MSG)
                IC(LBANK+9) = KEY(8)            ! Fill crate number
                CALL DBENTR(PATH,LK,LD,IDVSTP,LBANK,NKYS,KEY,0,'R')
                IF (IQUEST(1).NE.0) THEN
                  CALL INTMSG(
     &              ' DBENTR:  Error in entering data in data base')
                  GO TO 900
                ENDIF
              ENDIF
   10       CONTINUE
C
            IF (DETECT.EQ.'CAL') THEN
              KEY(8) = 57
              DO ICRT = 1,3
                KEY(8) = KEY(8)+10
                WRITE(MSG,106)KEY(8)
                CALL INTMSG(MSG)
                IC(LBANK+9) = KEY(8)            ! Fill crate number
                CALL DBENTR(PATH,LK,LD,IDVSTP,LBANK,NKYS,KEY,0,'R')
                IF (IQUEST(1).NE.0) THEN
                  MSG = ' DBENTR:  Error in entering data in data base'
                  CALL INTMSG(MSG)
                  GO TO 900
                ENDIF
              ENDDO                         ! ICRT
            ENDIF
C
            CALL INTMSG(' Done with all Crates ')
          ENDIF
          WRITE(MSG,104) DBFILE
  104     FORMAT(' Database located in ',A40)
          CALL INTMSG(MSG)
          IF(IPTH.EQ.1)THEN
            CALL DBCLB_PATH(CALTYPE(ITYPE)//'H',DETECT,PATH)
          ENDIF
  900     CONTINUE
        ENDDO                             ! IPTH
      ENDDO                             ! ITYPE
C
  998 CALL DBCLB_FINISH
  999 CONTINUE
C
      RETURN
      END
