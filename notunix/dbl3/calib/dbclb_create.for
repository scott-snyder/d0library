      PROGRAM DBCLB_CREATE
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Routine to create DBL3 calibration databases
C-                              and to insert a new path
C-
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created   1-DEC-1990   Srini Rajagopalan
C-   Updated  15-FEB-1991   Srini Rajagopalan, Corrections for VTX and CDC
C-   Updated  30-APR-1991   S. Abachi, Made into a subroutine
C-   Updated     MAY-1991   Jan Guida  Allow user to abort (from getopt)
C-   updated     Feb-1992   J.Green made back to program, added SAMUS
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:CALIB.DEF'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:LKCALIB.INC'
      INCLUDE 'D0$INC:DBSTP.INC'
      INCLUDE 'D0$INC:QUEST.INC'
C
      CHARACTER*1 ANS,ANS2
      CHARACTER*3 DETECT,DETARR(7)
      CHARACTER*4 BNKNAM
      CHARACTER*6 KEYNAM(NXKEY)
      CHARACTER*16 CALTYPE(6)
      CHARACTER*11 CALARR(3),CDARR(4),MUOARR(6),SAMARR(2),CLBTYP
      CHARACTER*11 DBFILE
      CHARACTER*25 PATH
      CHARACTER*48 DBLOG
      CHARACTER*80 RESULT,MSG
C
      INTEGER OUTNUM,SPT
      INTEGER RETLEN
      INTEGER KEY(NKYS)
      INTEGER NTYPE,MXCRAT,BANKID
      INTEGER FST_TYPE,LST_TYPE,ITYPE,IPTH,LBANK,ICRT
      INTEGER LK,LD
      INTEGER MUNMOD    ! function to get muon mod number
C
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
      DATA MUOARR/'ALL','PEDESTAL','GAINS','TIMES','DELTA TIME','DRIFT'/
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
      LENCOD(1) = 80
      LENCOD(2) = LNM$_STRING
      ITMLST(2) = %LOC(RESULT)
      ITMLST(3) = %LOC(RETLEN)
      ITMLST(4) = 0
C&ENDIF
C
      CALL SETCHK
      CALL MENSET('MENUDEF')
      CALL SETLIN(16)
      CALL SPLTIT
C
C  *** initialization >
C----
      CALL MZEBRA(0)                  ! Initialize Zebra
      CALL INZSTP                     ! Initialize /ZEBSTP/
      CALL CLBLNK                     ! Initialize Calib link area
C
C *** Get detector information >
C
      CALL GETOPT(1,' Select detector type ',7,DETARR,OUTNUM)
      IF (OUTNUM.EQ.0) THEN
        CALL INTMSG(' Attempt to create database aborted ')
        GO TO 999
      ENDIF
      DETECT = DETARR(OUTNUM)
      DBFILE = 'DBCALIB$'//DETECT
      WRITE(MSG,101)DETECT
  101 FORMAT(' Detector selected is ',A3)
      CALL INTMSG(MSG)
C
C *** Check to see if DBCALIB$XXX logical is set, if not set it
C
C&IF  VAXVMS
      ISTAT=SYS$TRNLNM(,'LNM$PROCESS_TABLE',DBFILE,,ITMLST)
      DBLOG = RESULT(:RETLEN)
      IF (DBLOG.EQ.' ') THEN
        WRITE(MSG,110)DBFILE
  110   FORMAT(' Logical ',A,' not defined, will use default value')
        CALL INTMSG(MSG)
        DBLOG = '[]'//DBFILE
        ISTAT = LIB$SET_LOGICAL(DBFILE,DBLOG)
      ENDIF
C&ELSE
C&    CALL INTMSG(' DBL3 logicals not available on Non-VAX machines ')
C&    CALL INTMSG(' Set them before executing ')
C&ENDIF
      CALL INTMSG(' DBL3 database will be created in file')
      WRITE (MSG,100)DBLOG
  100 FORMAT(5X,A)
      CALL INTMSG(MSG)
C
      OUTNUM = 0
      IF (DETECT.EQ.'CAL') THEN
        CALL GETOPT(1,' Select calibration type ',3,CALARR,OUTNUM)
        IF (OUTNUM.EQ.0) THEN
          CALL INTMSG(' Attempt to create a database aborted ')
          GO TO 999
        ENDIF
        CLBTYP = CALARR(OUTNUM)
        NTYPE = 2
        MXCRAT = 6
        BANKID = 7
      ELSEIF (DETECT.EQ.'MUO') THEN
        CALL GETOPT(1,' Select calibration type ',6,MUOARR,OUTNUM)
        IF (OUTNUM.EQ.0) THEN
          CALL INTMSG(' Attempt to create a database aborted ')
          GO TO 999
        ENDIF
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
          WRITE(MSG,104)DBFILE
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
      CALL EXIMEN (1,1)
C
      STOP
      END
