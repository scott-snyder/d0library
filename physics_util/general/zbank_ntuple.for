      LOGICAL FUNCTION ZBANK_NTUPLE ()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fill HBOOK NTUPLES from zebra bank.
C-
C-   Inputs  : none
C-   Outputs :
C-   Controls:
C-
C-   Created 24-NOV-1992   Chip Stewart
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL ZBANK_HBOOKN,ZBANK_END,FIRST
      CHARACTER PARAM*80,HBOOK_FILE*132,TOPDIR*50,PAWCDIR*50,FILEDIR*80
      CHARACTER LABEL(50)*80,MSG*100
      INTEGER ID,NLAB,LENF
      INTEGER MAXL(50),NXL(50)
      INTEGER LUNH,ICYCLE
C----------------------------------------------------------------------
      INCLUDE 'D0$OFFLINE_UTIL$GENERAL:VALUE.DEF'
C----------------------------------------------------------------------
      INTEGER I,J,K,L,STATUS,NVALS,II,JJ,KK,LOC
      INTEGER LENGTH,LTITLE,NWORDS,NPRIME,LP
C
      INTEGER MAXSIZE,MAXIDS,MAXDIM
      PARAMETER( MAXSIZE = 20000 )
      PARAMETER( MAXIDS  = 5000  )      ! Maximum number of IDs to store
      PARAMETER( MAXDIM  = 800   )      ! Maximum number of tags
C
      LOGICAL RW, BOOK, ZBANK_VAL,OK
      INTEGER IXT(MAXSIZE),TYPES(MAXSIZE)
      INTEGER NIDD,LL,IER,NL,IARRAY(MAXSIZE)
      REAL    XT1(MAXSIZE),XT(MAXSIZE),RARRAY(MAXSIZE)
      EQUIVALENCE(IXT,XT)
      EQUIVALENCE(IARRAY,RARRAY)
C
      CHARACTER*80 TAGS(MAXDIM)
      CHARACTER*32 CHRZPA,TOK(MAXDIM)
      CHARACTER*80 CHTITL,REMARK
      CHARACTER*800 CHFORM
      SAVE NLAB,LABEL,MAXL,NXL,FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      ZBANK_NTUPLE = .FALSE.
      IF(FIRST) GOTO 999
      LOC = 1
      DO I = 1, NLAB
        OK  = ZBANK_VAL (LABEL(I),NXL(I),NL,XT1)  ! XT1(1:NXL(I),1:NL)
        IF(NL.GT.MAXL(I)) THEN
          WRITE(MSG,'(A10,A20,I3)')'TRUNCATE ',LABEL(I),NL
          CALL ERRMSG('LINK COUNT BAD','ZBANK_NTUPLE',MSG,'W')
          NL = MAXL(I)
        END IF
        IF(MAXL(I).GT.1) THEN  ! variable length ntuple
          IXT(LOC) = NL
          LOC = LOC + 1
          DO K = 1 , NXL(I)
            DO J = 1, MAXL(I)
              IF((K.LE.NXL(I)).AND.(J.LE.NL)) THEN
                L = (J-1)*NXL(I)+K
                CALL UCOPY(XT1(L),XT(LOC),1)
              END IF
              LOC = LOC + 1
            END DO
          END DO
        ELSE
          CALL UCOPY(XT1(1),XT(LOC),NXL(I))
          LOC = LOC + NXL(I)
        END IF
      END DO
      CALL HFNT(ID)
      ZBANK_NTUPLE = .TRUE.
  999 RETURN
C----------------------------------------------------------------------
      ENTRY ZBANK_HBOOKN
C----------------------------------------------------------------------
      ZBANK_HBOOKN = .TRUE.
      IF(.NOT.FIRST) GOTO 1999
      FIRST = .FALSE.
      ZBANK_HBOOKN = .FALSE.
      CALL INRCP('NTUPLE_RCP',IER)
      CALL EZPICK('NTUPLE_RCP')              ! select CAHITS bank
      CALL EZERR(IER)
      IF(IER.NE.0) THEN
        CALL ERRMSG('NO_NTUPLE_RCP','NTUPLE',
     &    ' NTUPLE_RCP bank no found.','W')
      END IF
      CALL EZGETS('HBOOK_FILE',1,HBOOK_FILE,LENF,IER)
      CALL WORD(HBOOK_FILE,I,J,K)
      CALL TRNLNM(HBOOK_FILE(1:K),TOPDIR,K)
      IF(K.GT.0) THEN
        HBOOK_FILE = TOPDIR
      ELSE
        K = LENF
      END IF
      CALL EZGETS('HBOOK_DIRECTORY',1,TOPDIR,LENF,IER)
      FILEDIR = '//' // TOPDIR(1:LENF)
      PAWCDIR = '//PAWC/' // TOPDIR(1:LENF)
      MSG = ' NTUPLE FILE '//HBOOK_FILE(1:K)
      CALL INTMSG(MSG)
      CALL GTUNIT(77,LUNH,IER)
      CALL HROPEN(LUNH,TOPDIR,HBOOK_FILE,'N',1024,IER)
      CALL HMDIR(PAWCDIR,'S')
      CALL HCDIR(FILEDIR,' ')
      CALL DHSHOW
C-
      PARAM = 'NTUPLE_LIST'
      LP = LEN(PARAM)
C
C ****  Load array from current rcp bank
C
      CALL EZGET_VALUE_TYPE ( PARAM(1:LP),IARRAY,TYPES,NVALS,STATUS )
    6 IF ( STATUS.NE.0 ) THEN
        CALL ERRMSG('zbank_HBOOKN','EZGET_VALUE_TYPE',
     &    'ERROR IN HBOOK RCP FILE','W')
        GOTO 999
      ENDIF
C
      NIDD = 0                          ! Number of booked histograms
      II   = 0
      DO WHILE ( II.LT.NVALS )
C
        II = II + 1
C
C ****  Check for ON/OFF switch
C
        BOOK = .TRUE.
        IF ( TYPES(II) .EQ. VTLOG ) THEN
          BOOK = IARRAY(II) .NE. 0
          II = II + 1
        ENDIF
C
C ****  NTUPLE ID
C
        ID = IARRAY(II)
        II = II + 1
C
C ****  HISTOGRAM TITLE
C
        IF ( TYPES(II) .LT. VTCHR ) THEN
          REMARK = ' Error in specs/Param: '//PARAM(1:LP)
          CALL ERRMSG('BAD_SYNTAX','ZBANK_HBOOKN',REMARK,'W')
          GOTO 999
        ENDIF
        LTITLE = TYPES(II) - VTCHR
        NWORDS = 1 + ( LTITLE - 1 )/4
        CHTITL = ' '
        CALL DHTOC(LTITLE,IARRAY(II),CHTITL)
        II = II + NWORDS
C
        IF ( TYPES(II) .LT. VTCHR ) THEN
          REMARK = ' Error in NTUPLE specs/Param: '//PARAM(1:LP)
          CALL ERRMSG('BAD_SYNTAX','ZBANK_HBOOKN',REMARK,'W')
          GOTO 999
        ENDIF
C
        LENGTH = TYPES(II) - VTCHR
        NWORDS = 1 + ( LENGTH - 1 )/4
        CHRZPA = ' '
        CALL DHTOC(LENGTH,IARRAY(II),CHRZPA)
        II = II + NWORDS
C
C ****  Get allocation
C
        IF ( TYPES(II) .EQ. VTINT ) THEN
          NPRIME = IARRAY(II)
          II = II + 1
        ELSE
          NPRIME = 1024
        ENDIF
        RW = .FALSE.
        IF(NPRIME.LT.0) THEN
          RW = .TRUE.
          NPRIME = -NPRIME
        ELSEIF ( NPRIME .EQ. 0 ) THEN
          NPRIME = 1024
        END IF
C
C ****  Get tags
C
        NLAB = 0
        DO WHILE (( TYPES(II) .GT. VTCHR ) .AND. (II.LE.NVALS))
          NLAB = NLAB + 1
          LENGTH = TYPES(II) - 10
          NWORDS = 1 + ( LENGTH - 1 )/4
          LABEL(NLAB) = ' '
          TAGS(NLAB) = ' '
          CALL DHTOC(LENGTH,IARRAY(II),LABEL(NLAB))
          II = II + NWORDS
          CALL EZGETS (LABEL(NLAB),1,TAGS(NLAB),L,IER)
          CALL HNTTOK(TAGS(NLAB), TOK, NXL(NLAB), 50,ier)
          IF (IER .EQ. 1) THEN
            WRITE(MSG,'(A7,A20,I3)')'BAD ',TAGS(NLAB),NXL(NLAB)
            CALL ERRMSG('HNTTOK UNHAPPY','ZBANK_HBOOKN',MSG,'W')
            goto 999
          END IF
          K = INDEX(TAGS(NLAB),',')
          L = INDEX(TAGS(NLAB),']')
          IF(((K*L).GT.0).and.(K.LT.L)) THEN
            READ(TAGS(NLAB)(K+1:L-1),*)MAXL(NLAB)
          ELSE
            MAXL(NLAB) = 1
          END IF
          IF(MAXL(NLAB).GT.1) THEN
            NXL(NLAB) = NXL(NLAB) - 1
          END IF
          OK  = ZBANK_VAL (LABEL(NLAB),NXL(I),NL,XT1)
          IF(.NOT.OK) THEN
            WRITE(MSG,'(A7,A20,I3)')'QUIT ',LABEL(I),NL
            CALL ERRMSG('ZBANK_VAL BAD','ZBANK_NTUPLE',MSG,'W')
            GOTO 999
          ENDIF
          IF(NL.GT.MAXL(NLAB)) THEN
            WRITE(MSG,'(A6,A20,I3)')'BAD ',LABEL(I),NL
            CALL ERRMSG('LINK COUNT BAD','ZBANK_NTUPLE',MSG,'W')
            CALL ERRMSG('TAG LIMIT BAD','ZBANK_HBOOKN','BAD SIGN','W')
            GOTO 999
          END IF
        ENDDO
   99   CONTINUE
C
        IF ( BOOK ) THEN
          IF(RW) THEN
            CALL HBOOKN (ID,CHTITL(1:LTITLE),NLAB,CHRZPA,NPRIME,TAGS)
          ELSE
            CALL HBNT(ID,CHTITL(1:LTITLE),' ')
            CALL HBSET('BSIZE',NPRIME,STATUS)
            IF ( STATUS .NE. 0 ) THEN
              CALL ERRMSG('HBSET_ERROR','DO_HBOOKN',
     &              'Error setting buffer size','F')
            ENDIF
            CHFORM = ' '
            L = 1
            LL = 0
            DO I = 1, NLAB
              CALL WORD(TAGS(I),KK,JJ,K)
              CHFORM=CHFORM(1:L)//TAGS(I)(1:K)//','
              L = L + K + 1
              IF((I.GT.0).AND.(MOD(I,50).EQ.0)) THEN
                LL = I - 49
                L=L-1
                CALL HBNAME(ID,CHTITL(1:LTITLE),XT(LL),CHFORM(2:L) )
                CHFORM = ' '
                L = 1
              END IF
            END DO
            LL = (NLAB/50) * 50 + 1
            L = L -1
            CALL HBNAME(ID,CHTITL(1:LTITLE),XT(1),CHFORM(2:L))
          END IF
        ENDIF
        II = II + 1000 ! NO MORE NTUPLES  - OLD II = II - 1   ! Backspace
C
      ENDDO
      CALL EZRSET
      ZBANK_HBOOKN = .TRUE.
 1999 RETURN

      ENTRY ZBANK_END 
C----------------------------------------------------------------------
C-
      CALL HCDIR(PAWCDIR,' ')
      CALL HCDIR(FILEDIR,' ')
C
      CALL HROUT(0,ICYCLE,' ')
      CALL HREND(TOPDIR)
      CLOSE(LUNH)
      CALL RLUNIT(77,LUNH,IER)
      CALL INTMSG(' NTUPLE STORED IN '//HBOOK_FILE)
      ZBANK_END = .TRUE.
 3999 RETURN
      END
