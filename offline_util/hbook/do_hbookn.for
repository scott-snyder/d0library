      SUBROUTINE DO_HBOOKN(PARAM)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book HBOOK ntuples from rcp bank.
C-
C-   Inputs  : PARAM    [C*]    NAME OF ARRAY PARAMETER
C-   Outputs :
C-   Controls:
C-     The RCP bank containing PARAM must be EZPICKed before calling DO_HBOOKN.
C-     The array PARAM contains arguments to HBOOK similar to DO_HBOOK
C-     except this routine can do CW ntuples and does not do HCDIR
C-     directory book-keeping (HCDIR is 'user' controlled). 
C-     Array contents: ID title top-directory record-length tag-list
C-     example:
C-       \ARRAY NTUPLES_ESUM
C-         202 'ESUM 2' ' ' 1024
C-         'NJ[0,20]' 'PT(NJ):R' 'ETA(NJ)' 'W' 
C-         'NTT[0,25]' 'TTPT(NTT):R' 'TETA(NTT):R'
C-       \END
C-    In this example, the ID is 202, the ntuple is named 'ESUM 2' and the 
C-    record length is 1024. The Tags are NJ which can run from 0 to 20,
C-    PT and ETA which are indexed in NJ, W, NTT from 0 to 25 which is 
C-    the index for TTPT and TTETA. The Tag naming/filling conventions 
C-    are described in Chapter 3 of the HBOOK manual (V4.15).
C-    To fill the ntuple, call the DO_HFNT(ID1,N1,XT1) ENTRY point with 
C-    XT1 filled with the appropriate values. 
C-
C-   Created 19-OCT-1994   Chip Stewart
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) PARAM
      INTEGER ID
      INTEGER MAXLIST,NID
C----------------------------------------------------------------------
      INCLUDE 'D0$OFFLINE_UTIL$GENERAL:VALUE.DEF'
C----------------------------------------------------------------------
      INTEGER I,J,K,L,M,N,STATUS,NVALS,II,JJ,KK,IDD
      INTEGER LENGTH,LTITLE,NWORDS,NPRIME,NDIM,LENG2,LP
C
      INTEGER MAXSIZE,MAXIDS,MAXDIM,MAXCWN
      PARAMETER( MAXSIZE = 20000 )
      PARAMETER( MAXIDS  = 5000  )      ! Maximum number of IDs to store
      PARAMETER( MAXDIM  = 800   )      ! Maximum number of tags
C
      LOGICAL RW, BOOK
      INTEGER IARRAY(MAXSIZE),TYPES(MAXSIZE)
      INTEGER IDDLIST(MAXIDS),NIDD,ID1,N1,LL
      REAL    RARRAY(MAXSIZE),XT1(MAXSIZE),XT(MAXSIZE)
      EQUIVALENCE(IARRAY,RARRAY)
C
      CHARACTER*4 HTYPE
      CHARACTER*4 LOOP,CHOPT,CH
      CHARACTER*20 TAGS(MAXDIM)
      CHARACTER*32 CHRZPA
      CHARACTER*80 CHTITL,REMARK
      CHARACTER*800 CHFORM
C----------------------------------------------------------------------
      LP = LEN(PARAM)
C
C ****  Load array from current rcp bank
C
      CALL EZGET_VALUE_TYPE ( PARAM(1:LP),IARRAY,TYPES,NVALS,STATUS )
    6 IF ( STATUS.NE.0 ) THEN
        CALL ERRMSG('DO_HBOOKN','EZGET_VALUE_TYPE',
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
          CALL ERRMSG('BAD_SYNTAX','DO_HBOOKN',REMARK,'W')
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
          CALL ERRMSG('BAD_SYNTAX','DO_HBOOKN',REMARK,'W')
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
        NDIM = 0
        DO WHILE ( (TYPES(II) .GT. VTCHR) .and. (II.LE.NVALS) ) 
          NDIM = NDIM + 1
          LENGTH = TYPES(II) - 10
          NWORDS = 1 + ( LENGTH - 1 )/4
          TAGS(NDIM) = ' '
          CALL DHTOC(LENGTH,IARRAY(II),TAGS(NDIM))
          II = II + NWORDS
        ENDDO
   99   CONTINUE
C
        IF ( BOOK ) THEN
          IF(RW) THEN
            CALL HBOOKN (ID,CHTITL(1:LTITLE),NDIM,CHRZPA,NPRIME,TAGS)
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
            DO I = 1, NDIM
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
            LL = (NDIM/50) * 50 + 1
            L = L -1
            CALL HBNAME(ID,CHTITL(1:LTITLE),XT(1),CHFORM(2:L))
          END IF
        ENDIF
        II = II - 1   ! Backspace
C
      ENDDO
  999 RETURN
C----------------------------------------------------------------------
      ENTRY DO_HFNT(ID1,N1,XT1)
C----------------------------------------------------------------------
      ID = ID1
      N = N1
      CALL UCOPY(XT1,XT,N)
      CALL HFNT(ID)
      RETURN
      END
