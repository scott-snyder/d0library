      SUBROUTINE DO_HBOOK(PARAM)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book HBOOK histograms and ntuples from and
C-   rcp bank.
C-
C-   Inputs  : PARAM    [C*]    NAME OF ARRAY PARAMETER
C-   Outputs :
C-   Controls:
C-
C-             ALL HISTOGRAM PARAMETERS HAVE NAMES THAT ARE IDENTICAL TO THE
C-             HBOOK4 MANUAL NAMES.
C-
C-   Entry Points:
C-
C-    DO_HBOOK_GET_IDS(MAXID,ID,NID)    Get all ids (-ve --> not booked)
C-    DO_HF1(ID,X,W)
C-    DO_HF2(ID,X,Y,W)
C-    DO_HFN(TOPDIR,ID,XX)
C-    DO_HSAVE(TOPDIR,ID)
C-
C-   Created  12-DEC-1989   Boaz Klima
C-   Updated   6-JAN-1991   Harrison B. Prosper
C-      Added entry point GET_DO_HBOOK_IDS(MAXLIST,IDLIST,NID)
C-   Modified 12-APR-1991   Alexandr Peryshkin
C-      Added using  HBPROF
C-   Updated  11-NOV-1991   Harrison B. Prosper
C-      Use NTUPLE package
C-   Updated   6-DEC-1991   Harrison B. Prosper
C-      Add entry points DO_HF1, DO_HF2
C-   Updated  21-MAY-1992   Harrison B. Prosper
C-      If CHZPA is ' ' use HBOOKN directly
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) PARAM
      CHARACTER*(*) TOP_DIRECTORY
      INTEGER HID
      REAL    X,Y,W,XX(*)
      INTEGER MAXLIST,IDLIST(*),NID
C----------------------------------------------------------------------
      INCLUDE 'D0$OFFLINE_UTIL$GENERAL:VALUE.DEF'
C----------------------------------------------------------------------
      INTEGER I,J,K,L,M,N,STATUS,NVALS,II,JJ,IDD,IDO1,IDO2,IDMAX
      INTEGER LENGTH,LTITLE,NWORDS,NPRIME,NDIM,ID,NX,NY,LENG2,LP
      REAL    XMI,XMA,YMI,YMA,VMX
C
      INTEGER MAXSIZE,MAXIDS,MAXDIM
      PARAMETER( MAXSIZE = 20000 )
      PARAMETER( MAXIDS  = 5000  )      ! Maximum number of IDs to store
      PARAMETER( MAXDIM  = 500   )      ! Maximum number of tags
C
      INTEGER IARRAY(MAXSIZE),TYPES(MAXSIZE)
      INTEGER IDDLIST(MAXIDS),NIDD
      REAL    RARRAY(MAXSIZE)
      EQUIVALENCE(IARRAY,RARRAY)
C
      CHARACTER*4 HTYPE
      CHARACTER*4 LOOP,CHOPT,CH
      CHARACTER*8 TAGS(MAXDIM)
      CHARACTER*32 CHRZPA
      CHARACTER*80 CHTITL,REMARK
      LOGICAL BOOK
C----------------------------------------------------------------------
      LP = LEN(PARAM)
C
C ****  Load array from current rcp bank
C
      CALL EZGET_VALUE_TYPE ( PARAM(1:LP),IARRAY,TYPES,NVALS,STATUS )
C
      IF ( STATUS.NE.0 ) THEN
        CALL ERRMSG('DO_HBOOK','EZGET_VALUE_TYPE',
     &    'ERROR IN HBOOK RCP FILE','W')
        GOTO 999
      ENDIF
C
      NIDD = 0                          ! Number of booked histograms
      II   = 0
      DO WHILE ( II.LT.NVALS )
C
C ****  DEFAULT PARAMETERS
C
        BOOK=.TRUE.
        IDO1 = 0
        IDO2 = 1
C
        II = II + 1
C
C       CHARACTER TYPE      VTCHR + LENGTH
C ........................................
C
C ****  TEST FOR OPTIONAL BOOK SWITCH ( DEFAULT IS TRUE )
C
        IF ( TYPES(II).EQ.VTLOG ) THEN
          IF ( IARRAY(II).EQ.0 ) THEN
            BOOK=.FALSE.
          ELSE
            BOOK=.TRUE.
          ENDIF
          II = II + 1
        ELSE
          BOOK=.TRUE.
        ENDIF
C
C ****  TEST FOR OPTIONAL DO LOOP ( DEFAULT IS NO LOOP )
C
        IF ( TYPES(II).GT.VTCHR ) THEN
          LOOP=' '
          CALL DHTOC(4,IARRAY(II),LOOP)
          IF ( LOOP(1:2).EQ.'DO' .OR.
     &         LOOP(1:2).EQ.'do' .OR.
     &         LOOP(1:2).EQ.'Do'      ) THEN
            II = II + 1
            IDO1 = 0
            IDO2 = 1
            IF ( TYPES(II).EQ.VTINT ) THEN
              IDO1=IARRAY(II)
              II = II + 1
              IF ( TYPES(II).EQ.VTINT ) THEN
                IDO2=IARRAY(II)
                II = II + 1
              ENDIF
            ENDIF
          ELSE
            IDO1 = 0
            IDO2 = 1
          ENDIF
C
C ****  WHICH TYPE OF HISTOGRAM? ( 1DIM - 1 DIMENTIONAL,
C ****                             2DIM - 2 DIMENTIONAL,
C ****                             PROF - HBPROFile,
C ****                             NDIM - NTUPLE.
C
          HTYPE=' '
          CALL DHTOC(4,IARRAY(II),HTYPE)
          II = II + 1
C
C ****  HISTOGRAM ID
C
          ID = IARRAY(II)
          II = II + 1
C
C ****  HISTOGRAM TITLE
C
          IF ( TYPES(II) .LT. VTCHR ) THEN
            REMARK = ' Error in specs/Param: '//PARAM(1:LP)
            CALL ERRMSG('BAD_SYNTAX','DO_HBOOK',REMARK,'W')
            GOTO 999
          ENDIF
C
          LTITLE = TYPES(II) - VTCHR
          NWORDS = 1 + ( LTITLE - 1 )/4
          CHTITL = ' '
          CALL DHTOC(LTITLE,IARRAY(II),CHTITL)
          II = II + NWORDS
C
          IF ( HTYPE(1:1) .EQ. '1' ) THEN
C
C ****  1 DIMENSIONAL HYSTOGRAM(S)
C
            NX  = IARRAY(II)
            XMI = RARRAY(II+1)
            XMA = RARRAY(II+2)
            VMX = RARRAY(II+3)
            II = II + 3
            IDMAX = MAX(ID,ID+IDO1*IDO2-1)
C
            DO IDD = ID,IDMAX,IDO2
C
              IF ( BOOK ) THEN
                CALL HBOOK1(IDD,CHTITL(1:LTITLE),NX,XMI,XMA,VMX)
              ENDIF
C
              IF ( NIDD .LT. MAXIDS ) THEN
                NIDD = NIDD + 1
                IF ( BOOK ) THEN
                  IDDLIST(NIDD) = IDD
                ELSE
                  IDDLIST(NIDD) =-IDD
                ENDIF
              ENDIF
            ENDDO
C
          ELSEIF ( HTYPE(1:1) .EQ. '2' .OR. HTYPE .EQ.'PROF') THEN
C
C ****  2 DIMENSIONAL HYSTOGRAM(S)
C
            NX  = IARRAY(II)
            XMI = RARRAY(II+1)
            XMA = RARRAY(II+2)
            NY  = IARRAY(II+3)
            YMI = RARRAY(II+4)
            YMA = RARRAY(II+5)
            VMX = RARRAY(II+6)
            IF ( HTYPE .EQ.'PROF') THEN
              II = II + 5
            ELSE
              II = II + 6
            ENDIF
C
            IDMAX = MAX(ID,ID+IDO1*IDO2-1)
            DO IDD = ID,IDMAX,IDO2
C
              IF ( BOOK ) THEN
                IF ( HTYPE .EQ.'PROF') THEN
                  II = II - 5
                  YMI = RARRAY(II+3)
                  YMA = RARRAY(II+4)
                  IF (TYPES(II+5).LT.VTCHR) THEN
                    REMARK = ' Error in PROF specs/Param: '//PARAM(1:LP)
                    CALL ERRMSG('BAD_SYNTAX','DO_HBOOK',REMARK,'W')
                    GOTO 999
                  ENDIF
C
                  LENG2 = TYPES(II+5) - VTCHR
                  NWORDS = 1 + ( LENG2 - 1 )/4
                  CHOPT = ' '
                  CALL DHTOC(LENG2,IARRAY(II+5),CHOPT)
                  II = II + NWORDS + 4
                  CALL HBPROF(IDD,CHTITL(1:LTITLE),NX,XMI,XMA,YMI,YMA,
     &              CHOPT)
                ELSE
                  CALL HBOOK2(IDD,CHTITL(1:LTITLE),NX,XMI,XMA,NY,YMI,
     &              YMA,VMX)
                ENDIF
              ENDIF
C
C ****  NOTE IDs
C
              IF ( NIDD .LT. MAXIDS ) THEN
                NIDD = NIDD + 1
                IF ( BOOK ) THEN
                  IDDLIST(NIDD) = IDD
                ELSE
                  IDDLIST(NIDD) =-IDD
                ENDIF
              ENDIF
            ENDDO
C
          ELSEIF( HTYPE.EQ.'NDIM') THEN
C
C ****  N DIMENSIONAL HYSTOGRAM(S), NTUPLE(S)
C
C ****  Get Dimension
C
            NDIM = IARRAY(II)
            II = II + 1
C
            IF ( TYPES(II) .LT. VTCHR ) THEN
              REMARK = ' Error in NTUPLE specs/Param: '//PARAM(1:LP)
              CALL ERRMSG('BAD_SYNTAX','DO_HBOOK',REMARK,'W')
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
              NPRIME = 8191
            ENDIF
C
C ****  Get tags
C
            DO JJ = 1 , NDIM
              LENGTH = TYPES(II) - 10
              NWORDS = 1 + ( LENGTH - 1 )/4
              TAGS(JJ) = ' '
              CALL DHTOC(LENGTH,IARRAY(II),TAGS(JJ))
              II = II + NWORDS
            ENDDO
            II = II - 1   ! Backspace
C
            IDMAX = MAX(ID,ID+IDO1*IDO2-1)
            DO IDD = ID,IDMAX,IDO2
C
              IF ( BOOK ) THEN
C
C ****  TRY TO BOOK A DISK-RESIDENT NTUPLE; If that fails then
C ****  book a memory-resident ntuple
C
                CALL NTUPLE_SET_ID(IDD,1)
                CALL NTUPLE_BOOK
     &              (CHRZPA,NDIM,TAGS,CHTITL(1:LTITLE),I,STATUS)
                IF ( STATUS .NE. 0 ) THEN
                  CALL HBOOKN 
     &              (IDD,CHTITL(1:LTITLE),NDIM,' ',NPRIME,TAGS)
                ENDIF
              ENDIF
C
C ****  Note IDs
C
              IF ( NIDD .LT. MAXIDS ) THEN
                NIDD = NIDD + 1
                IF ( BOOK ) THEN
                  IDDLIST(NIDD) = IDD
                ELSE
                  IDDLIST(NIDD) =-IDD
                ENDIF
              ENDIF
            ENDDO
          ELSE
            REMARK = ' No action: '//HTYPE
            CALL ERRMSG('NO_HBOOK_TYPE','DO_HBOOK',REMARK,'W')
            GOTO 999
          ENDIF
        ENDIF
  900   CONTINUE
      ENDDO
  999 RETURN
C
C ****  Entry point to return list of histogram IDs
C
      ENTRY DO_HBOOK_GET_IDS(MAXLIST,IDLIST,NID)
C      ENTRY GET_DO_HBOOK_IDS(MAXLIST,IDLIST,NID)
      NID = MIN(MAXLIST,NIDD)
      CALL UCOPY(IDDLIST,IDLIST,NID)
      RETURN
C
C ****  Entry point to fill histograms
C
      ENTRY DO_HF1(HID,X,W)
      IF ( HID .GT. 0 ) THEN
        CALL HF1(HID,X,W)
      ENDIF
      RETURN
C
      ENTRY DO_HF2(HID,X,Y,W)
      IF ( HID .GT. 0 ) THEN
        CALL HF2(HID,X,Y,W)
      ENDIF
      RETURN
C
      ENTRY DO_HFN(TOP_DIRECTORY,HID,XX)
      IF ( HID .GT. 0 ) THEN
        CALL NTUPLE_FILL
     &    (TOP_DIRECTORY(1:LEN(TOP_DIRECTORY)),HID,XX,STATUS)
      ENDIF
      RETURN
C
      ENTRY DO_HSAVE(TOP_DIRECTORY,HID)
      IF ( HID .GE. 0 ) THEN
        CALL NTUPLE_SAVE(TOP_DIRECTORY(1:LEN(TOP_DIRECTORY)),HID,STATUS)
      ENDIF
      RETURN
      END
