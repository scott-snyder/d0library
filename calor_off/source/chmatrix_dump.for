      SUBROUTINE CHMATRIX_DUMP
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : DUMPS A SET OF HMATRICES AND ALSO QUAN BANK
C-   FOR NEV EVENTS
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created   6-JUN-1992   Rajendran Raja
C-   Updated   3-Sep-1992   Herbert Greenlee
C-      Use GTUNIT rather than DMPUNI to get dump unit.
C-   Updated   7-APR-1995   Alan M. Jonckheere  Changed DGET -> DDGET
C-      to avoid conflict with new intrinsic function name 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER DMPUNI,DUNIT
      INCLUDE 'D0$INC:HMATRIX_PARS.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:ZHMATRIX.INC'
      INTEGER NEV
      INTEGER IEV
      INTEGER NMAT,NAMES
      CHARACTER*32 DUMP_HMATRICES(HMTR_MAX)
      CHARACTER*4 DUMP_NAMES(20)
      INTEGER NUSE,IER,I,J,II
      LOGICAL FIRST, OPENOK
C
      CHARACTER*80 TITLE
      REAL    UP_LIM
      LOGICAL PLOT_MATRIX
      DOUBLE PRECISION DAVER,DHMAT,DEMAT
      REAL    AVER,HMAT,EMAT
      INTEGER IND,HMINDEX
      INTEGER HMATRIX_USER
      SAVE FIRST
      DATA FIRST / .TRUE. /
C
      INTEGER HID
C----------------------------------------------------------------------
      IF ( ACCUMULATE ) THEN
        RETURN    !ONLY DUMP BANKS IN USAGE MODE.
      ENDIF
C
      IF( FIRST ) THEN
        FIRST = .FALSE.
        IEV = 0
        HID = 1
        CALL EZPICK('HMATRIX_RCP')
        CALL EZGET('HMATRIX_USER',HMATRIX_USER,IER)
        CALL EZRSET
        IF(IER.EQ.0)CALL GTUNIT(HMATRIX_USER,DUNIT,IER)
        IF(IER.EQ.0)THEN
          CALL D0OPEN(DUNIT,'HMATRIX.DUMP','OF',OPENOK)
          IF(.NOT.OPENOK)CALL ERRMSG('CALORIMETER','CHMATRIX_DUMP',
     &      ' Unable to OPEN dump file','W')
        ELSE
          CALL ERRMSG('CALORIMETER','CHMATRIX_DUMP',
     &      ' No UNIT available','W')
          OPENOK = .FALSE.
        ENDIF
        IF(.NOT.OPENOK)GO TO 999
        CALL EZPICK('CAPHEL_RCP')
        CALL EZ_GET_CHARS('DUMP_HMATRICES',NMAT,DUMP_HMATRICES,IER)
        CALL EZ_GET_CHARS('DUMP_BANK_NAMES',NAMES,DUMP_NAMES,IER)
        CALL EZGET('NUMBER_OF_QUANS_TO_DUMP',NEV,IER)
        CALL EZRSET
        DO I = 1 , NMAT
          WRITE(DUNIT,1)DUMP_HMATRICES(I)
    1     FORMAT(// ' DUMP OF HMATRIX ',A/)
          CALL HMATRIX_SET(DUMP_HMATRICES(I),IER)
          DO J = 1 , NAMES
            IF ( DUMP_NAMES(J).EQ.'AVER') THEN
              CALL PRAVER(DUNIT,LAVER,0,'ALL',0)
            ELSEIF (DUMP_NAMES(J).EQ.'EMAT') THEN
              CALL PREMAT(DUNIT,LEMAT,0,'ALL',0)
            ELSEIF (DUMP_NAMES(J).EQ.'HMAT') THEN
              CALL PRHMAT(DUNIT,LHMAT,0,'ALL',0)
            ELSEIF (DUMP_NAMES(J).EQ.'HVIS') THEN
              CALL PRHVIS(DUNIT,LHVIS,0,'ALL',0)
            ELSEIF (DUMP_NAMES(J).EQ.'HINV') THEN
              CALL PRHINV(DUNIT,LHINV,0,'ALL',0)
            ELSEIF (DUMP_NAMES(J).EQ.'EIGN') THEN
              CALL PREIGN(DUNIT,LEIGN,0,'ALL',0)
            ELSEIF (DUMP_NAMES(J).EQ.'UMAT') THEN
              CALL PRUMAT(DUNIT,LUMAT,0,'ALL',0)
            ENDIF
C
            CALL EZPICK('HMATRIX_RCP')
            CALL EZGET('PLOT_MATRIX',PLOT_MATRIX,IER)
            CALL EZRSET
C
            IF ( (.NOT.ACCUMULATE).AND.PLOT_MATRIX ) THEN
C
              CALL DHDIR(' ','HMATRIX_DUMP',IER,' ')
C         ! Create/Set HBOOK directory
              IF ( IER.NE.0 ) THEN
                CALL ERRMSG('CALORIMETER','CHMATRIX_DUMP',
     &            ' ERROR SETTING HBOOK DIRECTORY ','W')
              ENDIF
C
              UP_LIM = VIS_DIM+1
C
              TITLE = 'AVERAGE QUANS IN '//DUMP_HMATRICES(J)
              CALL HBOOK1(HID,TITLE,TOT_DIM,1., UP_LIM,0.)
C
              TITLE = 'H MATRIX DIAGONAL '//DUMP_HMATRICES(J)
C
              CALL HBOOK1(HID+1,TITLE,TOT_DIM,1.,UP_LIM,0.)
C
              TITLE = 'E MATRIX DIAGONAL'//DUMP_HMATRICES(J)
C
              CALL HBOOK1(HID+2,TITLE,TOT_DIM,1.,UP_LIM,0.)
C
              DO 10 II = 1 ,VIS_DIM
                IND = 2*II-1                     ! DOUBLE PRECISION
                CALL DDGET(LAVER+IND,DAVER)
                AVER = DAVER
                CALL HFILL(HID,FLOAT(II),0.,AVER)
   10         CONTINUE
C
              DO 40 II = 1 , VIS_DIM
                IND = 2*HMINDEX(II,II,TOT_DIM,TOT_DIM)-1          ! DOUBLE PRECISION
C
                CALL DDGET(LHMAT+IND,DHMAT)
                HMAT = DHMAT
                CALL HFILL(HID+1,FLOAT(II),0.,HMAT)
C
                CALL DDGET(LEMAT+IND,DEMAT)
                EMAT = DEMAT
                CALL HFILL(HID+2,FLOAT(II),0.,EMAT)
C
   50           CONTINUE
   40         CONTINUE
              HID = HID + 3
C
            ENDIF
          ENDDO
          CALL HMATRIX_RESET
        ENDDO
      ENDIF
C
      IF ( IEV.GE.NEV .OR. .NOT.OPENOK ) THEN
        RETURN
      ELSE
        IEV = IEV+1
        CALL PRQUAN(DUNIT,LQUAN,0,'ALL',0)
      ENDIF
  999 RETURN
      END
