      SUBROUTINE LSQ_RZ_GET(MATRIX_NAME,SUBD,ACTION,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : GET MATRICES FROM RZ
C-
C-   Inputs  :MATRIX_NAME MATRIX TO BE FETCHED
C-            SUBD = SUBDIRECTORY TO FETCH FROM
C-            ACTION = ' ' ONLY FETCH MATRIX_NAME
C-                   = 'ALL' FETCH ALL MATRICES FROM RZ. IN THIS CASE
C-                   THE EXISTING MARICES IN MEMORY WILL BE
C-                   OVERWRITTEN. SO DO THIS BEFORE BOOKING ANY
C-                   MATRICES.
C-   Outputs :IER = 1, MATRIX_NAME DOES NOT EXIST
C-            IER =2 ERROR READING FROM RZ
C-            IER =3 Non EXISTANT SUB-DIRECTORY
C-   Controls:
C-
C-   Updated  21-FEB-1992   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:LSQ_PARS.INC'
      INCLUDE 'D0$INC:ZLSQ.INC'
      INCLUDE 'D0$INC:LSQ_MATRIX.INC'
      INCLUDE 'D0$INC:QUEST.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      LOGICAL HDE_EXIST_RZ
      INTEGER KEY,ICYCLE,TRULEN
      INTEGER LENTOP
      CHARACTER*40 DIR_SUB
      INTEGER LINK
      CHARACTER*(*) MATRIX_NAME,SUBD,ACTION
      INTEGER IER,INDEX
      LOGICAL LSQ_MATRIX_EXIST
      INTEGER ILO,IHI,I
      INTEGER NLEN
      CHARACTER*132 MSG1
C----------------------------------------------------------------------
      CALL RZCDIR('//'//TOP_DIRECTORY,' ')
C
      CALL RZCDIR(SUBD,' ')
C
      IF(IQUEST(1).EQ.1)THEN
        WRITE(MSG1,1)DIR_SUB
    1   FORMAT(' Non existent subdirectory ',A)
        CALL ERRMSG('LSQ','LSQ_RZ_GET',MSG1,'W')
        IER = 3
        RETURN
      ENDIF
C
C ****  READY TO READ FROM RZ FILES HERE
C
      IF(ACTION.EQ.'ALL')THEN
        KEY = 1
        ICYCLE = 99999
        CALL RZVIN(M_BUF,MB,NLEN,KEY,ICYCLE,' ')
        ICYCLE = IQUEST(6)
        WRITE(MSG1,3)ICYCLE ,SUBD(1:TRULEN(SUBD))
    3   FORMAT(' Read Cycle ',I7,' of CONTROL BLOCK ',
     &    ' from RZ  sub-dir. ',A)
        CALL ERRMSG('LSQ','LSQ_RZ_GET',MSG1,'W')
        MSG1 = ' EXISTING MATRICES WILL BE OVERWRITTEN '
        CALL ERRMSG('LSQ','LSQ_RZ_GET',MSG1,'W')
      ENDIF
C
      IF ( ACTION.NE.'ALL' ) THEN
        IF ( .NOT.LSQ_MATRIX_EXIST(MATRIX_NAME,INDEX,IER) ) THEN
          IER = 1
          RETURN
        ENDIF
      ENDIF
C
      IF(ACTION .EQ. 'ALL')THEN
        ILO = 1
        IHI = NMAT
      ELSE
        ILO = INDEX
        IHI = INDEX
      ENDIF
C
      DO I = ILO , IHI
        IF(M_DELETE(I).EQ.0)THEN
          KEY = I+1
          ICYCLE = 99999
          CALL RZIN(IDVSTP,LINK,2,KEY,ICYCLE,' ')
          ICYCLE = IQUEST(6)
          LSTLNK(I) = LINK
          WRITE(MSG1,2)ICYCLE,M_NAME(I)(1:TRULEN(M_NAME(I))),
     &        SUBD(1:TRULEN(SUBD))
    2     FORMAT(' Read Cycle ',I7,' of matrix ',A,
     &        ' from RZ  sub-dir. ',A)
          CALL ERRMSG('LSQ','LSQ_RZ_GET',MSG1,'W')
        ENDIF
      ENDDO
C
      IF(IQUEST(1).NE.0)THEN
        IER = 2
        CALL ERRMSG('LSQ','LSQ_RZ_GET',
     &      'ERROR SAVING TO RZ','W')
      ENDIF
C
  999 RETURN
      END
