      SUBROUTINE LSQ_RZ_SAVE(MATRIX_NAME,SUBD,ACTION,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Save LSQ TO RZ
C-
C-   Inputs  :MATRIX_NAME MATRIX TO BE SAVED
C-            SUBD = SUBDIRECTORY TO SAVE TO
C-            ACTION = ' ' ONLY SAVE MATRIX_NAME
C-                   = 'ALL' SAVE ALL MATRICES
C-   Outputs :IER = 1, MATRIX_NAME DOES NOT EXIST
C-   IER =2 ERROR WRITING TO RZ
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
      CHARACTER*132 MSG1
C----------------------------------------------------------------------
C
      IF ( ACTION.NE.'ALL' ) THEN
        IF ( .NOT.LSQ_MATRIX_EXIST(MATRIX_NAME,INDEX,IER) ) THEN
          IER = 1
          RETURN
        ENDIF
      ENDIF
C
      CALL RZCDIR('//'//TOP_DIRECTORY,' ')
C
      CALL RZCDIR(SUBD,' ')
C
      IF(IQUEST(1).EQ.1)THEN
        CALL RZMDIR(SUBD,NKEYS,KEY_DESCR,CHTAG)
        LENTOP = TRULEN(TOP_DIRECTORY)
        DIR_SUB = TOP_DIRECTORY(1:LENTOP)//'/'//SUBD
        WRITE(MSG1,1)DIR_SUB
    1   FORMAT(' Created sub_directory ',A)
        CALL ERRMSG('LSQ','LSQ_RZ_SAVE',MSG1,'W')
        CALL RZCDIR(SUBD,' ')
      ENDIF
C
C ****  READY TO WRITE OUT RZ FILE HERE
C

      IF(ACTION .EQ. 'ALL')THEN
        ILO = 1
        IHI = NMAT
      ELSE
        ILO = INDEX
        IHI = INDEX
      ENDIF
C
      KEY = 1
      CALL RZVOUT(M_BUF,MB,KEY,ICYCLE,' ')
      WRITE(MSG1,3)ICYCLE ,SUBD(1:TRULEN(SUBD))
    3 FORMAT(' Wrote Cycle ',I7,' of CONTROL BLOCK ',
     &  ' to RZ  sub-dir. ',A)
      CALL ERRMSG('LSQ','LSQ_RZ_SAVE',MSG1,'W')
C
      DO I = ILO , IHI
        IF(M_DELETE(I).EQ.0)THEN
          LINK = LSTLNK(I)
          IF ( LINK.GT.0 ) THEN
            KEY = I+1
            CALL RZOUT(IDVSTP,LINK,KEY,ICYCLE,' ')
            WRITE(MSG1,2)ICYCLE,M_NAME(I)(1:TRULEN(M_NAME(I))),
     &        SUBD(1:TRULEN(SUBD))
    2       FORMAT(' Wrote Cycle ',I7,' of matrix ',A,
     &        ' to RZ  sub-dir. ',A)
            CALL ERRMSG('LSQ','LSQ_RZ_SAVE',MSG1,'W')
          ENDIF
        ENDIF
      ENDDO
C
      IF(IQUEST(1).NE.0)THEN
        IER = 2
        CALL ERRMSG('LSQ','LSQ_RZ_SAVE',
     &      'ERROR SAVING TO RZ','W')
      ENDIF
C
  999 RETURN
      END
