      SUBROUTINE DB_SERVER_NAMES
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Extrcats and palces various names associated with
C-                         DBL3 server in common blocks
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created   9-AUG-1991   SHAHRIAR ABACHI
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE       'D0$INC:DB_SRVR_NAMES.INC'
      INCLUDE       'D0$INC:D0DB_IDS.INC'
      LOGICAL       VALID_PRC
C&IF VAXVMS
      INCLUDE '($LNMDEF)'
C&ELSE
C&
C&ENDIF
      INTEGER ITMLST(4)
      INTEGER*2 LENCOD(2)
      INTEGER ISTAT, CLEN, DL
      INTEGER  SYS$TRNLNM
      EXTERNAL SYS$TRNLNM
      CHARACTER*11 DBCLIENT
      CHARACTER*9  DBCL
      LOGICAL  VALID_LOG
      INTEGER RETLEN
      CHARACTER*80 RESULT
      EQUIVALENCE (ITMLST,LENCOD)
      INTEGER K_DB_L, I, DBLEN
C
      CALL DB_G_NAME   (SRVR_PRCNAME,SRVR_PRCNAME_L)
      CALL DB_G_IMGNAME(SRVR_IMGNAME,SRVR_IMGNAME_L)
C         Check validity of Image name
      PRINT *,'Server- Image name ',srvr_imgname(1:srvr_imgname_l)
      PRINT *,'Server- Process name ',srvr_prcname(1:srvr_prcname_l),
     +          srvr_prcname_l
C
      LENCOD(1) = 80
      LENCOD(2) = LNM$_STRING
      ITMLST(2) = %LOC(RESULT)
      ITMLST(3) = %LOC(RETLEN)
      ITMLST(4) = 0
      DBCLIENT(1:11) = 'DBL3$CLIENT'
      ISTAT=SYS$TRNLNM(,'LNM$PROCESS_TABLE',DBCLIENT,,ITMLST)
      DBCL = RESULT(:RETLEN)
      CALL STR$TRIM(DBCL,DBCL,DBLEN)
C
      VALID_LOG = .FALSE.
C
      DO I = 1, NUM_DB
        IF ( DBCL(1:2).EQ.DB_SNAME(I)(1:2)) THEN
          VALID_LOG = .TRUE.
          K_DB_L    = I
        ENDIF
      ENDDO
C
      IF (VALID_LOG) THEN
        DB_CURRENT_SNAME = DB_SNAME(K_DB_L)
        DB_CURRENT_LNAME = DB_LNAME(K_DB_L)
      ELSE
        STOP 444
      ENDIF
C
D     PRINT *,'Server- current names ',db_current_lname,' and '
D    +  ,db_current_sname
      RETURN
      END
