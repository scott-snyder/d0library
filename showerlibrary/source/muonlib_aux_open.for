      SUBROUTINE MUONLIB_AUX_OPEN(IUNIT,FILNAM,SRECL,NEW,READONLY,
     &                           IERR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Open MUONLIBRARY AUXILLIARY FILE
C-
C-   Inputs  : IUNIT = unit number
C-             FILNAM = Filename to be opened
C-             SRECL = RECORD LENGTH
C-             NEW if true, new file opened
C-             IF READONLY, open is done with readonly flag.
C-   Outputs : IERR = value of IOSTAT flag.
C-   Controls:
C-
C-   Created  24-MAY-1993   Jasbir Singh (also   Rajendran Raja)
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL NEW,READONLY
      INTEGER IUNIT,SRECL
      CHARACTER*80 FILNAM
      INTEGER IOSTAT_OPEN,IERR
      CHARACTER*160 MSG
      INTEGER TRULEN,LENM
C----------------------------------------------------------------------

      IF ( NEW ) THEN
        OPEN(UNIT=IUNIT,STATUS='NEW',FORM='UNFORMATTED',
     &      FILE=FILNAM,KEY=(1:4:INTEGER),
     +      ACCESS='KEYED',ERR=9960,RECL=SRECL,
     +      RECORDTYPE = 'VARIABLE',
     +      ORGANIZATION='INDEXED',
     +      IOSTAT = IOSTAT_OPEN)
      ELSEIF ( READONLY ) THEN
C
C ****  OPEN EXISTING AUXIILIARY FILE READONLY MODE
C
        OPEN(UNIT=IUNIT,STATUS='OLD',FORM='UNFORMATTED',
     &      FILE=FILNAM,
     +      ACCESS='KEYED',ERR=9960,
     +      RECORDTYPE = 'VARIABLE',
     +      ORGANIZATION='INDEXED',
     +      IOSTAT = IOSTAT_OPEN,READONLY)
      ELSE
C
C ****  OPEN EXISTING LIBRARY IN WRITE MODE
C
        OPEN(UNIT=IUNIT,STATUS='OLD',FORM='UNFORMATTED',
     &      FILE=FILNAM,
     +      ACCESS='KEYED',ERR=9960,
     +      RECORDTYPE = 'VARIABLE',
     +      ORGANIZATION='INDEXED',
     +      IOSTAT = IOSTAT_OPEN)
      ENDIF
      LENM = MIN(TRULEN(FILNAM),80)
      WRITE(MSG,998)NEW,READONLY,FILNAM(1:LENM)
  998 FORMAT('OPENED AUXILLIARY FILE',
     &  ' NEW = ',L,' READONLY = ',L,1X,A)
      CALL ERRMSG('MUONLIBRARY','MUONLIB_AUX_OPEN',
     &  MSG,'W')
  999 RETURN
 9960 CALL ERRMSG('MUONLIBRARY','MUONLIB_AUX_OPEN',
     &  'ERROR OPENING MUONLIBRARY','W')
      IERR = IOSTAT_OPEN
      RETURN
      END
