      SUBROUTINE MUONLIB_OPEN(IUNIT,FILNAM,SRECL,NEW,KEYED,READONLY,
     &                           IERR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Open MUONlibrary proper.
C-
C-   Inputs  : IUNIT = unit number
C-             FILNAM = Filename to be opened
C-             SRECL = RECORD LENGTH
C-             NEW if true, new file opened
C-             IF KEYED, KEYED ACCESS FILE ASSUMED.
C-             IF NOT KEYED RZ FILE OPENED. (NOT SUPPORTED)
C-             IF (KEYED. AND. NEW)THEN one is writing a MUONlibrary
C-             which will eventually be made into a keyed access file
C-             but for sake of efficiency will be written in variable
C-             length unformatted.
C-             IF READONLY, open is done with readonly flag.
C-   Outputs : IERR = value of IOSTAT flag.
C-   Controls:
C-
C-   Created  18-SEP-1990   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL NEW,KEYED,READONLY
      INTEGER IUNIT,SRECL
      CHARACTER*80 FILNAM
      INTEGER IOSTAT_OPEN,IERR
      CHARACTER*160 MSG
      INTEGER TRULEN,LENM
C----------------------------------------------------------------------
      IF ( KEYED ) THEN
C
C ****  KEYED ACCESS FILE ASSUMED
C

        IF ( NEW ) THEN
C
C ****  writing without keyed access to a variable length sequential
C ****  fortran file for speed. This file will be sorted and merged 
C ****  with others.
C
          OPEN(UNIT=IUNIT,STATUS='NEW',FORM='UNFORMATTED',
     &      FILE=FILNAM,RECORDTYPE='VARIABLE',
     &      RECL = SRECL,
     +      ACCESS='SEQUENTIAL',ERR=9960,
     +      IOSTAT = IOSTAT_OPEN)
        ELSEIF ( READONLY ) THEN
C
C ****  OPEN EXISTING LIBRARY IN READONLY MODE
C
          OPEN(UNIT=IUNIT,STATUS='OLD',FORM='UNFORMATTED',
     +      FILE=FILNAM,
     +      ACCESS='KEYED',ERR=9960,
     +      ORGANIZATION='INDEXED',
     +      IOSTAT = IOSTAT_OPEN,READONLY)
        ELSE
C
C ****  OPEN EXISTING LIBRARY IN WRITE MODE
C
          OPEN(UNIT=IUNIT,STATUS='OLD',FORM='UNFORMATTED',
     +      FILE=FILNAM,
     +      ACCESS='KEYED',ERR=9960,
     +      ORGANIZATION='INDEXED',
     +      IOSTAT = IOSTAT_OPEN)
        ENDIF
      ELSE
C
C ****  RZ FILE ASSUMED
C
        IF ( NEW ) THEN
          OPEN(UNIT=IUNIT,STATUS='NEW',FORM='UNFORMATTED',
     &      FILE=FILNAM,
     +      ACCESS='DIRECT',ERR=9960,RECL=SRECL,
     +      IOSTAT = IOSTAT_OPEN)
        ELSEIF ( READONLY ) THEN
C
C ****  OPEN EXISTING LIBRARY IN READONLY MODE
C
          OPEN(UNIT=IUNIT,FILE=FILNAM,
     &             STATUS='OLD',FORM='UNFORMATTED',
     &             ACCESS='DIRECT',ERR=9960,READONLY)
        ELSE
C
C ****  OPEN EXISTING LIBRARY IN WRITE MODE
C
          OPEN(UNIT=IUNIT,FILE=FILNAM,
     &             STATUS='OLD',FORM='UNFORMATTED',
     &             ACCESS='DIRECT',ERR=9960)
        ENDIF
      ENDIF
      LENM = MIN(TRULEN(FILNAM),80)
      WRITE(MSG,998)NEW,KEYED,READONLY,FILNAM(1:LENM)
  998 FORMAT('OPENED MUONLIBRARY',
     &  ' NEW = ',L,' KEYED = ',L,' READONLY = ',L,1X,A)
      CALL ERRMSG('MUONLIBRARY','MUONLIB_OPEN',
     &  MSG,'W')
  999 RETURN
 9960 CALL ERRMSG('MUONLIBRARY','MUONLIB_OPEN',
     &  'ERROR OPENING MUONLIBRARY','W')
      IERR = IOSTAT_OPEN
      RETURN
      END
