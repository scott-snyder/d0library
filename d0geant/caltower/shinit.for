      SUBROUTINE SHINIT
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Initialisation of RZ Shower Library.
C-             SHOWERLIBRARY should be defined at DCL level as the shower
C-             library file to be read.
C-
C-   Inputs  : RNDM(1) initial random number read from GEANT datacards
C-
C-   Outputs : ISUNIT in /SHLCON/ initialised; RZ file opened for read
C-             access on unit ISUNIT; zebra common /ZEBSHL/ defined
C-
C-   Created  22-FEB-1987   John Womersley
C-   Updated  28-FEB-1989   John Womersley
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:QUEST.INC/LIST'    ! ZEBRA QUEST VECTOR
      INCLUDE 'D0$INC:ZEBSHL.INC/LIST'
      INCLUDE 'D0$INC:GCUNIT.INC/LIST'
      INCLUDE 'D0$INC:GCFLAG.INC/LIST'
      INCLUDE 'D0$INC:SHLCON.INC/LIST'
      CHARACTER*13 FILNAM
      LOGICAL FIRST
      DATA FIRST /.TRUE./
      DATA FILNAM/'SHOWERLIBRARY'/
      INTEGER L,IERR
      CHARACTER*80 MSG
C----------------------------------------------------------------------
      IF(FIRST) THEN
        FIRST = .FALSE.
        IUSER = 1
        CHTAG(1) = 'VTX'
        CHTAG(2) = 'ETA'
        CHTAG(3) = 'MON'
        CHTAG(4) = 'PRT'
        CHTAG(5) = 'PHI'
        PARTAG(1) = 'E.M'
        PARTAG(2) = 'MUO'
        PARTAG(3) = 'HAD'
      ENDIF

      L=LEN(FILNAM)
      IUSER = 1
C
      CALL ERRMSG( 'START','SHINIT',
     &  'Started Shower library initialisation','I')
      IF (NRNDM(1).EQ.0)THEN
        CALL ERRMSG('NORAND','SHINIT',
     &    'IN FUTURE PLEASE USE RNDM CARD TO SET RANDOM NUMBER','W')
        ISEED = 229878
      ELSE
        ISEED = NRNDM(1)
      ENDIF
      WRITE(MSG,1000)ISEED
 1000 FORMAT(1X, 'Random number seed is',I12)
      CALL ERRMSG('RAND','SHINIT',MSG,'I')
C
C ****  get unit number for the shower library RZ file
C
      CALL GTUNIT(IUSER,ISUNIT,IERR)
      IF(IERR.NE.0)GOTO 9950
C
C ****  Define the Zebra store for the shower library
C
      IXSHLB=0
C
      CALL MZSTOR(IXSHLB,'/ZEBSHL/',' ',FENSHL,LS,LS,LS,
     +   LS(50000),ENDZSS)
      ISHDIV=IXSHLB+1
      CALL RZLOGL(ISUNIT,0)
      CALL MZLOGL(IXSHLB,0)
      CALL ERRMSG('MKZEB','SHINIT',
     &  'Created ZEBRA store /ZEBSHL/ ','I')
C
C ****  open the library file
C
C&IF VAXVMS,SIUNIX,ULTRIX,SUNOS
      OPEN(UNIT=ISUNIT,FILE=FILNAM(1:L),
     &             STATUS='OLD',FORM='UNFORMATTED',READONLY,
     &             ACCESS='DIRECT',RECL=4096,ERR=9940)
C&ENDIF
C&IF IBMAIX
C&      OPEN(UNIT=ISUNIT,FILE=FILNAM(1:L),
C&     &             STATUS='OLD',FORM='UNFORMATTED',
C&     &             ACCESS='DIRECT',RECL=4096,ERR=9940)
C&ENDIF
      CALL ERRMSG('OPFILE','SHINIT',
     &  ' Opened shower library file '//FILNAM(1:L),'I')
      CALL RZFILE(ISUNIT,'SHOWER_LIBRARY',' ')
      IF(IQUEST(1).NE.0)GOTO 9960
      CALL ERRMSG('RZDEF','SHINIT',
     &  ' Defined RZFILE for shower library','I')
C
  999 RETURN
C
 9950 CALL ERRMSG('NOUNIT','SHINIT',
     &   'ERROR GETTING UNIT NUMBER FROM GTUNIT','F')
      RETURN
C
 9940 CALL ERRMSG('FTOPER','SHINIT',
     &  'ERROR OPENING SHOWER LIBRARY FILE','F')
      RETURN
C
 9960 CONTINUE
      CALL ERRMSG('RZOPER','SHINIT',
     &  'RZ ERROR OPENING RZ SHOWER LIBRARY FILE','F')
      RETURN
      END
