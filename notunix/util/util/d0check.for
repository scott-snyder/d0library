      PROGRAM D0CHECK
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-
C-   Check if given .FOR file conforms to F77 standards with D0
C-   allowed extensions by invoking the fortran compiler with the
C-   /STANDARD=ALL switch and analyzing the compilation listing.
C-
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Created  12-JUN-1989   Harrison B. Prosper
C-   Updated  26-FEB-1992   Harrison B. Prosper  
C-    Change standards 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE

      CHARACTER*80 LISTING
      CHARACTER*40 NODE,DEVICE,DIR,NAME,TYPE,VER
      LOGICAL OK,FOUND,FAILED

      INTEGER       NQUAL
      PARAMETER(    NQUAL = 1 )         ! Number of non-value qualifiers
      LOGICAL       PRESENT(NQUAL),COMPILE
      EQUIVALENCE (PRESENT(1),COMPILE)

      CHARACTER*16  QUALIFIER(NQUAL)
      DATA QUALIFIER/'COMPILE'/

      INTEGER ERROR_CODE,LLISTING,STATUS_CODE
      INTEGER LNODE,LDEVICE,LDIR,LNAME,LTYPE,LVER

      CHARACTER*(*) FACILITY
      CHARACTER*(*) VERSION
      CHARACTER*(*) LISTIN
      CHARACTER*(*) LISTOUT
      CHARACTER*(*) FORTRAN

      PARAMETER( FACILITY = 'D0CHECK' )
      PARAMETER( VERSION  = FACILITY//' V2.00' )
      PARAMETER( LISTIN   = FACILITY//'.INPUT' )
      PARAMETER( LISTOUT  = FACILITY//'.OUTPUT' )
      PARAMETER( FORTRAN  = 'FORT/NOOBJ/STANDARD=ALL/WARN=ALL/LIST ')

      CHARACTER*23  DATETIME

      INTEGER MAXFIL
      PARAMETER( MAXFIL = 511 )
      CHARACTER*132 FILENAME(MAXFIL)
      INTEGER       LFILE(MAXFIL),NFILE,III,II,JJ

      INTEGER STATUS,LUNOUT
      PARAMETER( LUNOUT = 80  )
C
C ****  Declare CLD module name
C
      EXTERNAL D0CHECK_COMMANDS
C
C ****  Declare error message symbols
C
      EXTERNAL D0CK_BADEXTN,D0CK_VIOLATES,D0CK_NOTFOUND,D0CK_CONFORMS
      EXTERNAL D0CK_COMPILE,D0CK_CHECKING,D0CK_FAILED

C----------------------------------------------------------------------
C
C ****  Open temporary file
C
      OPEN (UNIT=LUNOUT,FILE=LISTIN,STATUS='NEW')
      WRITE(UNIT=LUNOUT,FMT='(A)') ' '
      CLOSE(UNIT=LUNOUT)

C *********************************************
C ****  Get list of files and qualifier(s)
C *********************************************

      FAILED = .FALSE.

      CALL DECODE_COMMAND_LINE (
     &  D0CHECK_COMMANDS,               ! INPUTS
     &  FACILITY,
     &  QUALIFIER,
     &  NQUAL,

     &  PRESENT,                        ! OUTPUTS
     &  FILENAME,
     &  LFILE,
     &  NFILE,
     &  ERROR_CODE)

      IF ( ERROR_CODE .NE. 0 ) GOTO 999
C
C ****  LOOP OVER FILES
C
      DO III = 1,NFILE

        CALL PARSE_FILENAME(FILENAME(III)(1:LFILE(III)),
     &    NODE,LNODE,
     &    DEVICE,LDEVICE,
     &    DIR,LDIR,
     &    NAME,LNAME,
     &    TYPE,LTYPE,
     &    VER,LVER)
C
C ****  Check type
C
        IF ( (TYPE(1:4)     .NE. '.FOR')    .AND.
     &       (TYPE(1:LTYPE) .NE. '.CHECK') ) THEN
          CALL LIB$SIGNAL(D0CK_BADEXTN, %VAL(1),
     &                    FILENAME(III)(1:LFILE(III)) )
          STATUS_CODE = %LOC(D0CK_BADEXTN)
          GOTO 999
        ENDIF

        IF ( COMPILE ) THEN
C
C ****  Compile with /STANDARD=ALL
C
          CALL LIB$SIGNAL(D0CK_COMPILE, %VAL(1),
     &      NAME(1:LNAME)//TYPE(1:LTYPE) )
          CALL LIB$SPAWN (FORTRAN//FILENAME(III),LISTIN,LISTOUT)
C
C ****  Delete temporary file
C
          OPEN (UNIT=LUNOUT,FILE=LISTOUT,STATUS='OLD')
          CLOSE(UNIT=LUNOUT,DISP='DELETE')

        ENDIF
C
C ****  Perform checks on listing
C
        LISTING = NAME(1:LNAME)//'.LIS'
        CALL LIB$SIGNAL( D0CK_CHECKING, %VAL(1),LISTING)

        CALL D0C_SET_FILENAME (FILENAME(III))

        CALL D0C_STANDARDS(LISTING,LISTING,OK,FOUND)

        IF ( FOUND ) THEN

          IF ( OK ) THEN
            CALL LIB$SIGNAL(D0CK_CONFORMS, 
     &                      %VAL(1),
     &                      NAME(1:LNAME)//TYPE(1:LTYPE) )
            STATUS_CODE = %LOC(D0CK_CONFORMS)
          ELSE
            CALL STR$TRIM(LISTING,LISTING,LLISTING)
            CALL LIB$SIGNAL(D0CK_VIOLATES, 
     &                      %VAL(2),
     &                      NAME(1:LNAME)//TYPE(1:LTYPE), 
     &                      LISTING(1:LLISTING) )
            FAILED = .TRUE.
            STATUS_CODE = %LOC(D0CK_VIOLATES)
          ENDIF

        ELSE
          CALL LIB$SIGNAL( D0CK_NOTFOUND, %VAL(1),
     &      DEVICE(1:LDEVICE)//DIR(1:LDIR)//NAME(1:LNAME)//
     &      TYPE(1:LTYPE) )
          STATUS_CODE = %LOC(D0CK_NOTFOUND)
        ENDIF
        WRITE(6,FMT='(A)') ' '

      ENDDO
C
C ****  Delete temporary file
C
  999 CONTINUE
      OPEN (UNIT=LUNOUT,FILE=LISTIN,STATUS='OLD')
      CLOSE(UNIT=LUNOUT,DISP='DELETE')

      CALL SYS$EXIT(%VAL(STATUS_CODE))

      END
