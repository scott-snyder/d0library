      SUBROUTINE FILE_NAME(FNAME,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : to provide a file name to the calling routine
C-
C-   Outputs : 
C-      FNAME = file name, 
C-      IER   = 0 good file, 1 bad file, 2 no more files
C-
C-   Controls: ASCI file with a list of files poited to by logical FILE_NAME
C-      single file per entry is assumed, 
C-      the line should fully define the file, directory and device if needed
C-
C-   Created  11-MAR-1992   Krzysztof L. Genser
C-   Updated   3-APR-1992   Krzysztof L. Genser  Declare DATA OPENED /.FALSE./ 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE

      INCLUDE 'D0$INC:FATCOM.INC/LIST'

      CHARACTER*(*) FNAME

      LOGICAL OPENED,OK
      SAVE OPENED
      DATA OPENED /.FALSE./

      INTEGER IER
      INTEGER INUNIT

      CHARACTER*255 CHLINE
      CHARACTER*255 CMSG

      INTEGER ISPACE,ISEMIC
      INTEGER IOSTAT

      CHARACTER*255 SPACES
      INTEGER LENOCC
C----------------------------------------------------------------------
C
C ****  was the file list opened so far ?
C
      IER=0
      IF ( .NOT. OPENED ) THEN
C
C ****  declare the file as opened
C
        OPENED = .TRUE.
C
C ****  get free unit
C
        CALL GTUNIT(87,INUNIT,IER)
        IF ( IER.NE.0 ) THEN
          CALL ERRMSG('FILE_NAMES','FILE_NAME',
     &      'Can not get INUNIT','I')
          IER=1
          RETURN
        ENDIF
C
C ****  open the ASCI file
C
        CALL D0OPEN (INUNIT,'FILE_NAMES','FI',OK)
        IF ( .NOT. OK ) THEN
          CALL ERRMSG('FILE_NAMES','FILE_NAME',
     &      'Can not open file','I')
          IER=1
          RETURN
        ENDIF
      ENDIF

1992  CONTINUE

C
C ****  read next line to the CHLINE
C
      READ(INUNIT,FMT='(A)',END=1001,ERR=1002,IOSTAT=IOSTAT) CHLINE
C
C ****  is it non empty string ?
C
      IF ( LENOCC(CHLINE) .LT. 1 ) THEN
         GOTO 1992
      ENDIF
C
C ****  strip blanks
C
      CHLINE=SPACES(CHLINE,1)
C
C ****  is it not a comment ?
C
      
      IF ( CHLINE(1:1) .EQ. '!' .OR. CHLINE(1:1) .EQ. '*') THEN
         GOTO 1992
      ENDIF
C
C ****  parse the line
C
C ****  assume first continous string to be the file name
C
      ISPACE=INDEX(CHLINE,' ')
      IF ( ISPACE.NE.0 ) THEN
        CHLINE=CHLINE(1:ISPACE-1)
      ENDIF
C
C ****  strip the version number if any
C
      ISEMIC=INDEX(CHLINE,';')
      IF ( ISEMIC.NE.0 ) THEN
        CHLINE=CHLINE(1:ISEMIC-1)
      ENDIF
C
C ****  this should be it; return the string to the calling routine
C
      FNAME=CHLINE
      RETURN

 1001 CONTINUE
C
C ****  end of file
C
      OPENED = .FALSE.
      CLOSE(INUNIT)
      CALL RLUNIT(87,INUNIT,IER)
      IER=2
      RETURN

 1002 CONTINUE
C
C ****  error while reading
C
      WRITE (CMSG,'(I10)') IOSTAT
      CMSG='READ returned IOSTAT value '//
     &    CMSG(1:LENOCC(CMSG))//' '
      CALL ERRMSG('FILE_NAMES','FILE_NAME',
     &    CMSG(1:LENOCC(CMSG)),'I')
      OPENED = .FALSE.
      CLOSE(INUNIT)
      CALL RLUNIT(87,INUNIT,IER)
      IER=1
      RETURN
      END
