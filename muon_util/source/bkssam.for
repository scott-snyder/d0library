      INTEGER FUNCTION BKSSAM()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : makes a header ZEBRA banks for SAMUS constants
C-
C-   Returned value  : SAMUS constants header bank address
C-   Inputs  : None
C-   Outputs : None
C-   Controls:
C-
C-   Created  30-SEP-1990   V. Glebov & A. Efimov & V. Podstavkov
C-   Updated  21-APR-1991   Vladimir Glebov  ! Move SSAM under STPC 
C-   Updated  07-May-1993   Evgeny Kozlovsky  MZFORM was called 
c-                                            to corect format
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZSTPO.LINK'
      INCLUDE 'D0$LINKS:IZSTPC.LINK'
      INCLUDE 'D0$LINKS:IZSTPN.LINK'
      INCLUDE 'D0$LINKS:IZSSAM.LINK'
      INTEGER NWORDS, NLINKS
      INTEGER KSSAM, LSTP
      INTEGER NFORM
C----------------------------------------------------------------------
      LOGICAL FIRST
      DATA FIRST /.TRUE./
C----------------------------------------------------------------------
      CHARACTER*4  PATH 
      CHARACTER*80 MSGSTR               ! Error message
C----------------------------------------------------------------------
C
      BKSSAM = 0
C                   find the bank STPH
      IF (LSTPH .EQ. 0) THEN            ! Bank STPH doesn't exist
        MSGSTR = ' BKSSAM: Bank STPH does not exist '
        CALL INTMSG (MSGSTR)
        RETURN
      ENDIF
C
      CALL CPATHG(PATH)                 ! get path name
      IF (PATH.EQ.'STPO') THEN
        LSTP = LC(LSTPH-IZSTPO)
      ELSE IF (PATH.EQ.'STPC') THEN
        LSTP = LC(LSTPH-IZSTPC)
      ELSE IF (PATH.EQ.'STPN') THEN
        LSTP = LC(LSTPH-IZSTPN)
      ELSE
        WRITE(MSGSTR,10) PATH
  10    FORMAT(' PATH name ',A4,' is not valid')
        CALL ERRMSG('PATH','BKSSAM',MSGSTR,'E')
      ENDIF
C
C     check existance of the banks
C
      IF (LSTP .EQ. 0) THEN             ! Bank STP* doesn't exist
        MSGSTR = ' BKSSAM: Top level bank does not exist '
        CALL INTMSG (MSGSTR)
        RETURN
      ENDIF
C
      KSSAM = LC(LSTP - IZSSAM)         ! SAMUS header bank address
      IF (KSSAM .NE. 0) THEN            ! bank already exist
        MSGSTR = ' BKSSAM: Bank SSAM already exist '
        CALL INTMSG (MSGSTR)
        RETURN
      ENDIF
C
C     creation SAMUS constant header bank
C
C
      IF(FIRST) THEN
        CALL MZFORM('SSAM','9I 1F', NFORM )
        FIRST = .FALSE.
      ENDIF
C
      NLINKS = 8
      NWORDS = 10
      CALL MZBOOK (IDVSTP, KSSAM, LSTP, -IZSSAM, 'SSAM',
     +             NLINKS, NLINKS, NWORDS, NFORM ,0 )
C                              
      BKSSAM = KSSAM
  999 RETURN
      END
