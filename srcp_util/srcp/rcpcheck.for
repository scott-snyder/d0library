      PROGRAM RCPCHECK
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Check RCP file by doing an INRCP.
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  23-APR-1991   Harrison B. Prosper
C-   Updated  12-Feb-1992   Herbert Greenlee
C-      UNIX version.  Removed machine blocks.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IER,I,J,STATUS,LENGTH
      CHARACTER*132 RCP_FILE
      INTEGER FLAGS
C----------------------------------------------------------------------
C
C ****  Get command line
C
C&IF VAXVMS
      INTEGER  LIB$GET_FOREIGN
C
      FLAGS = 0
      STATUS = LIB$GET_FOREIGN(RCP_FILE,,LENGTH,FLAGS)
      IF ( .NOT. STATUS ) CALL LIB$SIGNAL (%VAL(STATUS))
C&ELSE
C&      INTEGER LENOCC
C&C--------------------------------------------------------------------
C&      PRINT '(A,$)',' RCP_FILE >'
C&      READ (5,'(A)') RCP_FILE
C&      LENGTH=LENOCC(RCP_FILE)
C&ENDIF
      RCP_FILE = RCP_FILE(1:LENGTH)
C----------------------------------------------------------------------
      CALL MZEBRA(0)                    ! (1)  Initialize ZEBRA
      CALL INZSTP                       ! (2)  Setup /ZEBSTP/
C
C ****  Get file-name
C
      IF ( LENGTH .LE. 0 ) THEN
        WRITE(6,FMT='('' Name of RCP file: '',$)')
        READ (5,FMT='(A)') RCP_FILE
        CALL WORD(RCP_FILE,I,J,LENGTH)
        RCP_FILE = RCP_FILE(I:J)
      ENDIF
      IF ( RCP_FILE(1:1) .EQ. ' ' ) GOTO 999
C
C ****  Check extension
C
      CALL UPCASE(RCP_FILE(1:LENGTH),RCP_FILE(1:LENGTH))
      I = INDEX(RCP_FILE(1:LENGTH),'.RCP')
      IF ( I .LE. 0 ) THEN
        CALL INTMSG(' %RCPCHECK-E-BADFILEEXT: '//RCP_FILE(1:LENGTH))
        GOTO 999
      ENDIF
C
C ****  Read RCP-file
C
      CALL EZ_ABORT_ON_BAD_VALUE(.FALSE.)
      CALL INRCP(RCP_FILE(1:LENGTH),IER)
C
      IF ( IER .NE. 0 ) THEN
        CALL INTMSG(' %RCPCHECK-E-Problem reading file: '//
     &    RCP_FILE(1:LENGTH))
      ELSE
        CALL INTMSG
     &    (' %RCPCHECK-S-SUCCESS, Normal successful completion')
      ENDIF
  999 CONTINUE
      END
