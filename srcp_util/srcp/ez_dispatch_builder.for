      PROGRAM EZ_DISPATCH_BUILDER
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Build Dispatch routines from RCP-file
C-
C-   Created  25-JUN-1991   Harrison B. Prosper
C-   Updated  12-Feb-1992   Herbert Greenlee
C-      Got rid of machine block.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IER,N
      CHARACTER*32 RCP_BANK(5),RCP_FILE
      INTEGER  STATUS,LENGTH
      INTEGER  FLAGS
C----------------------------------------------------------------------
C
C ****  Get rcp-file name from command line
C
C&IF VAXVMS
      INTEGER  LIB$GET_FOREIGN
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
C
C ****  Assign default name if command line is empty
C
      IF ( LENGTH .LE. 0 ) THEN
        RCP_FILE = 'RCP$INPUT'
        LENGTH   = 9
      ENDIF
C
C ****  Setup ZEBRA
C
      CALL MZEBRA(0)
C
      CALL INZSTP
C
C ****  Read control file
C
      CALL INRCP('DISPATCH_BUILDER_RCP',IER)
      IF ( IER .NE. 0 ) THEN
        STOP ' Unable to access file DISPATCH_BUILDER_RCP'
      ENDIF
C
C ****  Read file containing menus
C
      CALL INRCP(RCP_FILE,IER)
      IF ( IER .NE. 0 ) THEN
        STOP ' Unable to access RCP file'
      ENDIF
C
C ****  Get bank name
C
      CALL INRCP_LIST(RCP_BANK,N)
C
C ****  Build dispatch routines
C
      CALL EZ_BUILD_ALL_DISPATCH('DISPATCH_BUILDER_RCP',RCP_BANK(1))
C
  999 CONTINUE
      END
