C+
      PROGRAM SAWSTP
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Write STP-file with the SAMUS constants
C-
C-   Input: none
C-
C-   Output: SAMUS STP-bank file
C-
C-   Created  28-SEP-1990   V. Glebov & A. Efimov & V. Podstavkov
C-   Updated  22-APR-1991   Vladimir Glebov  ! Move SSAM under STPC
C-   Updated  16-NOV-1992   Alexander Efimov   
C-   Updated   6-JAN-1993   Vladimir Glebov  ! Replace OPEN on D0OPEN 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC/LIST'
      INCLUDE 'D0$INC:QUEST.INC/LIST'
      INTEGER I, LUNDA, NUH, NUV
      PARAMETER (NUV=1)
      INTEGER IUHEAD(NUV)
      INTEGER LSSAM, GZSSAM
      LOGICAL ok
      DATA LUNDA / 9 /
C
C ****  Initialize the Zebra structure
C
      CALL MZEBRA (0)
C
C ****  Initialize /ZEBSTP/ store
C
      CALL INZSTP
C
C ****  Create and fill SAMUS banks
C
      CALL BKSSAM                       ! SAMUS header bank
      CALL BKSSTH ! geometry constants for SAMUS drift tube stations
      CALL BKSMAH ! geometry constants for SAMUS magnets
      CALL BKSBPH ! geometry constants for collimators, quadrupoles ...
C
C ****  Open the file LUNDA
C
      CALL D0OPEN (LUNDA, 'SAM_STPFILE', 'OU', OK)
      IF(.NOT.OK)GO TO 20
      CALL FZFILE (LUNDA, 0, 'O')
      IF (IQUEST(1) .NE. 0 ) THEN
        WRITE ( 6, * ) ' ***** Problem with FZFILE :IQUEST(1)='
     &              , IQUEST(1)
        GO TO 999
      END IF
C
C ****  Write the output structure on file LUNDA
C
      LSSAM = GZSSAM()
      NUH = 0
      CALL FZOUT (LUNDA, IDVSTP, LSSAM, 1, 'D', 1, NUH, IUHEAD )
      IF (IQUEST(1) .NE. 0) THEN
        WRITE (6, *) ' STATUS IQUEST 1-11 TO 17 ',
     &                IQUEST(1),(IQUEST(I),I=11,17)
        CALL DZSTOR (' DUMP STORE', IXSTP)
        CALL DZSURV (' Survey data structure', IXSTP, LSTPH)
        GO TO 999
      ENDIF
      GO TO 777
   20 WRITE(6,*)  ( '******** ERROR when opening the file :OPEN')
      GO TO 999
C
  777 CONTINUE
      STOP 'Job finished OK'
  999 CONTINUE
      STOP 'Job failed'
      END
