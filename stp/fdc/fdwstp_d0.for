      PROGRAM FDWSTP_D0
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Write Geometry file for the FDC
C-
C- Input: none
C-
C- Output:
C-    FDC STP-bank structure
C-
C-
C-   Created  10-MAY-1988   Jeffrey Bantly 
C-   Updated  17-FEB-1992   Susan K. Blessing  Replace OPEN with D0OPEN. 
C-    Use GTUNIT to get LUNDA.
C-   Updated  21-JUL-1992   Susan K. Blessing  Changes to allow STP files
C-    to be in exchange mode.
C-   Updated  31-JUL-1992   Qizhong Li-Demarteau  added a call to FZENDO 
C-   Updated  20-AUG-1992   Robert E. Avery   Move FSPLNK and BKSFDC
C-      initializations to this main routine.
C-   Updated  22-SEP-1992   Robert E. Avery  Change output to G format. 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBSTP.INC/LIST'
      INCLUDE 'D0$INC:QUEST.INC/LIST'
C
      INTEGER I, LUNDA, NUH, NUV 
      PARAMETER (NUV=1)
      INTEGER IUHEAD(NUV)
      INTEGER IERR
      INTEGER ILEN
C
      CHARACTER*12 XCHOP
C
      LOGICAL OK
C----------------------------------------------------------------------
C
C  Initialize the Zebra structure
C
      CALL MZEBRA ( 0 )
C
C
C  Initialize /ZEBSTP/ store
C
      CALL INZSTP
C
C  Create a permanent link area FDCPRM, if not already done
C
      CALL FSPLNK
C
C ****  Books the bank SFDC as the top level FDC bank in IXSTP
C
      CALL BKSFDC(LSFDC)
C
C ****  Create and fill FGEH and banks hanging to it : FMAT, FWAL, FDRT
C
      CALL BLFGEH_D0
C
C ****  Create and fill FALH and banks hanging to it : FALL, FALS?
C
      CALL BLFALH_D0
C
C ****  Pedestal, gain and time_to_position
C
      CALL BLFPDH
      CALL BLFGNH
      CALL BLFTMH_D0
C
C ****  Open the file LUNDA
C
      CALL GTUNIT(642,LUNDA,IERR)
      IF (IERR.NE.0) GO TO 20
C
      CALL D0OPEN(LUNDA,'FDC_D0STPFILE','OG',OK)
      IF (.NOT.OK) GO TO 20
      CALL XZRECL(ILEN,XCHOP)
C
      CALL FZFILE ( LUNDA, ILEN, XCHOP)
      IF ( IQUEST(1) .NE. 0 ) THEN
        WRITE ( 6, * ) ' ***** Problem with FZFILE :IQUEST(1)='
     &              , IQUEST(1)
        GO TO 999
      ENDIF
C
C ****  Write the output structure on file LUNDA
C
      CALL FZOUT ( LUNDA, IDVSTP, LSFDC, 1, 'D', 2, NUH, IUHEAD )
      IF ( IQUEST(1) .NE. 0 ) THEN
        WRITE ( 6,* ) ' STATUS IQUEST 1-11 TO 17 ',
     &                IQUEST(1),(IQUEST(I),I=11,17)
        CALL DZSTOR ( ' DUMP STORE', IXSTP)
        CALL DZSURV ( ' Survey data structure', IXSTP, LSTPH )
      ENDIF
C
      CALL FZENDO(LUNDA,'T')
C
      GO TO 999
   20 WRITE(6,*)  ( '******** ERROR when opening the file :OPEN')
      GO TO 999
C
C------------------------------------------------------------------------
  999 CONTINUE
      END
