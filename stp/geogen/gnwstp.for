      PROGRAM GNWSTP
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Write Geometry file for the BEAM PIPES and for the
C-                         GEANT volumes
C-
C- Input:
C-
C- Output:
C-    IERR       error code
C-    STP-bank structure
C-
C-
C-   Created   2-AUG-1988   Ghita Rahal-Callot
C-   Updated  19-JUL-1989   Rajendran Raja  
C-   CHANGED Logic of banks. INZSTP called and SGEN in its normal place
C-   Updated  20-Feb-1992   Herbert Greenlee
C-      UNIX compatible version.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC/LIST'
      INCLUDE 'D0$INC:QUEST.INC/LIST'
      INCLUDE 'D0$LINKS:IZSGEN.LINK'
      INCLUDE 'D0$LINKS:IZSTPC.LINK'
      INTEGER LUNDA, NUH, NUV, I ,IERR ,LSGEN ,LSTPC
      PARAMETER (NUV=1)
      INTEGER IUHEAD(NUV)
      LOGICAL OK
      DATA LUNDA / 9 /
C----------------------------------------------------------------------
C
C ****  Initialize the Zebra structure
C
      CALL MZEBRA(0)                    ! INITIALIZE ZEBRA
      CALL INZSTP
      CALL INRCP('GNWSTP_RCP',IERR)    ! Read in RCP file
      IF(IERR.NE.0)CALL ERRMSG('GNWSTP','GNWSTP',
     &  ' ERROR OPENING RCP FILE ','F')
      CALL EZPICK('GNWSTP_RCP')
C
C ****  Create and fill DGEH and banks hanging to it : SGBP  SGMC
C
      CALL BLSGEN
C
C ****  Open the file LUNDA
C
      CALL EZRSET
C
      CALL D0OPEN(LUNDA,'GEN_STPFILE','OU',OK)
      IF(.NOT.OK)GO TO 20
      CALL FZFILE ( LUNDA, 0, 'O')
      IF ( IQUEST(1) .NE. 0 ) THEN
        WRITE ( 6, * ) ' ***** Problem with FZFILE :IQUEST(1)='
     &              , IQUEST(1)
        GO TO 999
      ENDIF
C      CALL FZENDI ( LUNDA, 'O')
C
C ****  Write the output structure on file LUNDA
C
      LSTPC = LC(LSTPH-IZSTPC)
      LSGEN = LC(LSTPC-IZSGEN)
      CALL FZOUT ( LUNDA, IDVSTP, LSGEN, 1, ' ', 2, NUH, IUHEAD )
      IF ( IQUEST(1) .NE. 0 ) THEN
        WRITE ( 6,* ) ' STATUS IQUEST 1-11 TO 17 ',
     &                IQUEST(1),(IQUEST(I),I=11,17)
        CALL DZSTOR ( ' DUMP STORE', IXSTP)
        CALL DZSURV ( ' Survey data structure', IXSTP, LSTPH )
      ENDIF
      GO TO 999
   20 WRITE(6,*)  ( '******** ERROR when opening the file :OPEN')
      GO TO 999
  999 CONTINUE
      END
