      FUNCTION HMATRIX_FIN()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Finish up Hmatrix
C-
C-   Returned value  : True if OK
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  20-DEC-1990   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL HMATRIX_FIN
      INTEGER IER
      INCLUDE 'D0$INC:HMATRIX_PARS.INC'
      LOGICAL DO_NTUPLE
      CHARACTER*32 NTUPLE_FILE
      INTEGER NCHR
      INTEGER STATUS
C----------------------------------------------------------------------
      HMATRIX_FIN = .TRUE.
      IF ( ACCUMULATE ) THEN
        CALL HMATRIX_DO_AVERAGES          ! DO AVERAGES ETC
        CALL HMATRIX_RZ_SAVE              ! SAVE TO RZ.
        CALL RZCLOS(TOP_DIRECTORY,' ')
      ENDIF
      CALL EZPICK('HMATRIX_RCP')
      CALL EZGET('DO_NTUPLE',DO_NTUPLE,IER)
      CALL EZ_GET_CHARS('NTUPLE_FILE',NCHR,NTUPLE_FILE,IER)
      IF ( IER.EQ.-2 ) THEN
        NTUPLE_FILE = 'HBOOK_SAVE'
      ENDIF
      CALL EZRSET
      CALL NTUPLE_CLOSE(NTUPLE_FILE,STATUS)
C      CALL RZEND(TOP_DIRECTORY)
      CALL RZCLOS(TOP_DIRECTORY,' ')
  999 RETURN
      END
