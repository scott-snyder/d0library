      SUBROUTINE d0spythia_susyp
C-----------------------------------------------------------------------
C-
C-   Purpose and Methods: Load susy parameters from file given in RCP
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  12-NOV-1995   Adam L. Lyon
C-
C-----------------------------------------------------------------------
      IMPLICIT NONE

      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$SPYTHIA$INC:PARSUSY.INC'

      INTEGER gtunit_id, lun, ier, i
      CHARACTER*80, fname

C-----------------------------------------------------------------------

C*****Get the unit number to use
      CALL EZPICK('SPYTHIA_RCP')
      CALL EZGET('GTUNIT_ID',gtunit_id,ier)

      CALL EZ_FILE_OPEN(gtunit_id,'SUSYPARAM_FILE','IF',lun,fname,ier)
      IF ( ier .NE. 0 ) CALL ERRMSG('SUSYFILE','d0spythia_susyp',
     &     'Cannot open susy parameter file','F')

      DO i=1,94

        IF ( i .NE. 7 ) THEN
          READ(lun,100,END=500) psusy(i)
C&IF SIUNIX
 100      FORMAT(F14.3)
C&ELSE
C& 100      FORMAT(F15.3)
C&ENDIF
        ELSE
          READ(lun,101,END=500) psusy(i)
C&IF SIUNIX
 101      FORMAT(G9.5)
C&ELSE
C& 101      FORMAT(G10.5)
C&ENDIF
        ENDIF

      ENDDO

      CLOSE(lun)
      CALL RLUNIT(gtunit_id,lun,ier)

      GOTO 999

 500  CALL ERRMSG('SUSYPERR','d0spythia_susyp',
     &     'SUSY parameter file too short','F')

  999 RETURN
      END
