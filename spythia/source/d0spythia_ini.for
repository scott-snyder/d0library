      SUBROUTINE D0SPYTHIA_INI
C-----------------------------------------------------------------------
C-
C-   Purpose and Methods: Initialize stuff
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  10-NOV-1995   Adam L. Lyon
C-
C-----------------------------------------------------------------------
      IMPLICIT NONE

      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ISAUNT.INC'
      INCLUDE 'D0$INC:ITAPES.INC'

      INTEGER IEVTAP, GTUNIT_ID, IER, ilen
      CHARACTER*80 FILEVT
      CHARACTER*4 OPCHOP
      
C-----------------------------------------------------------------------

C*****initialize zebra

      CALL INZBRA
      CALL INZCOM(2)
      CALL INPAWC

C*****Load some rcp stuff

      CALL INRCP ('SPYTHIA_RCP',IER)
      IF(IER.NE.0)CALL ERRMSG('RCP','D0SPYTHIA_INI',
     &  'CANNOT OPEN RCP FILE ','F')

      CALL EZPICK('SPYTHIA_RCP')
      CALL EZGET('GTUNIT_ID',GTUNIT_ID,IER)
      

C ****  OUTPUT FILE
C
      CALL EZ_FILE_OPEN(GTUNIT_ID,'OUTPUT_FILE','XO',
     &  ITEVT,FILEVT,IER)
      IF(IER.NE.0)THEN
        CALL ERRMSG('PYTHRA','PYTHRA',
     &  'COULD NOT OPEN EVENT OUTPUT FILE','W')
      ENDIF
      CALL XZRECL(ilen, opchop)
      CALL FZFILE (ITEVT,ilen,opchop)
C
      IEVTAP=-ITEVT     ! no provision for handling unstable particles
C
C          Setup for ZEBRA only
C
      ISUNIT=IABS(IEVTAP)
C
      CALL EZSET('OUTPUT_UNIT',ITEVT,IER)
C
      CALL PJET_RCP('SPYTHIA_RCP')

  999 RETURN
      END
