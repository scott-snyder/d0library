      SUBROUTINE PMSAVE(IVIEW,ITRAK,VIEW,TRAK)
C========================================================================
C
C  Description:  Turns on device 2 (hardcopy device driver), writes the
C  ============  current retained segments to a file which is then
C                be printed on this device.
C
C  Author:
C  ========
C  Tami Kramer
C
C  Revision History:
C  ==================
C  Original Creation - March 17, 1988
C
C=======================================================================
C
      IMPLICIT NONE
C
C  Local Declarations:
C  ===================
C
      INTEGER LAS,IVIEW
      INTEGER TRAK                      ! If =1 the tracking view
      INTEGER ITRAK,VIEW
      INTEGER*4 ISTAT,LIB$SPAWN
      DATA LAS /2/
C
C  Executable Code:
C  =================
C
      CALL JDINIT(LAS)
      CALL JDEVON(LAS)
      IF (TRAK.EQ.1) THEN
        CALL PMHEAD2(ITRAK,VIEW)
        CALL PMCHNM2(ITRAK,VIEW)
        CALL PMPORT(ITRAK,VIEW)
      ELSE
        CALL PMHEAD
        CALL PMVIEW(IVIEW)
      ENDIF
      CALL JDEVOF(LAS)
      CALL JDEND(LAS)
      CALL QPRINT('LN03P.DAT','SYS$PRINT','FORM_DI3000',.FALSE.)      
      RETURN
      END
