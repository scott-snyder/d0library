C==========================================================================
      SUBROUTINE D0HFORM(CDRV,QUE,FORM)
C==========================================================================
C
C  Description:  For the DI3000 driver that is currently set for
C  ============  hardcopy, sets its form and queue 
C
C  Argument Descriptions:
C  =======================
C  CDRV  - CHARACTER*3 - Input - Device driver name.
C  QUE - CHARACTER*(*) - Output - Print queue for that device type
C  FORM - CHARACTER*(*) - OUTPUT - Graphics form used with that device  
C
C  Author:
C  ========
C  Sharon Hagopian
C
C  Revision History:
C  =================
C  Original Creation - April 23, 1991
C
C==========================================================================
C
      IMPLICIT NONE
      CHARACTER*3 CDRV
      CHARACTER*(*) QUE,FORM
C
C  Local Declarations:
C  ====================
C
      INTEGER*4 D0HLNM,STATUS
      CHARACTER*9 QLOGNAM
      CHARACTER*8 FLOGNAM
      CHARACTER*6 CPRINT
      CHARACTER*5 CFORM
C     Data Statements
C=========================================================
      DATA CPRINT/'$PRINT'/
      DATA CFORM/'$FORM'/
C
C  Executable Code:
C  =================
      QLOGNAM=CDRV//CPRINT
      FLOGNAM=CDRV//CFORM
      STATUS=D0HLNM(1,QLOGNAM,QUE)
      STATUS=D0HLNM(1,FLOGNAM,FORM)         
      IF(QUE.EQ.' ')THEN
        QUE='SYS$PRINT'
      ENDIF
      IF(FORM.EQ.' ')THEN
        IF (CDRV .EQ. 'LN3') THEN
          FORM = 'FORM_DI3000'
      ELSEIF (CDRV.EQ.'TLL'.OR.CDRV.EQ.'TLP'.OR.CDRV.EQ.'TL8')THEN
           FORM = 'DEFAULT'
        ELSEIF (CDRV .EQ. 'Q12') THEN
          FORM = 'GRAPHICS'
        ELSEIF (CDRV .EQ. 'PST') THEN
          FORM = 'POST'
        ELSE
          FORM = 'DEFAULT'
        ENDIF
      ENDIF
      RETURN
      END
