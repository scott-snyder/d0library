C==========================================================================
      SUBROUTINE D0HDRV(IDEVI,DRVNAM)
C==========================================================================
C
C  Description:  Determines the DI3000 driver that is currently set for
C  ============  device # DEVICE.
C
C  Argument Descriptions:
C  =======================
C  IDEVI - INTEGER - Input - Device number for which you'd like info
C  CDRV  - CHARACTER*3 - Output - Device driver name.
C
C  Author:
C  ========
C  Tami Kramer
C
C  Revision History:
C  =================
C  Original Creation - August 24,1988
C-   Updated  24-MAR-1992   LUPE HOWELL  Modifycation for SGI 
C
C==========================================================================
C
      IMPLICIT NONE
C
C  Local Declarations:
C  ====================
C
      INTEGER IDEVI,ITABLE
      INTEGER*4 D0HLNM,STATUS
      CHARACTER*6 LOGNAM
      CHARACTER*19 LOGTRA
      CHARACTER*3 DRVNAM
      CHARACTER*1 CDEVI
C
C  Executable Code:
C  =================
C  
C&IF VAX
      WRITE(CDEVI,101) IDEVI
  101 FORMAT(I1)
      LOGNAM = 'YD'//CDEVI//'DRV'
      STATUS = D0HLNM(1,LOGNAM,LOGTRA)
      DRVNAM(1:3) = LOGTRA(13:15)
C&ELSEIF SIUNIX
C&      IF ( IDEVI .EQ. 1 ) THEN
C&        DRVNAM(1:3) = 'GPV'
C&      ELSEIF ( IDEVI .EQ. 2 )THEN
C&        DRVNAM(1:3) = 'PST'
C&      ENDIF
C&ENDIF
      RETURN
      END
