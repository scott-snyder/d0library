      SUBROUTINE ECEM_MISC_GEOM
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Compute ECEM geometry for GEANT 
C-
C-   Here the elements which are common to the plate level
C-   and homogenous geometry are defined.  These are:
C-      1) EM1&2 readout ring (G10 + Copper)
C-      2) Stainless steel strongback
C-      3) Stainless steel skin on back of EM4
C-
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created  13-MAR-1990   Natalie Roe 
C-   Updated  30-SEP-1990   Andrew Milder  Changed strongback from E11A to E41B
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C  Include files
      INCLUDE 'D0$INC:SRCP_VOLUME.INC'
C  Local Variables
      INTEGER ICODE,IER,I,J,K          
      REAL CM_PER_INCH,MOTHERZ,Z_BEGIN,VAL(4),MLO,MHI
      PARAMETER ( CM_PER_INCH = 2.54 )
      CHARACTER*4 MATERIAL_CODE,NAME
      LOGICAL FIRST/.TRUE./       
      INTEGER ICHARR
      CHARACTER*4 CHARR
      EQUIVALENCE (ICHARR,CHARR)
C----------------------------------------------------------------------
D     WRITE(6,*)'IN ECEM_MISC_GEOM'
C----------------------------------------------------------------------
      CALL EZPICK('ENDCAP')
      CALL EZGETA('ECEM+Z_MODULE_VOLUME',5,5,1,MLO,IER)
      CALL EZGETA('ECEM+Z_MODULE_VOLUME',11,11,1,MHI,IER)
      MOTHERZ=(MLO+MHI)/2.
C----------------------------------------------------------------------
C  SUPPORT PIPE,  EM 1&2 READOUT BOARD, STRONGBACK
C----------------------------------------------------------------------
      DO I=1,4
       IF(I.EQ.1)VOLUME_LABEL='ECEM+Z_SUPPORT_PIPE'
       IF(I.EQ.2)VOLUME_LABEL='ECEM+Z_READOUT_12_MLB' 
       IF(I.EQ.3)VOLUME_LABEL='ECEM+Z_READOUT_12_COPPER' 
       IF(I.EQ.4)VOLUME_LABEL='ECEM+Z_11_ABSORBER' 
       CALL EZGETA_i(VOLUME_LABEL,1,1,1,VOLUME_NAME,IER)
       IF(IER.NE.0)WRITE(6,1000)IER,'NAME'
       IF (I.EQ.4) THEN
         CHARR= 'E41B'
         VOLUME_NAME = ICHARR
         CALL UCTOH('POSP',POSITIONING,4,4)
       ELSE
         CALL UCTOH('POS',POSITIONING,4,3)
       ENDIF
       CALL EZGETA_i(VOLUME_LABEL,2,2,1,ICODE,IER) 
       IF(IER.NE.0)WRITE(6,1000)IER,'ICODE'
       CALL UHTOC(ICODE,4,MATERIAL_CODE,4)
       CALL EZGET(MATERIAL_CODE,VOLUME_MATERIAL_CODE,IER)
       IF(IER.NE.0)WRITE(6,1000)IER,'MATERIAL'
       CALL EZGETA_i(VOLUME_LABEL,3,3,1,VOLUME_MOTHER,IER) 
       IF(IER.NE.0)WRITE(6,1000)IER,'MOTHER'
       CALL UCTOH('TUBE',VOLUME_SHAPE,4,4)               
       NUMBER_PARAMS=3 
       IF (I.LE.3) THEN
         CALL EZGETA(VOLUME_LABEL,4,7,1,VAL(1),IER)
         PARAM(1)= VAL(1)  ! inner radius
         PARAM(2)= VAL(2)  ! outer radius
         PARAM(3)= (VAL(4)-VAL(3))/2. ! half length
       ELSE
         CALL EZGETA(VOLUME_LABEL,4,6,1,VAL(1),IER)
         PARAM(1)= VAL(1)  ! inner radius
         PARAM(2)= VAL(2)  ! outer radius
         PARAM(3)= VAL(3)/2. ! half length
         CALL EZGETA('ECEM+Z_FLOOR4_VOLUME',6,6,1,VAL(4),IER)
         VAL(3)=VAL(4)-PARAM(3)*2.
       ENDIF  
       IF(IER.NE.0)WRITE(6,1000)IER,'PARAMS'
       DO J=1,NUMBER_PARAMS
        PARAM(J)=PARAM(J)*CM_PER_INCH
       ENDDO
       ROTATION_MATRIX = 1               ! Identity
       COPY_NUMBER=1
       X_POSITION=0.
       Y_POSITION=0.
       Z_POSITION=CM_PER_INCH*( (VAL(3)+VAL(4))/2. - MOTHERZ)
       CALL WRITE_VOLUME
      ENDDO

C----------------------------------------------------------------------
 1000 FORMAT(' IER EQ',I3,' IN EZGET FOR ',A12,' IN ECEM_PLATE_GEOM')
D1001 FORMAT('MODULE ',I3,' PLATE ',I3,': PARAM ',I3,' = ',F12.6)
 1002 FORMAT('ECEM+Z_',I2.2,'_',A8)
D1003 FORMAT(' Z AT END OF CELL',I3,' = ',F12.6)
C----------------------------------------------------------------------
      RETURN
      END
