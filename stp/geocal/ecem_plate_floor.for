      SUBROUTINE ECEM_PLATE_FLOOR
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Create the GEANT ECEM Floor volumes 
C-                         appropriate for a plate-level description
C-
C-      Each Floor volume will be a 'CONE' with inner radius given 
C-      by the outer radius of the cryostat inner cold wall, outer 
C-      radius given by the EM/OH boundary, length given by the EM 
C-      module lengths, and filled with liquid argon.
C-
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created  27-NOV-1989   Natalie Roe
C-   Updated   2-DEC-1989   Stuart Fuess  Module-->Floor 
C-   Updated  14-DEC-1989   Stuart Fuess  Was TBM_90_ECEM_MODULE 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C  Include files
      INCLUDE 'D0$INC:SRCP_VOLUME.INC'
C  Integers
      INTEGER IER,I,J,ICODE 
C  Parameters
      REAL CM_PER_INCH,ZHILO(2),MLO,MHI,MOTHERZ
      PARAMETER ( CM_PER_INCH = 2.54 )
      CHARACTER*4 MATERIAL_CODE
C----------------------------------------------------------------------
D     WRITE(6,*)'IN TBM_90_ECEM_MODULE'
C----------------------------------------------------------------------
      CALL EZPICK('ENDCAP')
C----------------------------------------------------------------------
      DO I=1,4
       WRITE(VOLUME_LABEL,1002)I
       CALL EZGETA_i(VOLUME_LABEL,1,1,1,VOLUME_NAME,IER)
       IF(IER.NE.0)WRITE(6,1000)IER,'NAME'
       CALL EZGETA_i(VOLUME_LABEL,2,2,1,ICODE,IER) 
       IF(IER.NE.0)WRITE(6,1000)IER,'ICODE'
       CALL UHTOC(ICODE,4,MATERIAL_CODE,4)
       CALL EZGET(MATERIAL_CODE,VOLUME_MATERIAL_CODE,IER)
       IF(IER.NE.0)WRITE(6,1000)IER,'MATERIAL'
       CALL EZGETA_i(VOLUME_LABEL,3,3,1,VOLUME_MOTHER,IER) 
       IF(IER.NE.0)WRITE(6,1000)IER,'MOTHER'
       CALL UCTOH('CONE',VOLUME_SHAPE,4,4)               
       CALL UCTOH('POS',POSITIONING,4,3)               
       NUMBER_PARAMS=5 
       CALL EZGETA(VOLUME_LABEL,6,7,1,PARAM(2),IER)
       CALL EZGETA(VOLUME_LABEL,9,10,1,PARAM(4),IER)
       CALL EZGETA(VOLUME_LABEL,5,8,3,ZHILO(1),IER)
       PARAM(1)=(ZHILO(2)-ZHILO(1))/2.
       IF(IER.NE.0)WRITE(6,1000)IER,'PARAMS'
       DO J=1,NUMBER_PARAMS
D       WRITE(6,1001)I,J,PARAM(J)
        PARAM(J)=PARAM(J)*CM_PER_INCH
       ENDDO
       ROTATION_MATRIX = 1               ! Identity
       COPY_NUMBER=1
       CALL EZGETA('ECEM+Z_MODULE_VOLUME',5,5,1,MLO,IER)
       CALL EZGETA('ECEM+Z_MODULE_VOLUME',11,11,1,MHI,IER)
       MOTHERZ=(MLO+MHI)/2.
       X_POSITION=0.
       Y_POSITION=0.
       Z_POSITION=CM_PER_INCH*( (ZHILO(1)+ZHILO(2))/2. - MOTHERZ)
       CALL WRITE_VOLUME
      ENDDO
C----------------------------------------------------------------------
 1000 FORMAT('IER EQ',I3,' IN EZGET FOR ',A12)
D1001 FORMAT('FLOOR ',I3,': PARAM ',I3,' = ',F12.6)
 1002 FORMAT('ECEM+Z_FLOOR_',I1.1,'_VOLUME')
C----------------------------------------------------------------------
      RETURN
      END
