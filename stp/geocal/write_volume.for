      SUBROUTINE WRITE_VOLUME
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Write the GEANT SRCP parameters for a
C-                         volume to a file
C-
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created   4-NOV-1988   Stuart Fuess
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C  Include files
      INCLUDE 'D0$INC:SRCP_VOLUME.INC'
      INCLUDE 'D0$INC:WRITE_UNIT.INC'
C  Integers
      INTEGER I
C----------------------------------------------------------------------
C  Write info to file
C----------------------------------------------------------------------
      WRITE(OUT_VOL,1000) VOLUME_LABEL
      WRITE(OUT_VOL,1001) VOLUME_NAME, VOLUME_SHAPE, 
     &  VOLUME_MATERIAL_CODE, VOLUME_MOTHER, POSITIONING
      WRITE(OUT_VOL,1002) ROTATION_MATRIX, COPY_NUMBER, X_POSITION,
     &  Y_POSITION, Z_POSITION, NUMBER_PARAMS
      WRITE(OUT_VOL,1003) (PARAM(I),I=1,NUMBER_PARAMS)
      WRITE(OUT_VOL,1004)
      RETURN
 1000 FORMAT('\ARRAY  ',A32)
 1001 FORMAT('''',A4,'''',2X,'''',A4,'''',2X,I5,2X,'''',A4,'''',
     &  2X,'''',A4,'''')
 1002 FORMAT(I5,2X,I5,3(2X,F10.4),2X,I5)
 1003 FORMAT(4(4X,F10.4))
 1004 FORMAT('\END')
      END
