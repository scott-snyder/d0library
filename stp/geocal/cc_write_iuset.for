C DEC/CMS REPLACEMENT HISTORY, Element CC_WRITE_IUSET.FOR
C *5    30-DEC-1988 23:23:18 RAJA "GOT RID OF VPSP"
C *4    28-DEC-1988 16:13:20 FUESS "IDTYPE for each volume"
C *3    27-DEC-1988 16:12:07 FUESS "Fix format statement"
C *2    10-DEC-1988 15:52:50 FUESS "USE D0$INC"
C *1     8-DEC-1988 14:35:27 FUESS "Initial entry of CC geometry with Layers"
C DEC/CMS REPLACEMENT HISTORY, Element CC_WRITE_IUSET.FOR
      SUBROUTINE CC_WRITE_IUSET
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Write CC Detector Sets
C-
C-      Write the SRCP structures which define the Central Calorimeter
C-      Detector Sets.
C-
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created   8-DEC-1988   Stuart Fuess
C-   Updated  27-DEC-1988   Stuart Fuess  Fix format statement 
C-   Updated  28-DEC-1988   Stuart Fuess  IDTYPE for each volume 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C  Include files
      INCLUDE 'D0$INC:CC_IUSET.INC'
      INCLUDE 'D0$INC:CC_UNIT.INC'
C  Integers
      INTEGER I
C----------------------------------------------------------------------
C  Write info to file
C----------------------------------------------------------------------
      WRITE(CC_OUT_SET,1000) IUSET_LABEL
      WRITE(CC_OUT_SET,1001) IUSET_NAME
      WRITE(CC_OUT_SET,1002) IUSET_NV
      DO I=1,IUSET_NV
        WRITE(CC_OUT_SET,1003) IUSET_VOLUME_NAME(I),
     &    IUSET_IDTYPE(I),
     &    IUSET_VOLUME_NAME(I)
      ENDDO
      WRITE(CC_OUT_SET,1004)
      RETURN
 1000 FORMAT('\ARRAY  ',A32)
 1001 FORMAT(' ''',A4,'''',2X,'  256  256  0  1000')
 1002 FORMAT(I4)
 1003 FORMAT(' ''',A4,'''  ',I4,'  1  ''',A4,
     &  '''  8  1  ''DEDX''  32  0.  1000.  1  ''ADCS''  16')
 1004 FORMAT('\END')
      END
