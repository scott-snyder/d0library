      SUBROUTINE WRITE_IUSET
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Write Detector Sets
C-
C-      Write the SRCP structures which define the Detector Sets.
C-
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created  11-OCT-1989   Stuart Fuess   
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C  Include files
      INCLUDE 'D0$INC:IUSET.INC'
      INCLUDE 'D0$INC:WRITE_UNIT.INC'
C  Integers
      INTEGER I
C----------------------------------------------------------------------
C  Write info to file
C----------------------------------------------------------------------
      WRITE(OUT_SET,1000) IUSET_LABEL
      WRITE(OUT_SET,1001) IUSET_NAME
      WRITE(OUT_SET,1002) IUSET_NV
      DO I=1,IUSET_NV
        WRITE(OUT_SET,1003) IUSET_VOLUME_NAME(I),
     &    IUSET_IDTYPE(I),
     &    IUSET_VOLUME_NAME(I)
      ENDDO
      WRITE(OUT_SET,1004)
      RETURN
 1000 FORMAT('\ARRAY  ',A32)
 1001 FORMAT(' ''',A4,'''',2X,'  256  256  0  1000')
 1002 FORMAT(I5)
 1003 FORMAT(' ''',A4,'''  ',I5,'  1  ''',A4,
     &  '''  8  1  ''DEDX''  32  0.  1000.  1  ''ADCS''  16')
 1004 FORMAT('\END')
      END
