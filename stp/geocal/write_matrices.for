      SUBROUTINE WRITE_MATRICES
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Write the GEANT SRCP parameters for
C-                         matrices to a file
C-
C-   Entries:
C-              WRITE_MATRICES
C-              ZERO_MATRICES
C-              STORE_MATRIX
C-
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created    10-OCT-1989   Stuart Fuess   from CC_WRITE_MATRICES
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C  Include files
      INCLUDE 'D0$INC:MATRIX_LIST.INC'
      INCLUDE 'D0$INC:MATRIX.INC'
      INCLUDE 'D0$INC:WRITE_UNIT.INC'
C  Integers
      INTEGER NM, IM, I
C----------------------------------------------------------------------
C  Write info to file
C----------------------------------------------------------------------
      WRITE(OUT_VOL,1000) MATRIX_LIST_SRCP_LABEL
      WRITE(OUT_VOL,1001) NUMBER_MATRICES
      DO IM=1,NUMBER_MATRICES
        WRITE(OUT_VOL,1002) ID_MATRIX_LIST(IM),
     &                        (VAL_MATRIX_LIST(I,IM),I=1,6)
      ENDDO
      WRITE(OUT_VOL,1003)
      RETURN
C----------------------------------------------------------------------
C  Entry ZERO_MATRICES
C----------------------------------------------------------------------
      ENTRY ZERO_MATRICES
      NUMBER_MATRICES = 0
      RETURN
C----------------------------------------------------------------------
C  Entry STORE_MATRIX
C----------------------------------------------------------------------
      ENTRY STORE_MATRIX
      NUMBER_MATRICES = NUMBER_MATRICES + 1
      NM = NUMBER_MATRICES
      ID_MATRIX_LIST(NM) = ID_MATRIX
      DO I=1,6
        VAL_MATRIX_LIST(I,NM)  = VAL_MATRIX(I)
      ENDDO
      RETURN
C----------------------------------------------------------------------
 1000 FORMAT('\ARRAY ',A32)
 1001 FORMAT(T10,I3)
 1002 FORMAT(I4,6F12.4)
 1003 FORMAT('\END')
      END
