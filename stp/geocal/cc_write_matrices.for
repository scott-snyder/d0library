C DEC/CMS REPLACEMENT HISTORY, Element CC_WRITE_MATRICES.FOR
C *2    10-DEC-1988 15:53:24 FUESS "USE D0$INC"
C *1     8-DEC-1988 14:35:59 FUESS "Initial entry of CC geometry with Layers"
C DEC/CMS REPLACEMENT HISTORY, Element CC_WRITE_MATRICES.FOR
      SUBROUTINE CC_WRITE_MATRICES
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Write the GEANT SRCP parameters for
C-                         matrices to a file
C-
C-   Entries:
C-              CC_WRITE_MATRICES
C-              CC_ZERO_MATRICES
C-              CC_STORE_MATRIX
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
      INCLUDE 'D0$INC:CC_MATRIX_LIST.INC'
      INCLUDE 'D0$INC:CC_MATRIX.INC'
      INCLUDE 'D0$INC:CC_UNIT.INC'
C  Integers
      INTEGER NM, IM, I
C----------------------------------------------------------------------
C  Write info to file
C----------------------------------------------------------------------
      WRITE(CC_OUT_VOL,1000)
      WRITE(CC_OUT_VOL,1001) CC_NUMBER_MATRICES
      DO IM=1,CC_NUMBER_MATRICES
        WRITE(CC_OUT_VOL,1002) CC_ID_MATRIX(IM), 
     &                        (CC_VAL_MATRIX(I,IM),I=1,6)
      ENDDO
      WRITE(CC_OUT_VOL,1003)
      RETURN
C----------------------------------------------------------------------
C  Entry CC_ZERO_MATRICES
C----------------------------------------------------------------------
      ENTRY CC_ZERO_MATRICES
      CC_NUMBER_MATRICES = 0
      RETURN
C----------------------------------------------------------------------
C  Entry CC_STORE_MATRICE
C----------------------------------------------------------------------
      ENTRY CC_STORE_MATRIX
      CC_NUMBER_MATRICES = CC_NUMBER_MATRICES + 1
      NM = CC_NUMBER_MATRICES
      CC_ID_MATRIX(NM) = ID_MATRIX
      DO I=1,6
        CC_VAL_MATRIX(I,NM)  = VAL_MATRIX(I)
      ENDDO
      RETURN
C----------------------------------------------------------------------
 1000 FORMAT('\ARRAY  CC_ROT_MATRICES')
 1001 FORMAT(T10,I3)
 1002 FORMAT(I4,6F12.4)
 1003 FORMAT('\END')
      END
