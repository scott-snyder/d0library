      SUBROUTINE MU_SAM_WAM_CABLES(IREG,IABC,QQ_IN,QQ_OUT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-              Rearrange the bits from the SAMUS-WAMUS triplet finder,
C-       preparing them to the SAMUS-WAMUS road finder.  In the hardware,
C-       this rearrangement is done in the cables.
C-
C-   Inputs  : IREG  - 4,5 for North,South
C-             IABC  - 1,2,3 for A,B,C
C-             QQ_IN - Bit map from SAMUS-WAMUS triplet finder
C-
C-   Outputs : QQ_OUT - Bit map for the SAMUS-WAMUS road finder
C-
C-   Created   5-JUL-1992   Kamel Bazizi, Guilherme Lima
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IREG,IABC,QQ_IN(0:15,6),QQ_OUT(0:15,6)
      INTEGER II,III,QQ(0:15,6)
      INTEGER FLIPQUAD(4),FLIPSTAT(3,2),EXCHBITS(0:15),EXCHNEED(4)
C<<
C.. Flipping interchanges quadrants (1,2) and (3,4)
      DATA FLIPQUAD/2,1,4,3/
C<<
C.. The -1's are flipped (looking from the Interaction Point)
      DATA FLIPSTAT/
     &   1,  -1,   1,     ! SNA,SNB,SNC
     &   -1,   1,  -1/    ! SSA,SSB,SSC
C<<
C.. Need to exchange bits (0-6) with (7-13) in quads 1 and 3 before flipping,
C   to make quadrants (1,2) and (3,4) symmetric by flipping operation
      DATA EXCHNEED/-1,1,-1,1/
      DATA EXCHBITS/8, 9, 10, 11, 12, 13, 14, 15,
     &              0, 1,  2,  3,  4,  5,  6,  7/
C<<
C----------------------------------------------------------------------
C<<
      DO III=1,4
C<<
        DO II=0,15
          IF(EXCHNEED(III).LT.0) THEN
            QQ(EXCHBITS(II),III)=QQ_IN(II,III)
          ELSE
            QQ(II,III)=QQ_IN(II,III)
          ENDIF
        ENDDO
C<<
        DO II=0,15
          IF(FLIPSTAT(IABC,IREG-3).LT.0) THEN
            QQ_OUT(II,FLIPQUAD(III))=QQ(II,III)
          ELSE
            QQ_OUT(II,III)=QQ(II,III)
          ENDIF
        ENDDO
      ENDDO
C<<
  999 RETURN
      END
