      SUBROUTINE QCD_UPK_VERT
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Get Vertex quality info from VERT bank
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created   5-MAR-1996   Brent J. May/Andrew Brandt
C-
C-   Relevant words in VERT bank:
C-   ---------------------------
C        +2   B   status:
C                 bit 31: 1 if main primary vertex
C                 bit 30: 1 if additional primary vertex
C                 bit 29: 1 if secondary vertex
C                 bit 28: 1 if vertex found by VTX hits
C                 bit 27: 1 if vertex found by CDC hits  (method = 1)
C                 bit 26: 1 if vertex found by FDC hits
C                 bit 25: 1 if vertex found by CDC track (method = 2)
C                 bit 24: 1 if vertex found by CDC track (method = 3)
C                 bit 23: 1 if X-Y from online; 0 if from RCP
C                 bits 16 - 22 to be defined
C                 bits 8-15: number of tracks contributed to this vertex
C                 bits 0-7: weight (percentage of track number for this vertex)
C        +5   F   Z
C        +8   F   DZ
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE  'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:QCD_JUTL_HEAD.INC/LIST'
C
      INTEGER ITEMP, IVERT, LVERT, GZVERT, I
      EXTERNAL GZVERT
C----------------------------------------------------------------------
C
C- Initialize
C
      IVERT = 0
      DO I = 1,3
        VERT_Z(I) = -999.
        VERT_DZ(I) = -999.
        VERT_WGHT(I) = -1
        VERT_NTRKS(I) = -1
        VERT_MTHD(I) = -1
      END DO
C
      LVERT = GZVERT(1)
      DO WHILE(IVERT .LT. MAXV .AND. LVERT .NE. 0)
C
C- For completeness, get vertex and error in z,x,y
C
        IVERT = IVERT + 1
        VERT_Z(IVERT) = Q(LVERT+5)
        VERT_DZ(IVERT) = Q(LVERT+8)
C
C- Get number of reco objects pointing to vertex (range [0,31])
C
        ITEMP = IQ(LVERT+0)
C
C- Get number of tracks etc for vertex
C
        ITEMP = IQ(LVERT+2)
        VERT_WGHT(IVERT) = IBITS(ITEMP,0,8)   ! range [0,100]
        VERT_NTRKS(IVERT) = IBITS(ITEMP,8,8)  ! range [0,255]
        VERT_MTHD(IVERT) = IBITS(ITEMP,24,5)  ! range [0,31]
        LVERT = LQ(LVERT)
      END DO
      NZ = IVERT
C
  999 RETURN
      END
