      SUBROUTINE GTMSEG(IMUON,ISEG,IABC,NMOD,NHIT,IORIENT,BUF,
     1  SUBS,A1,B1,D1,CHI21,A2,B2,D2,CHI22,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Get contents of segment bank ISEG under MUON
C-   bank IMUON
C-
C-   Inputs  : IMUON  =  MUON track number
C-             ISEG   =  segment number
C-
C-   Outputs :
C       IABC - I    Segment Flag (0 A, 1 BC)
C       NMOD - I    Number of module belonging to the segment
C       NHIT - I    Number of hit belonging to the segment
C       IORIENT- I    Orientation
C       BUF(5) -F    Reserved
C       SUBS - F    Artificial offset
C       A1 - F    A1
C       B1 - F    B1
C       D1(2,2) - F    Error matrix for A1 and B1
C       CHI21 - F    Chi square
C       A2 - F    A2
C       B2 - F    B2
C       D2 - F    Error matrix for A2 and B2
C       CHI22 - F    Chi square
C       IER - I error flag
C-   Controls:
C-
C-   Created  19-MAY-1993   Regina Demina
C-   Updated  14-SEP-1993   Daria Zieminska   
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INTEGER IMUON,ISEG, IBANK
      INTEGER IPOINT, IER, IPOINT1
      INTEGER  GZMSEG
      INTEGER  IABC, NMOD, NHIT, IHT, NHIT_MAX, IHOFF, IORIENT
      REAL BUF(5),SUBS
      REAL B1, A1, D1(2,2), CHI21
      REAL B2, A2, D2(2,2), CHI22
C
C=======================================================================
      IER = 0
C
C  link to first MSEG
C
      IPOINT1 = GZMSEG(IMUON)
      IPOINT = IPOINT1
      IF(IPOINT.EQ.0) THEN
        IER = 1
        GOTO 999
      ENDIF
      IF(ISEG.GT.1) THEN
        DO IBANK = 1,ISEG-1
          IPOINT1 = IPOINT
          IPOINT = LQ(IPOINT1)
        ENDDO
      ENDIF
      IF(IPOINT.EQ.0) THEN
        IER = 1
        GOTO 999
      ENDIF
C
      IABC = IQ(IPOINT + 1)
      NMOD = IQ(IPOINT + 2)
      NHIT = IQ(IPOINT + 3)
      IORIENT = IQ(IPOINT + 4)
      BUF(1) = Q(IPOINT + 5)
      BUF(2) = Q(IPOINT + 6)
      BUF(3) = Q(IPOINT + 7)
      BUF(4) = Q(IPOINT + 8)
      BUF(5) = Q(IPOINT + 9)
      SUBS= Q(IPOINT + 10)
      A1 = Q(IPOINT + 11)
      B1 = Q(IPOINT + 12)
      D1(1,1) = Q(IPOINT + 13)
      D1(1,2) = Q(IPOINT + 14)
      D1(2,1) = Q(IPOINT + 15)
      D1(2,2) = Q(IPOINT + 16)
      CHI21 = Q(IPOINT + 17)
      A2 = Q(IPOINT + 18)
      B2 = Q(IPOINT + 19)
      D2(1,1) = Q(IPOINT + 20)
      D2(1,2) = Q(IPOINT + 21)
      D2(2,1) = Q(IPOINT + 22)
      D2(2,2) = Q(IPOINT + 23)
      CHI22 = Q(IPOINT + 24)
  999 RETURN
      END
