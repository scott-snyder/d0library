      SUBROUTINE L2_HLIMIT(SIZE)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Create Storage area for Histogramming
C-
C-   Inputs  : SIZE = Size of storage area
C-   Outputs : none
C-   Controls: none
C-
C-   Created  20-FEB-1990   Jan Guida, Srini Rajagopalan
C-   Updated   3-JAN-1991   Jan Guida   Put IF(FIRST) for MZLINK
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBHST.INC'
C
C--     Zebra working space; Histogram Banks.
C
C      INTEGER NNH,NHR
C      PARAMETER (NNH=200000)
C      PARAMETER (NHR=9)
C      COMMON/ZEBHST/IXHST,IDVHST,FENHST,LCLBH,LRHST,ZHSTR
C
C      INTEGER IXHST,IDVHST,FENHST(10),ZHSTR(NNH+1)
C      REAL H(NNH)
C      INTEGER IH(NNH),LH(NNH)
C      INTEGER*2 KH(2*NNH)
C      EQUIVALENCE (LCLBH,LH(1)),(LH(9),IH(1),H(1),KH(1))
C      INTEGER LCLBH               ! working space header
C     $,  LRHST(NHR)
C

      INCLUDE 'D0$INC:L2CLBH.INC'
      INTEGER SIZE,ENDZH
      INTEGER ND,NIO
      LOGICAL FIRST
      DATA FIRST /.TRUE./
      DATA ND,NIO,HIDMX /15,2,1/
C----------------------------------------------------------------------
C
        CALL L2_INZHST(SIZE)
        CALL MZWIPE(IDVHST)
C
C  *** Create a permanent link area.
C
      IF (FIRST) THEN
        CALL MZLINK (IXHST,'/L2CLBH/',LCHST(1),LCHST(MXID),LCHST(1))
        FIRST = .FALSE.
      ENDIF
C
C  *** Create top level Header bank and fill header information.
C
      CALL MZBOOK(IDVHST,LCLBH,LCLBH,1,'CLBH',HIDMX,HIDMX,ND,NIO,0)
C
      IH(LCLBH+1) = 1                   ! Version Number
      IH(LCLBH+2) = 0                   ! Store Flag
      IH(LCLBH+3) = HIDMX               ! Number of Links requested
      IH(LCLBH+4) = 0                   ! Number of Histograms
C
  999 RETURN
      END
