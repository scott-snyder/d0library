      SUBROUTINE GTVTXT(ITRACK,QTRAK,QHSEC,QHZLA)
C-----------------------------------------------------------------------
C
C  Returns a VTX track and associated hits: 
C
C  Input:   ITRACK       track number
C
C  Output:  QTRAK(1:21)  contains information on the fitted track
C                        candidate
C          QHSEC(4,IHSEC)    fit residual in r-z view
C
C          QHZLA(1:3,IHZLA)  not used; left in for compatibility
C
C  Daria Zieminska May 1987
C  Modified Dec. 1989: call GTVTXT_LINK
C-   Updated  29-OCT-1992   Peter M. Grudberg  Remove strips 
C-----------------------------------------------------------------------
      IMPLICIT NONE           
      INTEGER ITRACK,LVTXT,GZVTXT,LOC,LZFIND,NHSEC,NHZLA
      REAL QTRAK(21),QHSEC(4,24),QHZLA(3,6)
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'                             
C                      
      LOC=GZVTXT(ITRACK)
      CALL GTVTXT_LINK(LOC,QTRAK,QHSEC,QHZLA)
 1000 RETURN
      END
