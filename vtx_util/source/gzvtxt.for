      FUNCTION GZVTXT(ITRACK)
C-------------------------------------------------------------------
C
C  Returns pointer to Zebra bank VTXT for VTX track ITRACK
C
C    D.Z. MAY 1987
C    P.G. OCT 1991 Handle ITRACK=0
C-   Updated  10-SEP-1992   sandor Feher  fixed for ITRACK=0
C                         (return bank right under VTRH for ITRACK=0)
C------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$LINKS:IZZTRH.LINK/LIST'
      INCLUDE 'D0$LINKS:IZVTRH.LINK/LIST'
      INCLUDE 'D0$LINKS:IZVTXT.LINK/LIST'
      INTEGER GZVTXT
      INTEGER GZZTRH,LZTRH,LVTRH,ITRACK,LVTXT,LZFIND
C-------------------------------------------------------------------
      GZVTXT=0
      LZTRH=GZZTRH()
      IF(LZTRH.LE.0)GO TO 999
      LVTRH=LQ(LZTRH-IZVTRH)
      IF(LVTRH.LE.0)GO TO 999
      LVTXT=LQ(LVTRH-IZVTXT)
C
      GZVTXT=LVTXT       
C
      IF ( LVTXT.NE.0 .AND. ITRACK.NE.0 )
     &  GZVTXT=LZFIND(IXCOM,LVTXT,ITRACK,-5)
  999 RETURN
      END
