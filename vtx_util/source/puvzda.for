      SUBROUTINE PUVZDA(LAYERZ)
C-----------------------------------------------------------------------
C- SUBROUTINE PUVZDA(LAYERZ) eliminates the unused portion of 
C- bank VZDA after it has been filled.
C- 
C- T. Trippe, 2 Nov. 1986
C- P. Grudberg  13-DEC-1988 - changed for new VZDA format
C-   Updated  30-JAN-1990   Peter Grudberg  - IXMAIN => IXCOM in MZPUSH call 
C-----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$INC:ZEBSTP.INC/LIST'
      INCLUDE 'D0$INC:VTXLNK.INC/LIST'
      INCLUDE 'D0$LINKS:IZVZST.LINK/LIST'
      INTEGER LVZST, NBSTRP, NBEND, NWLAYR
      INTEGER NWDZDA
      INTEGER NPTZDA,MXVZDA,NWVZDA,MZHTMX
      INTEGER LAYERZ,NHTSDA
C
      LVZST = LC( LVGEH - IZVZST )
      NWLAYR = IC( LVZST + 2 )
      IF(LVZDA(LAYERZ).NE.0) THEN
         MXVZDA=IQ(LVZDA(LAYERZ)-1)
         NHTSDA=IQ(LVZDA(LAYERZ)+1)
         NWDZDA=IQ(LVZDA(LAYERZ)+3)
         NBSTRP=IC(LVZST+3+NWLAYR*LAYERZ)
         NBEND =IC(LVZST+4+NWLAYR*LAYERZ)
         NWVZDA=3+2*NBSTRP*NBEND+NWDZDA*NHTSDA 
         CALL MZPUSH(IXCOM,LVZDA(LAYERZ),0,NWVZDA-MXVZDA,'R')
      ENDIF
   20 CONTINUE
C
      RETURN
      END
