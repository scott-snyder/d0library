      SUBROUTINE PUVZLA(LAYERZ)
C-----------------------------------------------------------------------
C- SUBROUTINE PUVZLA(LAYERZ) eliminates the unused portion of
C- bank VZLA after it has been filled.
C- 
C- T. Trippe, 16 May. 1987
C-----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$INC:VTXLNK.INC/LIST'
      INTEGER LAYERZ,NHITS,NWVZLA,NWDSHT,MXVZLA
C
      IF(LVZLA(LAYERZ).NE.0) THEN
         NHITS =IQ(LVZLA(LAYERZ)+1)
         NWDSHT=IQ(LVZLA(LAYERZ)+3)
         NWVZLA=3+NWDSHT*NHITS            ! number of data words used
         MXVZLA=IQ(LVZLA(LAYERZ)-1)       ! number of data words booked
         CALL MZPUSH(IXCOM, LVZLA(LAYERZ),0,NWVZLA-MXVZLA,'R')
      ENDIF
C
      RETURN
      END
