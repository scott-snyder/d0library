      SUBROUTINE ZDDROP
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : to drop all reconstructed CD banks to allow
C-                         user to REDO the reconstruction in CD from
C-                         STA files.
C-                         (It is done once per event only)
C-
C-   Inputs  : none
C-   Outputs : none
C-
C-   Created  15-JUL-1991   Qizhong Li-Demarteau
C-   Updated  20-OCT-1993   Robert E. Avery  Don't drop hits unless
C-                            there is a raw data bank available. 
C-   Updated   3-MAR-1994   Liang-ping Chen  Don't drop hits unless  
C-                            there is a raw data or Level2 hit bank available. 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'        
      INCLUDE 'D0$LINKS:IZCDD1.LINK'
      INCLUDE 'D0$LINKS:IZCDD2.LINK'
      INCLUDE 'D0$LINKS:IZCDD3.LINK'
      INCLUDE 'D0$LINKS:IZCDD4.LINK'
      INTEGER RUN, ID, RUNSAV, IDSAV
      INTEGER LVTXH, GZVTXH, LCDCH, GZCDCH, LFDCH, GZFDCH
      INTEGER LTRDH, GZTRDH, LZTRH, GZZTRH, LVERH, GZVERH
      INTEGER LCDD, LCDH, GZCDH1, GZCDH2, GZCDH3 
C
      SAVE RUNSAV,IDSAV
      DATA RUNSAV,IDSAV/-1,-1/
C----------------------------------------------------------------------
C
      CALL EVNTID(RUN,ID)
      IF (RUN .NE. RUNSAV .OR. ID .NE. IDSAV) THEN
        RUNSAV = RUN
        IDSAV = ID
C
        LCDD = LQ(LHEAD-IZCDD1)
        LCDH = GZCDH1()
        LVTXH = GZVTXH()
        IF ((LVTXH.GT.0).AND.(LCDD.GT.0.OR.LCDH.GT.0))
     &     CALL MZDROP(IXCOM,LVTXH,' ')
C
        LCDD = LQ(LHEAD-IZCDD2)
        LCDH = GZCDH2()
        LCDCH = GZCDCH()
        IF ((LCDCH.GT.0).AND.(LCDD.GT.0.OR.LCDH.GT.0))
     &     CALL MZDROP(IXCOM,LCDCH,' ')
C
        LCDD = LQ(LHEAD-IZCDD3)
        LCDH = GZCDH3()
        LFDCH = GZFDCH()
        IF ((LFDCH.GT.0).AND.(LCDD.GT.0.OR.LCDH.GT.0)) 
     &     CALL MZDROP(IXCOM,LFDCH,' ')
C
        LCDD = LQ(LHEAD-IZCDD4)
        LTRDH = GZTRDH()
        IF ((LTRDH.GT.0).AND.(LCDD.GT.0)) CALL MZDROP(IXCOM,LTRDH,' ')
C
        LZTRH = GZZTRH()
        IF (LZTRH .GT. 0) CALL MZDROP(IXCOM,LZTRH,' ')
        LVERH = GZVERH()
        IF (LVERH .GT. 0) CALL MZDROP(IXCOM,LVERH,' ')
      ENDIF
C
  999 RETURN
      END
