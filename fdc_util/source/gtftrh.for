      SUBROUTINE GTFTRH(ICONT)
C-----------------------------------------------------------------  
C          
C  Purpose and Methods : Fetch contents of Zebra bank FTRH 
C                        (Head bank for tracks in Forward Drift Chamber ) 
C
C  Output: ICONT 
C        1   I   Version number (=1) 
C        2   I   Total number of tracks in FDC
C        3   F   Z0(1) = Z point for FDCT track calculations, Half 0
C        4   F   Z0(2) = Z point for FDCT track calculations, Half 1
C        5   I   Number of tracks in Half 0 of the FDC
C        6   I   Number of tracks in Half 1 of the FDC
C        7   I   spare 
C        8   I   spare 
C        9   I   spare 
C       10   I   spare 
C
C                     
C-   Created  xx-DEC-1988   Daria Zieminska 
C-   Updated  28-FEB-1990   Jeffrey Bantly  clean up 
C
C-----------------------------------------------------------------
      IMPLICIT NONE           
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'                             
      INTEGER ICONT(10),IWORD,LKFTRH
      INTEGER GZFTRH
C-----------------------------------------------------------------
      LKFTRH=GZFTRH(0)
      IF(LKFTRH.LE. 5) THEN
        CALL VFILL(ICONT,10,-1)
      ELSE
        DO 100 IWORD=1,10
          ICONT(IWORD)=IQ(LKFTRH+IWORD)
  100   CONTINUE
      ENDIF
C-----------------------------------------------------------------
  999 RETURN
      END
