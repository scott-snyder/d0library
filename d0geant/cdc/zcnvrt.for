      SUBROUTINE ZCNVRT(RWDATA, LENGTH)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : bilinear conversion simulation
C-
C-   Inputs  : RWDATA: array of simulated data
C-             LENGTH: data length
C-   Outputs : RWDATA: data after biliner conversion
C-
C-   Created  30-MAY-1989   Qizhong Li-Demarteau
C-   Updated   4-MAY-1992   Alexandre Zinchenko  To handle possible
C-                          negative amplitudes (undershoots)
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL    RWDATA(0:*)
      INTEGER IFADC, LENGTH, I, MXDATA
      REAL    HEIGHT(0:885), BILIRT, BILIPT
      LOGICAL FIRST
      DATA    FIRST/.TRUE./
      DATA    BILIRT/11.0/, BILIPT/192.0/
C
C  maxmum conversion: (255 - 192) * 11 + 192 = 885
C
      DATA    MXDATA/885/
C----------------------------------------------------------------------
C
C  build a mapping array for bilinear conversion
C
      IF (FIRST) THEN
        FIRST = .FALSE.
        DO 100 I = 0, BILIPT
          HEIGHT(I) = FLOAT(I)
  100   CONTINUE
        DO 200 I = BILIPT+1, MXDATA     
          HEIGHT(I) = (I - BILIPT) / BILIRT + BILIPT
  200   CONTINUE
      ENDIF
C
      DO 300 I = 0, LENGTH-1
        IFADC = NINT(RWDATA(I))
        IF (IFADC .GT. MXDATA) IFADC = MXDATA
        RWDATA(I) = HEIGHT(IABS(IFADC)) 
        IF(IFADC.LT.0) RWDATA(I)=-RWDATA(I) 
  300 CONTINUE      
C----------------------------------------------------------------------
  999 RETURN
      END
