      SUBROUTINE XYHLIM(IDH,IMIN,IMAX)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : find peak region of a histogram for 
C-                         Gaussian fit
C-
C-   Inputs  : IDH - histogram ID
C-   Outputs : IMIN - lower limit
C-             IMAX - upper limit
C-   Controls: 
C-
C-   Created  29-MAR-1993   Alexandre Zinchenko
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL FIRST
      INTEGER IDH, IER, NBXY, IMIN, IMAX, N0, I, LVMAX 
      REAL CHI2, DIM(500), A1, VMAX, DELTA
      DATA FIRST /.TRUE./, DELTA /0.4/
C
      IF (FIRST) THEN
        FIRST = .FALSE.
        CALL EZPICK('XYVERT_RCP')
        CALL EZGET('BIN_NUMB_XY',NBXY,IER)
        IF (IER.NE.0) CALL ERRMSG(' Error reading RCP-file',
     &     'XYHLIM',' Exit','F')
        CALL EZRSET
      ENDIF        
      CALL HCOPY(IDH,IDH+100,' ')
      CALL HSMOOF(IDH+100,0,CHI2)
      CALL HUNPAK(IDH+100,DIM,' ',0)
C
      A1 = VMAX(DIM,NBXY)
      N0 = LVMAX(DIM,NBXY)
      IMIN = 1
      IMAX = NBXY
      DO 10 I = N0, MAX0(N0-50,1), -1
        IF (DIM(I).LT.A1*DELTA) THEN
          IMIN = I
          GO TO 11
        ENDIF
   10 CONTINUE
   11 DO 20 I = N0, MIN0(N0+50,NBXY), 1
        IF (DIM(I).LT.A1*DELTA) THEN
          IMAX = I
          GO TO 21
        ENDIF
   20 CONTINUE
   21 CALL HDELET(IDH+100)
C----------------------------------------------------------------------
  999 RETURN
      END
