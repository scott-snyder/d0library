      SUBROUTINE DBKSWR
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-      Book histograms for sense wires: <residuals> vs drift distance  
C-                                      <resolution> vs drift distance
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: called by DTRDIA
C-             ENTRY point: DTSSWR(HSTSWR)
C-
C-   Created  18-AUG-1988   Qizhong Li-Demarteau
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      LOGICAL YES, HSTSWR
      INTEGER I, IDT, IDS
      CHARACTER*41 FTNAME, SGNAME
      CHARACTER*50 TTITLE, STITLE
      CHARACTER*2 HNWIRN(28)
      DATA FTNAME/'residuals (track) VS drift dist. (wire '/
      DATA SGNAME/'residuals (segment) VS drift dist. (wire '/

      DATA HNWIRN /'00','01','02','03','04','05','06','07','08','09',
     +           '10','11','12','13','14','15','16','17','18','19',
     +           '20','21','22','23','24','25','26','27'/
C
C----------------------------------------------------------------------
C
      YES=.FALSE.
      CALL GETPAR(1,' Book residuals and resolution histograms 
     &  for SW? Y/N>','L',YES)
      IF(.NOT.YES) GOTO 999
C
      DO 100 I = 0,27
        IDT = 1200 + I
        TTITLE = FTNAME//HNWIRN(I+1)//')$'
        CALL HBOOK2(IDT,TTITLE,50,-7.5,7.5,80,-0.2,0.2,0.)
        IDS = 1230 + I
        STITLE = SGNAME//HNWIRN(I+1)//')$'
        CALL HBOOK2(IDS,STITLE,50,-7.5,7.5,40,-0.1,0.1,0.)
  100 CONTINUE  
C
 999  RETURN
C---------------------------------------------------------------------------
      ENTRY DTSSWR(HSTSWR)
      HSTSWR = YES
      RETURN
      END
