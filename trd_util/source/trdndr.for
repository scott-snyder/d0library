      LOGICAL FUNCTION TRDNDR
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :  TRD post analysis. For the time being print the
C-   TRD histograms relevant for the rejection
C-
C-   Returned value  :
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  11-JUL-1989   A. Zylberstejn
C-   Updated  29-JUN-1992   A. Zylberstejn   
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER NHIST,NX,NY,NWT,LOC,TRUNIT
      PARAMETER( NHIST = 2 )
      INTEGER NUMHIS(NHIST)
      INTEGER I,J,LOUT
      REAL CONTH(100),STEP,XMI,XMA,YMI,YMA
      CHARACTER*80 TIT
      LOGICAL HEXIST
      DATA NUMHIS/7010,7110/
C----------------------------------------------------------------------
      LOUT=TRUNIT()
      TRDNDR=.TRUE.
      DO 10 I=1,NHIST
        IF(.NOT. HEXIST(NUMHIS(I)))THEN
          WRITE(LOUT,*)' HISTOGRAM NB.',NUMHIS(I),' DOES NOT EXIST'
          GO TO 10
        END IF
C  Get title and binning
        CALL HGIVE(NUMHIS(I),TIT,NX,XMI,XMA,NY,YMI,YMA,NWT,LOC)
        CALL HUNPAK(NUMHIS(I),CONTH,'HIST',1)
        WRITE(LOUT,*)' HISTO ',NUMHIS(I)
        WRITE(LOUT,*)'NX,XMI,XMA',NX,XMI,XMA
        WRITE(LOUT,'(A80)')TIT(1:nwt)
        WRITE(LOUT,'(6F10.1)')(CONTH(J),J=1,NX)
        STEP=(XMA-XMI)/FLOAT(NX)
C        print*,' STEP',STEP
   10 CONTINUE
  999 RETURN
      END
