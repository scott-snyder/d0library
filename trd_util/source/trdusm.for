      LOGICAL FUNCTION TRDUSM
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Output TRD information
C-
C-   Returned value  :
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  23-JUN-1989   A. Zylberstejn
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:firsht.INC'
      INTEGER ICH,ID,IK,JSH,IBID
      INTEGER LOUT,trUNIT
      data ibid/1/
C----------------------------------------------------------------------
      TRDUSM=.TRUE.
      CALL HCDIR('//PAWC/TRD',' ')  ! Go to TRD directory
      lout=trunit()
      CALL HOUTPU(LOUT)
C  Print of histograms of ENERGY PER LAYER
      IF(IBID.EQ.0)THEN
        CALL HPRINT(0)
        RETURN
      END IF
C  Print histograms and percentage of histograms of quantities summed
C   over the 3 TRD layers
      DO 40 IK=1,2
        JSH=FIRSHT+(IK-1)*100
        DO 40 ICH=25,29,2
          ID=ICH+JSH
          CALL hplis(lout,id,1)
c          call hdelet(id)
   40 CONTINUE
      DO 60 IK=1,2
        JSH=FIRSHT+(IK-1)*100
        DO 60 ICH=31,39,2
          ID=ICH+JSH
c         print*,' dans trdsum appel a hplis avec id= ',id
          CALL hplis(lout,id,1)
c          call hdelet(id)
   60 CONTINUE
c      print*,' dans trdusm appel a histdo'
      call histdo
  999 RETURN
      END
