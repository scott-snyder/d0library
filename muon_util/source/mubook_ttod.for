      SUBROUTINE MUBOOK_TTOD
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : book histograms for time-->distance plots
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created   6-FEB-1992   David Hedin
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*10 CMODU
      CHARACTER*20 DTTIT
      INTEGER MODNUM, MUNMOD3, NMODU, ID1, ID, I,  DUM
C
      CALL HBOOK1(30001,' S*DEV 1 ',50,-.5,.5,0.)
      CALL HBOOK1(30002,' S*DEV 2 ',50,-.5,.5,0.)
      CALL HBOOK1(30003,' S*DEV 3 ',50,-.5,.5,0.)
      CALL HBOOK1(30004,' S*DEV 4 ',50,-.5,.5,0.)
      CALL HBOOK1(30005,' S*DEV 5 ',50,-.5,.5,0.)
      CALL HBOOK1(30006,' S*DEV 6 ',50,-.5,.5,0.)
      CALL HBOOK1(30007,' S*DEV 7 ',50,-.5,.5,0.)
      CALL HBOOK1(30008,' S*DEV 8 ',50,-.5,.5,0.)
      CALL HBOOK1(30009,' S*DEV 9 ',50,-.5,.5,0.)
      CALL HBOOK1(30010,' S*DEV 10 ',50,-.5,.5,0.)
      CALL HBOOK1(30011,' S*DEV 11 ',50,-.5,.5,0.)
      CALL HBOOK1(30012,' S*DEV 12 ',50,-.5,.5,0.)
      CALL HBOOK1(30013,' S*DEV 13 ',50,-.5,.5,0.)
      CALL HBOOK1(30014,' S*DEV 14 ',50,-.5,.5,0.)
      CALL HBOOK1(30015,' S*DEV 15 ',50,-.5,.5,0.)
      CALL HBOOK1(30016,' S*DEV 16 ',50,-.5,.5,0.)
      CALL HBOOK1(30017,' S*DEV 17 ',50,-.5,.5,0.)
      CALL HBOOK1(30018,' S*DEV 18 ',50,-.5,.5,0.)
      CALL HBOOK1(30019,' S*DEV 19 ',50,-.5,.5,0.)
      CALL HBOOK1(30020,' S*DEV 20 ',50,-.5,.5,0.)
      NMODU = MUNMOD3(0,DUM)                  ! number of modules
      DO  I = 1,NMODU                        ! loop over modules
        MODNUM = MUNMOD3(1,I)
        ID = 30000 + 10*MODNUM
        WRITE(CMODU,101) MODNUM
        DTTIT(1:20) = ' S*DEV<2 '//CMODU
        CALL HBOOK1(ID,DTTIT,50,-.5,.5,0.)  !fine adjustment to T0
        DTTIT(1:20) = ' S*DEV>2 '//CMODU
        CALL HBOOK1(ID+1,DTTIT,50,-.5,.5,0.)  !fine adjustment to T0
      ENDDO
C
  101 FORMAT(I5)
  999 RETURN
      END
