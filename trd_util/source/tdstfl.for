      SUBROUTINE TDSTFL(ACCEPTANCE,EPST,EPSL,EPSL_2,LIK1,LIK2)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fill in the contents of the bank tdst.
C-
C-   Inputs  :  ACCEPTANCE
C-              EPST
C-              EPSL
C-              EPSL_2
C-              LIK1
C-              LIK2
C-
C-   Outputs :
C-
C-   Controls:
C-
C-   Created                A. Zylberstejn   
C-   Updated   8-JUL-1995   A. Zylberstejn: update version number
C-   Updated  29-AUG-1995   Lewis Taylor Goss  add EPSL_2 
C-   Updated  29-FEB-1996   A. Zylberstejn   Increment version number
C-   Updated  12-SEP-1996   A. Zylberstejn   Increment version nb. after YD
C-   quantitites have been put in the bank
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER LTDST
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:TRD_DST_ENERGIES.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC'
C----------------------------------------------------------------------
      INTEGER I,GZTDST
      LOGICAL ACCEPTANCE
      REAL EPST,EPSL,EPSL_2,LIK1,LIK2
C----------------------------------------------------------------------
      LOGICAL FIRST
      SAVE FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      IF ( FIRST ) THEN
C do initialization here if necessary.
        FIRST = .FALSE.
      ENDIF
C
C      IF ( LTDST .LT. 0 ) THEN
C        LTDST = GZTDST()    ! GET LINK.
C      ENDIF
C      print*,' enter tdstfl, with ltdst',ltdst
C
C Book the bank if argument = 0.
C      IF ( LTDST .EQ. 0 ) THEN
      CALL BKTDST(LTDST)
      IF(LTDST.LE.0)THEN
        CALL ERRMSG('Cant book Tdst','TDSTFL',' ','W')
        GO TO 999
      END IF
C      ENDIF
C
c      Q(LTDST+1) = 1.1               ! Bank version
c      Q(LTDST+1)  = 2.0               !after July 1995
c      Q(LTDST+1)  = 2.1               !after Feb. 96
      Q(LTDST+1)  = 2.2               !after Sep. 96     
      DO I=1,5
        Q(LTDST+1+I)=ENERGY(I)
        Q(LTDST+11+I)=ENERGY_FIRED_CELLS(I)
      END DO
      Q(LTDST+7)=0.
      IF (ACCEPTANCE) Q(LTDST+7)=1.
      Q(LTDST+8)=EPST
      Q(LTDST+9)=EPSL
      Q(LTDST+10)=LIK1
      Q(LTDST+11)=LIK2
      Q(LTDST+20)=EPSL_2
C
      CALL TANAFL(LTDST)! fill bank tana
C
  999 RETURN
      END
