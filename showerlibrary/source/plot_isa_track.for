      SUBROUTINE PLOT_ISA_TRACK
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : PLOTS ISAJET TRACK GIVEN BY LISP1
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  17-MAY-1990   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKC.INC'
      INCLUDE 'D0$LINKS:IZISAE.LINK/LIST'
      INCLUDE 'D0$LINKS:IZISV1.LINK/LIST'
      INCLUDE 'D0$LINKS:IZISP1.LINK/LIST'
      INTEGER LISAE,LISV1,LISP1,LJET
      EQUIVALENCE (CSTLNK(31),LISAE),(CSTLNK(32),LISV1)
      EQUIVALENCE (CSTLNK(33),LISP1),(CSTLNK(34),LJET)
C
      INTEGER GZISAE,GZISV1
C
      REAL    LTRK,PTMIN,P4(4),PP,BEGIN_VTX(3),END_VTX(3)
      REAL LTR
      INTEGER IPART,I
C
      DATA PTMIN/0.0/                   ! MINIMUM PT TO PLOT
      DATA LTRK/300.0/                  ! MAXIMUM LENGTH OF TRACK
C----------------------------------------------------------------------
      CALL UCOPY(Q(LISV1+7),BEGIN_VTX,3)
      LGCAH = LQ(LISP1-5)               ! GCAH LINK.
      IF(LGCAH.NE.0)THEN                ! 1ST GCAH TRACK
        CALL UCOPY(Q(LGCAH+26),END_VTX,3)  ! END VERTEX
      ELSE
        CALL UZERO(END_VTX,1,3)
        LTR = LTRK
      ENDIF
      CALL UCOPY(Q(LISP1+2),P4,4)
      PP = SQRT(P4(1)**2+P4(2)**2+P4(3)**2)
      LTR = 0
      DO I = 1 , 3
        LTR = LTR + (END_VTX(I)-BEGIN_VTX(I))**2
      ENDDO
      LTR = SQRT(LTR)
      IF(LTR.LT.0.1)THEN
        LTR = LTRK
        CALL UZERO(END_VTX,1,3)
      ENDIF
C
      IPART = IQ(LISP1+1)         ! PARTICLE ID
      CALL PUOPEN
      CALL PLISTK(PTMIN,LTR,P4,PP,BEGIN_VTX,END_VTX)
      CALL JRCLOS
  999 RETURN
      END
