      SUBROUTINE PLOT_GCAH
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :PLOTS GCAH TRACK AND CELLS
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
      REAL    LTRK,PTMIN,P4(4),PP,BEGIN_VTX(3),END_VTX(3)
      REAL LTR
      INTEGER IPART,I
C
      INTEGER ITRA,IDATA(100),NPOINT
      REAL    VERT(3,3),P(4,2),TOTAL
C
      INTEGER GZISAE,GZISV1
      LOGICAL FIRST
      DATA FIRST/.TRUE./
      DATA PTMIN/0.0/                   ! MINIMUM PT.
      REAL    EMIN,ENER,COLRNGE
      INTEGER IC,NUM
      DATA EMIN/0.0/
      CHARACTER*30 STR1,STR2
      CHARACTER*3  CC
      INTEGER KCOL, KINT, KFIL, KSTY
C
      REAL CELLER(2)
      INTEGER IE,IP,IL,ITAG
      REAL    XS(30),YS(30),ZS(30)
      INTEGER NS,IERR
C
      LOGICAL CEXIST
C----------------------------------------------------------------------
      WRITE(STR1,100) EMIN
  100 FORMAT('CAL EMIN= ',F6.2)
      CALL PUMESS(STR1)
C
      CALL GHGCAH(LGCAH,ITRA,VERT,P,IDATA,TOTAL,NPOINT)
C
C ****  PLOT GCAH TRACK.
C
      LTR = 0.
      DO I = 1 , 3
        BEGIN_VTX(I) = VERT(I,1)
        END_VTX(I)   = VERT(I,3)        ! 1ST INTERACTION POINT IN CAL.
        LTR = LTR + (BEGIN_VTX(I)-END_VTX(I))**2
        P4(I) = P(I,2)                  ! MOMENTUM AT ENTRY INTO CAL
      ENDDO
      LTR = SQRT(LTR)
      PP = SQRT(P4(1)**2+P4(2)**2+P4(3)**2)
C
C      CALL PUOPEN
      CALL PLISTK(PTMIN,LTR,P4,PP,BEGIN_VTX,END_VTX)
C
C---  Display Calorimeter cells
C
      DO 200 I=1,NPOINT
        CALL GDGCAH(LGCAH,I,CELLER,IE,IP,IL,ITAG)
        ENER = CELLER(1)                ! total energy
        IF(CEXIST(IE,IP,IL).AND.ENER.GE.EMIN)THEN
C
C Determining color for the cells
C
          CALL PCECOL(ENER,EMIN,COLRNGE,IC,CC,NUM)
          CALL PXCOLN('CDC',IC,3,1,.FALSE.,KCOL,KINT,KFIL,KSTY)
          CALL JCOLOR(KCOL)
          CALL JLWIDE(KSTY)
          CALL CELVEC(IE,IP,IL, XS,YS,ZS,NS,IERR) ! return cells coord.
          IF(IERR.NE.0) GO TO 200
          IF(NS.LE.2.OR.NS.GT.20)GO TO 200
          CALL PLTVEC(XS,YS,ZS,NS) ! draw cells
        ENDIF
  200 CONTINUE
C      CALL JRCLOS
  999 RETURN
      END
