      SUBROUTINE PCPJET(XV,IERR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Draw Parton Jets Axes
C-
C-   Inputs  : 
C-   Outputs : XV - primary vertex coordinate
C-   Controls: 
C-
C-   Created   9-FEB-1990   Nobuaki Oshima
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZISV1.LINK'
      REAL    PXYZ(3),PT,P, XV(3),XC(3)
      INTEGER GZISAE, LISAE, LISV1
      INTEGER GZPJET, LPJET
      INTEGER IERR
      CHARACTER*8  PTSTR
      CHARACTER*26 MESS1,MESS2
C.
      DATA MESS1/' ISAE bank does not exist'/
      DATA MESS2/' PJET bank does not exist'/
C----------------------------------------------------------------------
      IERR = 0
C-
C---  Get ISV1 coordinate
C-
      LISAE= GZISAE ()
      IF (LISAE .EQ. 0)         GO TO 900
      CALL VZERO (XV,3)
      LISV1  = LQ(LISAE-IZISV1)
      XV(1) = Q(LISV1+7)
      XV(2) = Q(LISV1+8)
      XV(3) = Q(LISV1+9)
C-
C--- Get PJET banks, then loop it...
      LPJET = GZPJET()
      IF (LPJET .LE. 0)         GO TO 888
  100 CONTINUE
      PT      = Q(LPJET+2)
      PXYZ(1) = Q(LPJET+3)
      PXYZ(2) = Q(LPJET+4)
      PXYZ(3) = Q(LPJET+5)
      P = SQRT(PXYZ(1)**2+PXYZ(2)**2+PXYZ(3)**2)
      CALL VZERO (XC,3)
      CALL PLISTK(-1., 400., PXYZ, P, XV, XC)
      CALL PXFTOC(PT, PTSTR)
C
      CALL JSIZE(10., 18.)
      CALL J3STRG(PTSTR)
C-
      LPJET = LQ(LPJET)
      IF (LPJET .GT. 0)   GO TO 100
      GO TO 999
  888 CALL JRCLOS
      CALL PUMESS(MESS2)
      IERR = 2
      GO TO 999
  900 CALL JRCLOS
      CALL PUMESS(MESS1)
      IERR = 1
  999 RETURN
      END
