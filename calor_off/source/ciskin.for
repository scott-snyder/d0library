      SUBROUTINE CISKIN(NPART,VERT,IDPART,PART)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Convert Isajet banks to PVERT,PKINE
C-
C-   Inputs  :
C-   Outputs : NPART = Number of particles in Isajet bank
C-             VERT  = x,y,z of primary vertex.
C-             IDPART = Particle ID code (Isajet convention)
C-             PART(1-5) = px,py,pz,P and mass.
C-   Controls:
C-
C-   Created  19-MAY-1989   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER LISP1,LISV1,GZISV1
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZISP1.LINK'
      INTEGER IDPART(*),NPART
      REAL    PART(5,*),VERT(*)
C----------------------------------------------------------------------
      LISV1 = GZISV1()
      IF(LISV1.EQ.0) GOTO 999
      LISP1 = LQ(LISV1-IZISP1)
      VERT(1) = Q(LISV1+7)
      VERT(2) = Q(LISV1+8)
      VERT(3) = Q(LISV1+9)
      NPART = 0
  997 IF(LISP1.EQ.0)GO TO 998
      NPART = NPART +1
      IDPART(NPART) = IQ(LISP1+1)
      CALL UCOPY(Q(LISP1+2),PART(1,NPART),5)
      LISP1 = LQ(LISP1)
      GO TO 997
  998 CONTINUE
  999 RETURN
      END
