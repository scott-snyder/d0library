      SUBROUTINE GIPART(NPART,ID,PX,PY,PZ,P,M,PHI,THETA,ETA,X,Y,Z)
C------------------------------------------------------------------------
C-                                                                      -
C     RETURNS PARTICLE INFO FROM BANKS ISP1
C-                                                                      -
C-    OUTPUT: NPART = number of particles                               -
C-    ID = isajet ID                                                    -
C-    PX,PY,PZ,P   momentums of ith particle                            -
C-    M,PHI,THETA,ETA mass and angles of ith particle
C     X,Y,Z    position of the ith particle
C             DH 5-87                                                   -
C-                                                                      -
C------------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$LINKS:IZISAE.LINK'
      INCLUDE 'D0$LINKS:IZISP1.LINK'
      INCLUDE 'D0$LINKS:IZISV1.LINK'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER SI
      PARAMETER (SI=1000)  
      INTEGER NPART,ID(SI)
      REAL PX(SI),PY(SI),PZ(SI),P(SI),M(SI),PHI(SI),THETA(SI),
     A ETA(SI),X(SI),Y(SI),Z(SI),XV,YV,ZV
     
C
      INTEGER LISAE,LISP1,LISV1
C   
C
      LISAE=LQ(LHEAD-IZISAE)
      NPART=0
C        Loop over vertices and particles
C
      LISV1=LISAE-IZISV1
C
  400 LISV1=LQ(LISV1)        ! loop over vertices
      IF(LISV1.GT.0) THEN
        XV=Q(LISV1+7)
        YV=Q(LISV1+8)
        ZV=Q(LISV1+9)
        LISP1=LISV1-IZISP1
C
  500   LISP1=LQ(LISP1)      ! loop over particles
        IF(LISP1.GT.0) THEN        
          IF(NPART.LT.SI) THEN
            NPART=NPART+1
            ID(NPART)=  IQ(LISP1+1)     ! particle id
            PX(NPART)=   Q(LISP1+2)
            PY(NPART)=   Q(LISP1+3)
            PZ(NPART)=   Q(LISP1+4)
            P(NPART)=    Q(LISP1+5)
            M(NPART)=    Q(LISP1+6)
            PHI(NPART)=  Q(LISP1+7)
            THETA(NPART)=Q(LISP1+8)
            ETA(NPART)=  Q(LISP1+9)
            X(NPART)=XV
            Y(NPART)=YV
            Z(NPART)=ZV
          ENDIF
C
        GOTO 500
        ENDIF
C
      GOTO 400
      ENDIF
      RETURN    
      END   
