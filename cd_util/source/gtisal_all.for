      SUBROUTINE GTISAL_ALL(NPART,ID,PX,PY,PZ,P,M,PHI,THETA,ETA)
C------------------------------------------------------------------------
C-                                                                      -
C     RETURNS PARTICLE INFO FROM BANKS ISAL FOR LEPTONS
C-                                                                      -
C-    OUTPUT: NPART = number of leptons                 -
C-    ID = isajet ID                                                    -
C-    PX,PY,PZ,P   momentums of ith particle                            -
C-    M,PHI,THETA,ETA mass and angles of ith particle
C             DH 5-87                                                   -
C-                                                                      -
C------------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$LINKS:IZISAE.LINK'
      INCLUDE 'D0$LINKS:IZISAL.LINK'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER SI
      PARAMETER (SI=100)  
      INTEGER NPART,ID(SI)
      REAL PX(SI),PY(SI),PZ(SI),P(SI),M(SI),PHI(SI),THETA(SI),ETA(SI)  
C
      INTEGER LISAE,LISAL,NUM
C   
C
      LISAE=LQ(LHEAD-IZISAE)
      NPART=0
C        Loop over LEPTONS
C
      LISAL=LISAE-IZISAL
C
  400 LISAL=LQ(LISAL)
      IF(LISAL.GT.0) THEN
        IF(NPART.LT.SI) THEN
            NPART=NPART+1  
            ID(NPART)=  IQ(LISAL+1)     ! particle id
            PX(NPART)=   Q(LISAL+2)
            PY(NPART)=   Q(LISAL+3)
            PZ(NPART)=   Q(LISAL+4)
            P(NPART)=    Q(LISAL+5)
            M(NPART)=    Q(LISAL+6)
            PHI(NPART)=  Q(LISAL+7)
            THETA(NPART)=Q(LISAL+8)
            ETA(NPART)=  Q(LISAL+9)
        ENDIF
C
      GOTO 400
      ENDIF
      RETURN    
      END   
