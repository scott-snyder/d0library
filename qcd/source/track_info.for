      SUBROUTINE TRACK_INFO(ZVER,X,Y,Z,THETA,PHI,IMPACT,ZDIST,NCAL,
     &                      CALHITS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Given track trajectory, determine distance 
C-                         to vertex and possible hit calorimeter cells
C-
C-   Inputs  :      Track:   X,Y,Z,Theta,Phi
C-                  Primary Z vertex (ignore x,y of vertex)   
C-                  
C-   Outputs :             IMPACT = closest approach to beam (in x,y)
C-                         ZDIST = closest approach to vertex along z
C-                         NCAL = number of cal cells in CALHITS
C-                         CALHITS = array of calorimeter cells that
C-                                   track traverses
C-                                   (i,1) = ith cell IETA
C-                                   (i,2) = ith cell IPHI
C-                                   (i,3) = ith cell ILYR
C-   Controls: 
C-
C-   Created   7-FEB-1994   Brent J. May
C-   Updated  17-MAR-1996   Andrew G. Brandt  protect against ZDIST crash
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL ZVER,X,Y,Z,THETA,PHI,IMPACT,ZDIST
      REAL DIR(3), VTX(3), DIST, DET2
      INTEGER NCAL, CALHITS(20,3), IER 
C----------------------------------------------------------------------
      DIR(1) = SIN(THETA)*COS(PHI)
      DIR(2) = SIN(THETA)*SIN(PHI)
      DIR(3) = COS(THETA)
      VTX(1) = X
      VTX(2) = Y
      VTX(3) = Z
C See CRC handbook for formulas
      DIST = DET2(-VTX(2),ZVER-VTX(3),DIR(2),DIR(3))
      DIST = DIST + DET2(ZVER-VTX(3),-VTX(1),DIR(3),DIR(1))
      DIST = DIST + DET2(-VTX(1),-VTX(2),DIR(1),DIR(2))
      DIST = SQRT(DIST/(DIR(1)**2+DIR(2)**2+DIR(3)**2))
      IMPACT = ABS(VTX(2)*DIR(1)-VTX(1)*DIR(2))
C protect from bomb
      IF((DIR(2)**2+DIR(1)**2).EQ.0.) THEN
        IMPACT=1000.
        ZDIST=1000.
        GO TO 999
      END IF
      IMPACT = IMPACT/SQRT(DIR(2)**2+DIR(1)**2)
      IF((DIST**2-IMPACT**2).LE.0.) THEN
        ZDIST = 0.
      ELSE
        ZDIST = SQRT(DIST**2-IMPACT**2+.0001)
      END IF
      CALL CLINPH(VTX,DIR,20,NCAL,CALHITS(1,1),CALHITS(1,2),
     &            CALHITS(1,3),IER)
  999 RETURN
      END
      REAL FUNCTION DET2(A,B,C,D)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return the 2x2 determinant  
C-
C-   Returned value  : 
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created   7-FEB-1994   Brent J. May
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL A,B,C,D
C----------------------------------------------------------------------
      DET2 = (A*D-B*C)**2
  999 RETURN
      END
