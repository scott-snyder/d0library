      SUBROUTINE SPHERICITY0(NOBJ,P4VECS,CMFRAME,S,Y,A,P3_EIGVAL,
     &   P3_EIGVEC,PTOT,MOBJ)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : To calculate the total momentum tensor
C-                         from the given set of object 3-vectors and
C-                         return the sphericity and y of the set
C-                         (for definitions, see "Collider Physics"
C-                         by Barger & Philips, p. 280 - 282)
C-
C-   Inputs  : NOBJ           [I]
C-             P4VECS(4,NOBJ) [R]                 (NOBJ < 101)
C-             CMFRAME        [I]     if CMFRAME.EQ.1, does calculations in 
C-                          the z-boosted CM frame.  Else, in the lab frame
C-
C-   Outputs : S              [R]     (= sphericity),
C-             Y              [R]     is the co-ordinate normal to S
C-             A              [R]     (= aplanarity),
C-             P3_EIGVAL(3)   [R]     has the normalized eigenvalues
C-             P3_EIGVEC(3,3) [R]     has the unit eigenvectors as its columns
C-             PTOT(4)        [R]     has the total 4-momentum of the objects
C-             MOBJ           [R]     the invariant mass of the objects
C-   Controls: None
C-
C-   Created  13-AUG-1993   Dhiman Chakraborty
C-   Updated  24-NOV-1993   Dhiman Chakraborty
C-                          Added APLANARITY to the argument list
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER NOBJ,II,JJ,MDIM,IERR,CMFRAME
      PARAMETER( MDIM = 3 )
      INTEGER INDEXX(MDIM)
      REAL    P4VECS(4,100),P4V(4,100),P3C(MDIM,1),P3R(1,MDIM)
      REAL    P3_TENSOR(MDIM,MDIM),WORK(MDIM)
      REAL    P3_EIGVAL(MDIM),P3_EIGVEC(MDIM,MDIM),TEMP(MDIM,MDIM)
      REAL    SUM_PSQ,SCALE_PSQ,XTEMP
      REAL    S,Y,A
      REAL    PTOT(4),UZ,BETAZ,GAMMAZ,MOBJ
C----------------------------------------------------------------------
      CALL VZERO(P3R,MDIM)
      CALL VZERO(P3C,MDIM)
      CALL VZERO(TEMP,MDIM*MDIM)
      CALL VZERO(P3_TENSOR,MDIM*MDIM)
      CALL VZERO (P3_EIGVAL,MDIM)
      CALL VZERO (P3_EIGVEC,MDIM*MDIM)
      CALL VZERO(PTOT,4)
      MOBJ = 0.
      SUM_PSQ = 0.
      SCALE_PSQ = 0.
      S = 0.
      Y = 0.
      IF (NOBJ.LE.1) GOTO 999
      CALL UCOPY(P4VECS,P4V,4*NOBJ)
      IF(CMFRAME.NE.1) GOTO 111
      CALL VZERO (PTOT,4)
      DO II = 1,4
        DO JJ = 1,NOBJ
          PTOT(II) = PTOT(II) + P4V(II,JJ)
        ENDDO
      ENDDO
      MOBJ = PTOT(4)**2 - PTOT(1)**2 - PTOT(2)**2 - PTOT(3)**2
      IF(MOBJ.GT.0) THEN
        MOBJ = SQRT(MOBJ)
      ELSE
        MOBJ = 0.
      ENDIF
      IF(MOBJ.LT.0.001) GOTO 111
      UZ = PTOT(3)/MOBJ
      GAMMAZ = SQRT(1. + UZ**2)
      BETAZ = (UZ/ABS(UZ))*SQRT(1. - GAMMAZ**(-2))
      DO II = 1,NOBJ
        XTEMP = P4V(3,II) 
        P4V(3,II) = GAMMAZ*(P4V(3,II) - BETAZ*P4V(4,II))
        P4V(4,II) = GAMMAZ*(P4V(4,II) - BETAZ*XTEMP)
      ENDDO
  111 CONTINUE
      DO JJ = 1,NOBJ
        DO II = 1,MDIM
          P3R(1,II) = P4V(II,JJ)
          P3C(II,1) = P4V(II,JJ)
          SUM_PSQ = SUM_PSQ + P4V(II,JJ) ** 2
        ENDDO
        CALL MXDIPR(P3R,P3C,TEMP,1,MDIM,MDIM,1,MDIM,MDIM,1)
        CALL RMADD(MDIM,MDIM,TEMP(1,1),TEMP(1,2),TEMP(2,1),
     &        P3_TENSOR(1,1),P3_TENSOR(1,2),P3_TENSOR(2,1),
     &        P3_TENSOR(1,1),P3_TENSOR(1,2),P3_TENSOR(2,1))
      ENDDO
      CALL EISRS1(MDIM,MDIM,P3_TENSOR,P3_EIGVAL,P3_EIGVEC,IERR,WORK)
      SCALE_PSQ = 1./SUM_PSQ
      CALL VSCALE(P3_EIGVAL,SCALE_PSQ,P3_EIGVAL,MDIM)
      CALL SORTZV(P3_EIGVAL,INDEXX,MDIM,1.,0,0)
      S = 1.5 * (P3_EIGVAL(INDEXX(2)) + P3_EIGVAL(INDEXX(1)))
      Y = SQRT(0.75) * (P3_EIGVAL(INDEXX(2)) - P3_EIGVAL(INDEXX(1)))
      A = 1.5 * P3_EIGVAL(INDEXX(1))
  999 RETURN
      END
