      SUBROUTINE TOP_LEPTONS_UTIL_G_SPHERICITY(NOBJ,P4VECS,CMFRAME,
     &  GSpher,GAplan,GY)
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
C-   Outputs : GSPHER         [R]     = sphericity
C-             GAPLAN         [R]     = aplanarity
C-             Y              [R]     = the co-ordinate normal to S
C-   Removed from argument list but still available in routine:
C-             P3_EIGVAL(3)   [R]     = the normalized eigenvalues
C-             P3_EIGVEC(3,3) [R]     = the unit eigenvectors as columns
C-
C-   Controls: None
C-
C-   Created  13-AUG-1993   Dhiman Chakraborty
C-   Modified 26-Aug-1993   changed name to add to top_leptons
C-                          added aplanarity as argument and removed 
C-                          some other arguments - jt
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER NOBJ,II,JJ,MDIM,IERR,CMFRAME
      PARAMETER( MDIM = 3 )
      INTEGER INDEXX(MDIM)
      REAL    P4VECS(4,100),P4V(4,100),P3C(MDIM,1),P3R(1,MDIM)
      REAL    P3_TENSOR(MDIM,MDIM),WORK(MDIM)
      REAL    P3_EIGVAL(MDIM),P3_EIGVEC(MDIM,MDIM),TEMP(MDIM,MDIM)
      REAL    SUM_PSQ,SCALE_PSQ
      REAL    GSPHER,GAPLAN,GY
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
      GSPHER = 0.
      GAPLAN = 0.
      GY = 0.
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
      BETAZ = (UZ/ABS(UZ))*SQRT(1. - GAMMAZ**-2)
      DO II = 1,NOBJ
        P4V(3,II) = GAMMAZ*(P4V(3,II) - BETAZ*P4V(4,II))
        P4V(4,II) = GAMMAZ*(P4V(4,II) - BETAZ*P4V(3,II))
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
      GSPHER = 1.5 * (P3_EIGVAL(INDEXX(2)) + P3_EIGVAL(INDEXX(1)))
      GAPLAN = 1.5 * (P3_EIGVAL(INDEXX(1)))
      GY = SQRT(0.75) * (P3_EIGVAL(INDEXX(2)) - P3_EIGVAL(INDEXX(1)))
  999 RETURN
      END
