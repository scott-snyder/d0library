      SUBROUTINE JBUILD(MAT,IT,P1,P2,P3,P4,P5,P6,P7)
      REAL MAT(4,4),MAT2(4,4)
      COMMON/DI3PR/JUNIT
      IF (IT.NE.10) THEN
C  CONVERT THE LATEST TRANSFORMATION TO A MATRIX
        CALL JTRANS(MAT2,IT,P1,P2,P3,P4,P5,P6,P7)
      ELSE
        r = sqrt((p4-p1)**2 + (p5-p2)**2 + (p6-p3)**2)
        IF (R.EQ.0.0) THEN
          WRITE(6,*) 'ARB. TRANS. ERROR',P1,P2,P3,P4,P5,P6,P7
          RETURN
        END IF
        IF ((P4-P1).NE.0.0) THEN
          th = atan((p5-p2)/(p4-p1))
        ELSE
          TH = 3.1415926/2
        END IF
        th2 = asin((p6-p3)/r)
        WRITE(6,*) IT,P1,P2,P3,P4,P5,P6,P7
        dum = 0.
        CALL JTRANS(mat2,2,-p1,-p2,-p3,dum,dum,dum,dum)
        CALL J_MATMUL(MAT,MAT2)
        CALL JTRANS(mat2,6,-th,dum,dum,dum,dum,dum,dum)
        CALL J_MATMUL(MAT,MAT2)
        CALL JTRANS(mat2,5,th2,dum,dum,dum,dum,dum,dum)
        CALL J_MATMUL(MAT,MAT2)
        CALL JTRANS(mat2,4,p7,dum,dum,dum,dum,dum,dum)
        CALL J_MATMUL(MAT,MAT2)
        CALL JTRANS(mat2,5,-th2,dum,dum,dum,dum,dum,dum)
        CALL J_MATMUL(MAT,MAT2)
        CALL JTRANS(mat2,6,th,dum,dum,dum,dum,dum,dum)
        CALL J_MATMUL(MAT,MAT2)
        CALL JTRANS(mat2,2,p1,p2,p3,dum,dum,dum,dum)
      END IF
C  COMBINE THE TRANSFORMATIONS
      CALL J_MATMUL(MAT,MAT2)
      END
