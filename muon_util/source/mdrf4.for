      SUBROUTINE MDRF4(Z0,Z1,Z2,Z3,DR,Y0,Y1,Y2,Y3,
     &  T0,T1,T2,T3,ANGLE,GAIN,CHIMIN,DEL012,DELTA_T0)
C=======================================================================
C=
C=   Purpose and Methods : Calculates T0 correction for 4 hit A layer tracks
C=
C=   Inputs  : Z, DR, Y are wire position in Z, drift distance, w.p. in Y
C=             T is drift time in ns, ANGLE is angle w.r.t wire plane,
C=             GAIN is ns/TDC count
C=   Outputs : CHIMIN = Minimum Chi**2 for fitted track.
C=             DEL012 = 3-miss residual.
C=             DELTA_T0 = Calculated T0 correction, in ns.
C=                        If DELTA_T0 = 999999., then no information
C=                        is available.
C=
C=   Controls:
C=
C=   Created: 6/93 R. Markeloff.
C=
C======================================================================
      IMPLICIT NONE
C
      INTEGER I0, I1, I2, I3, J0, J1, J2, J3
C
      REAL Z0, Z1, Z2, Z3, DR(0:3), Y0, Y1, Y2, Y3, CHI
      REAL T0, T1, T2, T3, ANGLE(0:3), DELTA_T0, CHIMIN, SLOPE, S(2)
      REAL DERIV(0:3), SY, SX, SY2, SX2, SXY, ALPHA, B, C, D, E
      REAL A(5,4), U(5,4), W(4), V(4,4), WMAX, WMIN, NCOND, TOL
      REAL RHS(5), X(4), SUM, GAIN(0:3), DEL012, ZP, ERROR, DEL
      REAL MDRF_DERIV
C
      DATA S / -1., 1. /
      DATA TOL / 1.0E-05 /
C
C======================================================================
C
      DELTA_T0 = 999999.
      DEL012 = -999999.
      SY = Y0 + Y1 + Y2 + Y3
      SY2 = Y0**2 + Y1**2 + Y2**2 + Y3**2
C
C---- Loop over combinations of solutions
C
      CHIMIN = 999999.
      DO I0 = 1,2
        DO I1 = 1,2
          DO I2 = 1,2
            DO I3 = 1,2
C
C------------ Look for solution with minimum Chi**2
C
              SX = Z0+S(I0)*DR(0) + Z1+S(I1)*DR(1) +
     &                  Z2+S(I2)*DR(2) + Z3+S(I3)*DR(3)
              SX2 = (Z0+S(I0)*DR(0))**2+(Z1+S(I1)*DR(1))**2 +
     &                  (Z2+S(I2)*DR(2))**2 + (Z3+S(I3)*DR(3))**2
              SXY = (Z0+S(I0)*DR(0))*Y0+(Z1+S(I1)*DR(1))*Y1 +
     &                  (Z2+S(I2)*DR(2))*Y2 + (Z3+S(I3)*DR(3))*Y3
              CALL MUSLIN2(4.,SX,SY,SX2,SY2,SXY,ALPHA,B,C,D,E,CHI)
              IF (ABS(CHI) .LT. CHIMIN) THEN
                CHIMIN = ABS(CHI)
                J0 = I0
                J1 = I1
                J2 = I2
                J3 = I3
              ENDIF
            ENDDO
          ENDDO
        ENDDO
      ENDDO
C
C---- Tracks with hits all on the same side of the wire are rejected
C
      IF (S(J0) .EQ. S(J1) .AND. S(J0) .EQ. S(J2) .AND.
     &  S(J0) .EQ. S(J3)) RETURN
C
C---- Calculate 3-miss residual
C
      IF (S(J0) .NE. S(J1) .OR. S(J0) .NE. S(J2)) THEN
        SLOPE=(Z2+S(J2)*DR(2)-Z0-S(J0)*DR(0))/(Y2-Y0)
        ZP = Z0 + S(J0)*DR(0) + SLOPE*(Y1-Y0)
        DEL = Z1 + S(J1)*DR(1) - ZP
      ENDIF
C
C---- Calculate derivatives of distance along midplane w.r.t. drift time
C
      DO I0 = 0,3
        DERIV(I0) = MDRF_DERIV(T0,ANGLE(I0))
      ENDDO
C
C---- Set up matrix for SVD solution to 5 equations with 4 unknowns
C
      A(1,1) = S(J0)*(Y2 - Y1)
      A(1,2) = S(J1)*(Y0 - Y2)
      A(1,3) = S(J2)*(Y1 - Y0)
      A(1,4) = 0.
C
      A(2,1) = 0.
      A(2,2) = S(J1)*(Y3 - Y2)
      A(2,3) = S(J2)*(Y1 - Y3)
      A(2,4) = S(J3)*(Y2 - Y1)
C
      A(3,1) = GAIN(1)*DERIV(1)
      A(3,2) = -GAIN(0)*DERIV(0)
      A(3,3) = 0.
      A(3,4) = 0.
C
      A(4,1) = GAIN(2)*DERIV(2)
      A(4,2) = 0.
      A(4,3) = -GAIN(0)*DERIV(0)
      A(4,4) = 0.
C
      A(5,1) = GAIN(3)*DERIV(3)
      A(5,2) = 0.
      A(5,3) = 0.
      A(5,4) = -GAIN(0)*DERIV(0)
C
      DO I0 = 1,5
        DO I1 = 1,4
          U(I0,I1) = A(I0,I1)
        ENDDO
      ENDDO
C
C---- Now perform the singular value decompostion of the matrix U
C
      CALL MDRF_SVDCMP(U,5,4,W,V)
C
C---- Check condition number of matrix
C
      WMAX = 0.
      WMIN = 9999999.
      DO I0 = 1,4
        WMAX = MAX(WMAX,ABS(W(I0)))
        WMIN = MIN(WMIN,ABS(W(I0)))
      ENDDO
      NCOND = WMIN/WMAX
C
C----- Reject matrices that are close to singular
C
      IF (NCOND .LT. TOL .OR. WMIN .LT. TOL) RETURN
C
C---- Set up vector on right-hand side of equation
C
      RHS(1) = (Z0+S(J0)*DR(0))*(Y2-Y1) + (Z1+S(J1)*DR(1))*(Y0-Y2) +
     &  (Z2+S(J2)*DR(2))*(Y1-Y0)
      RHS(2) = (Z1+S(J1)*DR(1))*(Y3-Y2) + (Z2+S(J2)*DR(2))*(Y1-Y3) +
     &  (Z3+S(J3)*DR(3))*(Y2-Y1)
      RHS(3) = 0.
      RHS(4) = 0.
      RHS(5) = 0.
C
C----- The vector X returned from SVBKSB is the best solution to the
C----- overdetermined system, in the least squares sense.
C
      CALL MDRF_SVBKSB(U,W,V,5,4,RHS,X)
C
C---- Verify that the calculated offsets are physical
C
      DO I0 = 1,4
        IF (ABS(X(I0)) .GT. DR(I0)) RETURN
      ENDDO
C
      DEL012 = DEL
C
C---- Check that the solution found is a good one
C
      ERROR = 0.
      DO I0 = 1,5
        SUM = 0.
        DO I1 = 1,4
          SUM = SUM + A(I0,I1)*X(I1)
        ENDDO
        ERROR = ERROR + (SUM - RHS(I0))**2
      ENDDO
      IF (ERROR .GT. 1.0E-08) RETURN
C
C---- Calculate average T0 correction
C
      SUM = 0.
      DO I0 = 1,4
        SUM = SUM + X(I0)/(GAIN(I0-1)*DERIV(I0-1))
      ENDDO
      DELTA_T0 = - SUM/4.
C
      RETURN
      END
