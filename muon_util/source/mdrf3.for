      SUBROUTINE MDRF3(Z0,Z1,Z2,DR0,DR1,DR2,Y0,Y1,Y2,T0,T1,T2,
     &  ANGLE0,ANGLE1,ANGLE2,GAIN0,GAIN1,GAIN2,DEL012,DELTA_T0)
C=======================================================================
C=
C=   Purpose and Methods : Calculate T0 correction for a three-hit
C=                         track in the bend view.  Also returns the
C=                         3-miss residual.
C=
C=   Inputs  : Z, DR, Y are wire position in Z, drift distance, w.p. in Y
C=             T is drift time in ns, ANGLE is angle w.r.t wire plane,
C=             GAIN is ns/TDC count
C=   Outputs : DEL012 = minimum miss distance
C=             DELTA_T0 = Calculated T0 correction, in TDC counts.
C=                        If DELTA_T0 = 999999., then no information
C=                        is available.
C=
C=   Controls:
C=
C=   Created: 6/93 R. Markeloff.  Modified from D. Hedin's code.
C=
C======================================================================
      IMPLICIT NONE
C
      INTEGER I0, I1, I2
C
      REAL Z0, Z1, Z2, DR0, DR1, DR2, Y0, Y1, Y2, DEL
      REAL ZP, DELTA_T0, DELMIN, SLOPE, S(2), S0, S1, S2
      REAL T0, T1, T2, ANGLE0, ANGLE1, ANGLE2, DY02, DY01
      REAL DERIV0, DERIV1, DERIV2, R0, NUMERATOR, DENOMINATOR
      REAL GAIN0, GAIN1, GAIN2, DEL012
      REAL MDRF_DERIV
C
      DATA S / -1., 1. /
C======================================================================
C
      DELTA_T0 = 999999.
C
C       Loop over combinations of solutions
C       ===================================
      DELMIN = 999999.
      DEL012 = -999999.
      DO I0 = 1,2
        DO I1 = 1,2
          DO I2 = 1,2
C
C---------- Look for the combination for which the middle hit
C---------- is closest to the line defined by the other two hits
C
            SLOPE = (Z2+S(I2)*DR2-Z0-S(I0)*DR0)/(Y2-Y0)
            ZP = Z0 + S(I0)*DR0 + SLOPE*(Y1-Y0)
            DEL = Z1 + S(I1)*DR1 - ZP
            IF (ABS(DEL) .LT. ABS(DELMIN)) THEN
              DELMIN = DEL
              S0 = S(I0)
              S1 = S(I1)
              S2 = S(I2)
            ENDIF
          ENDDO
        ENDDO
      ENDDO
C
C---- Tracks with hits all on the same side of the wire are rejected
C
      IF (S0 .EQ. S1 .AND. S0 .EQ. S2) RETURN
      DEL012 = DELMIN
C
C---- Calculate derivatives of distance along midplane w.r.t. drift time
C
      DERIV0 = MDRF_DERIV(T0,ANGLE0)
      DERIV1 = MDRF_DERIV(T1,ANGLE1)
      DERIV2 = MDRF_DERIV(T2,ANGLE2)
C
      DY02 = Y0 - Y2
      DY01 = Y0 - Y1
C
C---- Solve for the displacements in the drift direction
C
      NUMERATOR = (DY01-DY02)*(Z0+S0*DR0) + DY02*(Z1+S1*DR1) -
     &  DY01*(Z2+S2*DR2)
      DENOMINATOR = S0*(DY01-DY02) +
     &  S1*DY02*(DERIV1*GAIN1)/(DERIV0*GAIN0) -
     &  S2*DY01*(DERIV2*GAIN2)/(DERIV0*GAIN0)
      IF (DENOMINATOR .NE. 0.) THEN
        R0 = NUMERATOR/DENOMINATOR
C
C------ Convert from cm at midplane to TDC counts
C
        DELTA_T0 = -R0/(DERIV0*GAIN0)
      ENDIF
C
      RETURN
      END
