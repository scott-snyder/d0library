      SUBROUTINE VTX_END_GAINS(LUN)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Calculates relative gain constants
C-
C-   Inputs  :
C-              LUN : Unit Number of the input file
C-
C-   Outputs :
C-   Controls:
C-
C-   Created  16-SEP-1992   John Hauptman
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:VTXCOFF.INC/LIST'
      INTEGER M_WIRE,M_SECTOR,M_LAYER,NE,IE
      REAL R_PER_CM
      PARAMETER (M_wire = 7)
      PARAMETER (M_SECTOR = 31)
      PARAMETER (M_LAYER = 2)
      PARAMETER (R_per_cm = 18.)
      REAL el_half(0:2)
      REAL x_wire, y_wire
      REAL      Sig_Fit(3)
      CHARACTER c_lsw*11
      LOGICAL hExist
      REAL    dz_sum     (0:M_wire,0:M_sector,0:M_layer),
     &          dz_2_sum   (0:M_wire,0:M_sector,0:M_layer),
     &          Epsilon_sum(0:M_wire,0:M_sector,0:M_layer)
      INTEGER Nz(0:M_wire,0:M_sector,0:M_layer), Iteration
      REAL DZ_WINDOW,ZCDC,SIG_ZCDC,A0,A1,ATOT
      REAL ALPHA,R0,R1,ZVTX,Z_QDIV,DZ,RR
      REAL DALPHA,EPS,FN,AVG,SIG,EPS_AVG
      REAL C,EPS_FIT,S,CHI2,DZ_RMS
      REAL VAR
      INTEGER L,K,I,IH,LUN,ID,IC,IG
      DATA ITERATION /0/
      DATA el_half / 48.3, 53.3, 58.4 /  ! cm,
C-------------------------------------------------------------------------
      Iteration = Iteration + 1
C
      IF ( iteration .EQ. 1 ) THEN
        DO l = 0, M_Layer               ! Book for epsilon distributions.
          DO k = 0, M_sector
            DO i = 0, M_wire
              ih = (l+1)*1000 + k*10 + i
              WRITE(c_lsw,'(A,I1,I2,A,I1,A)') 'eps',l,k,'-',i,'$'
              CALL hBook1( ih,c_lsw, 50, -.5, +.5, 0.)
            END DO
          END DO
        END DO
C
        dz_Window = 10.0
C
      ENDIF
C
      CALL VFILL( dz_sum,  (M_wire+1)*(M_sector+1)*(M_layer+1), 0. )
      CALL VFILL( dz_2_sum,(M_wire+1)*(M_sector+1)*(M_layer+1), 0. )
      CALL VFILL( Nz,      (M_wire+1)*(M_sector+1)*(M_layer+1), 0. )
      CALL VFILL( Epsilon_sum,
     &          (M_wire+1)*(M_sector+1)*(M_layer+1), 0. )
C
      CALL hBook1(80+Iteration,'dz $',      50,-10.,+10., 0.)
C
  100 READ(LUN,end=200) l,k,i,zCDC,sig_zCDC,A0,A1,ID
 1234 FORMAT(1x,i1,2x,i2,2x,i1,2(2x,F7.2),2(2x,f8.1),2X,I1 )
C
      A0 = A0 * (1. - Epsilon( i,k,l ) / 2.)
      A1 = A1 * (1. + Epsilon( i,k,l ) / 2.)
      Atot = A0 + A1
      alpha = (A1-A0) / Atot
      r0 = r_input(1,i,k,l)
      r1 = r_input(2,i,k,l)
      zVTX = z_Qdiv(l,A0,A1,r0,r1)
C
      dz = zVTX - zCDC
C
      CALL hFill(80+Iteration,dz,0.,1.)
C
      rr = (r_input(2,i,k,l) + r_input(1,i,k,l))
     &                          / (2. * el_half(l) * R_per_cm)
      dalpha = ( - dz / el_half(l) ) / ( 1. + rr )
      eps = 2. * dalpha / (1.-alpha * (alpha+dalpha))
      ih = (l+1)*1000 + k*10 + i
      CALL hFill(ih,eps,0.,1.)
C
      IF(abs(dz).LT.dz_Window)  THEN
        dz_sum     (i,k,l) = dz_sum     (i,k,l) + dz
        dz_2_sum   (i,k,l) = dz_2_sum   (i,k,l) + dz**2
        epsilon_sum(i,k,l) = epsilon_sum(i,k,l) + eps
        Nz         (i,k,l) = Nz         (i,k,l) + 1
      END IF
      go to 100
C
  200 DO l = 0, M_layer
        DO k = 0, M_sector
          DO i = 0, M_wire
            fn  = Nz(i,k,l)
            IF(fn.GE.50.)  THEN
              avg = dz_sum(i,k,l) / fn
              var = dz_2_sum(i,k,l)/fn - avg**2
              IF(var.GE.0)  sig = Sqrt( var )
              eps_Avg = epsilon_sum(i,k,l) / fn
              ih = (l+1)*1000 + k*10 + i
              IC = 100
              CALL hFitGa(ih, c,eps_Fit,s,Chi2,IC,Sig_Fit)
              CALL hReset(ih,' ')
C
              IF( abs(Eps_Fit).GT. 0.5 )  Eps_Fit = Eps_Avg
              IF( abs(Eps_Fit).GT. 0.5 )  Eps_Fit = 0.
C
              epsilon(i,k,l) = epsilon(i,k,l) + eps_Fit   ! augment epsilon
            END IF
          END DO
        END DO
      END DO
C
      IG = 100
      CALL hFitGa(80+Iteration,c,avg,dz_rms,Chi2,IG,Sig_Fit)
      dz_Window = 3. * dz_rms
C
  999 RETURN
C
      ENTRY VTX_END_GAINS_CLEAR
C
      DO l = 0, M_Layer
        DO k = 0, M_sector
          DO i = 0, M_wire
            ih = (l+1)*1000 + k*10 + i
            IF(hExist(ih))  CALL hDelet(ih)
          END DO
        END DO
      END DO
C
      END
