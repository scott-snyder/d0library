C*********************************************************************
 
C   Parametrization of parton distributions in a virtual photon
C   Author: Gerhard A. Schuler, CERN-Th
C   13 August 1992
C...DDILOG calls replaced by PYDILN ones to make program
C...self-contained. (TS 30 September 1992)
C
      SUBROUTINE PYSTGS(X,Q2,P2,ALAM,NF,PDFUNC)
C
C   X      : Bjorken-x
C   Q2     : deep inelastic scale (e.g. scale of probing photon)
C   P2     : Scale of probed photon
C   ALAM   : Lambda_QCD in leading order
C   NF     : number of massless flavours
C   PDFUNC : number densities ordered according to
C            Sigma  Delta  G  u  d  s  c  b  t
C            where Sigma and Delta are define as in CERN-6519/92
C   Note   : antiquark densities = quark densities
C   Validity range:
C            Lambda^2 <= P^2 <= Q^2
C                   1 <= Nf  <= 6
C
      IMPLICIT NONE
      DOUBLE PRECISION X,Q2,P2,ALAM,PDFUNC(-2:6)
      INTEGER NF
C
      DOUBLE PRECISION AL2PI,AVCHSQ(1:6),CHARSQ(1:6),ALAM2,ACHAR2
     >,FACTOR,CSINGL,AVCHQU(1:6),ACHAR4,CNONSG
     >,gsum,ssum,dsum,t2,t3,t4,t5,t8,t9,t11,t14,t17,t19
     >,t20,t21,t27,t28,t29,t31,t34,t35,t42,t43,t54,t67,t68
     >,t76,t78,t79,t80,t97,t105,t111
     >,Pi,epsi,ANF,PYDILN
C  Li2(x) = DDILOG(x) from GENLIB C304; replaced by PYDILN.
      INTEGER ILOOP
      DATA AL2PI/1.161714913D-3/
      DATA Pi/3.14159D0/
C  sum_f e_f^2
      DATA AVCHSQ/0.4444444444D0,0.5555555556D0,0.6666666667D0,
     >             1.111111111D0, 1.222222222D0, 1.666666667D0/
C  e_f^2, f=u,d,s,c,b,t
      DATA CHARSQ/0.4444444444D0,0.1111111111D0,0.1111111111D0,
     >            0.4444444444D0,0.1111111111D0,0.4444444444D0/
C  sum_f e_f^4
      DATA AVCHQU/0.1975308642D0,0.2098765432D0,0.2222222222D0,
     >            0.4197530864D0,0.4320987654D0,0.6296296296D0/
C
      ALAM2  = ALAM*ALAM
C  error handlings
      IF(NF.LT.1.OR.NF.GT.6) GOTO 110
      IF( X.LT.0.OR. X.GT.1) GOTO 120
      IF(P2.LT.ALAM2.OR.P2.GT.Q2) GOTO 130
      IF( X.GE.0.9999D0) GOTO 140
C  <e^2>
      ACHAR2 = AVCHSQ(NF)/DBLE(NF)
C  <e^4>
      ACHAR4 = AVCHQU(NF)/DBLE(NF)
C  alpha_{em}/(2 Pi) s
      FACTOR = AL2PI*DLOG(Q2/P2)
      EPSI   = DLOG(Q2/P2)/DLOG(Q2/ALAM2)
      ANF    = DBLE(NF)
C  singlet and non-singlet factors C_S and C_{NS}
      CSINGL = 6D0*DBLE(NF)*ACHAR2
      CNONSG = 6D0*DBLE(NF)*(ACHAR4 - ACHAR2*ACHAR2)
C
      t2 = 11D0-2D0/3D0*ANF
      t3 = 1D0/t2
      t4 = 1D0+x
      t5 = dlog(x)
      t8 = -x
      t9 = 1D0/x
      t11 = x**2
      t14 = t3*(2D0*t4*t5+1D0+t8+4D0/3D0*t9-4D0/3D0*t11)
      t17 = epsi**2
      t19 = t2**2
      t20 = 1D0/t19
      t21 = 2D0*x
      t27 = 1D0+t8
      t28 = dlog(t27)
      t29 = t5*t28
      t31 = 4D0*t4*t29
      t35 = t5**2
C     t34 = -4D0*t4*dilog(t9)
C use    dilog(1/x) = -dilog(x)-ln(x)^2/2 = -Li2(1-x)-ln(x)^2/2
      t34 = -4D0*t4*(-PYDILN(1D0-x) - t35/2D0)
      t42 = t28*(2D0-2D0*x+8D0/3D0*t9-8D0/3D0*t11)
      t43 = -ANF/9D0
      t54 = 8D0/3D0*t11
      t67 = t27**2
      t68 = t11+t67
      t76 = t3*(t68*(2D0*t28-t5)-2D0*t11*t5+t21-1D0/2D0)
      t78 = 4D0/3D0*epsi*t76
      t79 = 8D0/3D0*t76
      t80 = t28**2
      t97 = Pi**2
C
      t111 = PYDILN(x)
C     t111 = dilog(t27) = dilog(1-x) = Li2(x)
      t105 = 64D0/9D0*t20*(t68*(4D0*t80-4D0*t29+t35/2D0)
     >   +8D0*t11*t111      +3D0*t11*t35-3D0*x*t5+(8D0*x-2D0)*t28
     >  -x/2D0+11D0/4D0-t97*(6D0*t11+2D0*t67)/3D0)
C     feld(1) = 4D0/3D0*epsi*t14+t17*(8D0/3D0*t14
      gsum    = 4D0/3D0*epsi*t14+t17*(8D0/3D0*t14
     >  +16D0*t20*(t5*(t21-8D0/3D0*t9+t2*t4/3D0)+t31+t34-6D0*x*t35
     >  +t42+t27*(41D0/2D0+t43)+4D0/3D0*(t9-t11)*(-11D0/2D0+t43))
     >  +64D0/9D0*t20*(t5*(t21+t54)+t31+t34-3D0*t4*t35+t42
     >  -23D0/6D0+23D0/6D0*x))/6D0
C     feld(2) = t11+t67+t78+t17*(t79+t105)/6D0
      dsum    = t11+t67+t78+t17*(t79+t105)/6D0
C     feld(3) = t11+t67+t78+t17*(t79+t105+16D0/3D0*t20*ANF*
      ssum    = t11+t67+t78+t17*(t79+t105+16D0/3D0*t20*ANF*
     >            ((t21-1D0)*t35+(t54-3D0)*t5+8D0/9D0*t9
     >   -9D0+15D0*x-62D0/9D0*t11))/6D0
C
      PDFUNC(-2) = FACTOR*ssum
      PDFUNC(-1) = FACTOR*dsum
      PDFUNC( 0) = FACTOR*gsum
      DO 100 ILOOP=1,NF
        PDFUNC(ILOOP) = 3D0*ACHAR2*PDFUNC(-2)
     >                + 3D0*(CHARSQ(ILOOP) - ACHAR2)*PDFUNC(-1)
  100 CONTINUE
      PDFUNC(-2) = PDFUNC(-2)*CSINGL
      PDFUNC(-1) = PDFUNC(-1)*CNONSG
      PDFUNC( 0) = PDFUNC( 0)*CSINGL
      RETURN
C
  110 CONTINUE
      WRITE(6,5000)
      WRITE(6,5100) NF
      GOTO 140
C
  120 CONTINUE
      WRITE(6,5000)
      WRITE(6,5200) X
      GOTO 140
C
  130 CONTINUE
      WRITE(6,5000)
      WRITE(6,5300) Q2,P2,ALAM
      GOTO 140
C
  140 CONTINUE
      DO 150 ILOOP=-2,6
      PDFUNC(ILOOP) = 0D0
  150 CONTINUE
C
      RETURN
 5000 FORMAT(1X,'Message from PHOTON: PDFs put to zero')
 5100 FORMAT(1X,'NF out of range, NF = ',I3)
 5200 FORMAT(1X,'X out of range, X = ',E14.7)
 5300 FORMAT(1X,'Q2, P2 or ALAM out of range, Q2, P2, ALAM = ',3E14.7)
      END
