      SUBROUTINE higgs_sector_nocorrs

C       This subroutine corrects all higgs masses using the leading terms
C       from the 1-loop corrections. It also determines the correct value for
C       alpha, the higgs mass mixing parameter, including its radiative
C       corrections.

      IMPLICIT NONE
      INCLUDE 'D0$SPYTHIA$INC:VARS.INC'
      INCLUDE 'D0$SPYTHIA$INC:MASSMAT.INC'

      REAL*8 mqq,epsilon,delta,s2alpha

C       Calculate tree level masses
C
      mqq=sqrt(abs(mstop(1)*mstop(2)))

      epsilon=0.

      delta=sqrt((mh(3)**2+mz**2+epsilon)**2-4.*mh(3)**2*mz**2*cos(2.*
     &    atan(tbeta))**2-4.*epsilon*mh(3)**2*sin(atan(tbeta))**2
     &    -4.*epsilon*mz**2*cos(atan(tbeta))**2)

      mh(1)=sqrt(0.5*(mh(3)**2+mz**2-delta))
      mh(2)=sqrt(0.5*(mh(3)**2+mz**2+delta))

      s2alpha=-(mh(3)**2+mz**2)*sin(2.*atan(tbeta))/delta
      halpha=0.5*asin(s2alpha)

      mh(4)=sqrt(mh(3)**2+mw**2)

      RETURN
      END
