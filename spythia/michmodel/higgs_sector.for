      SUBROUTINE higgs_sector

C       This subroutine corrects all higgs masses using the leading terms
C       from the 1-loop corrections. It also determines the correct value for
C       alpha, the higgs mass mixing parameter, including its radiative
C       corrections.

      IMPLICIT NONE
      INCLUDE 'D0$SPYTHIA$INC:VARS.INC'
      INCLUDE 'D0$SPYTHIA$INC:MASSMAT.INC'

      REAL*8 mqq,epsilon,delta,s2alpha,sqqrt,gtop,e,ma_sign,sgn
      EXTERNAL sqqrt,sgn

      e=exp(1.0)
      ma_sign=sgn(mh(3))

C       Calculate first the tree level mh0
C
      epsilon=0.
      delta=sqrt((mh(3)**2+mz**2+epsilon)**2-4.*mh(3)**2*mz**2*cos(2.*
     &    atan(tbeta))**2-4.*epsilon*mh(3)**2*sin(atan(tbeta))**2
     &    -4.*epsilon*mz**2*cos(atan(tbeta))**2)

      mh0=sqrt(0.5*(mh(3)**2+mz**2+epsilon-delta))

C       Now calculate 1-loop corrected masses
C
      gtop=sqrt(2.*pi*alpha(2))*mq(5)/(mw*sin(atan(tbeta)))

      delta=1./(32.*pi**2*sin(atan(tbeta))**2)
     &    *(3.*(4.*pi*alpha(2))*(mq(5)**2/mw**2)*atopz*200.*muz
     &    /(mstop(2)**2-mstop(1)**2)*2.*(mstop(2)**2*log(mstop(2)**2
     &    /(e*mz**2))-mstop(1)**2*log(mstop(1)**2/(e*mz**2))))

      delta=delta+1./(32.*pi**2*cos(atan(tbeta))**2)
     &    *(3.*(4.*pi*alpha(2))*(mq(6)**2/mw**2)*abotz*200.*muz
     &    /(msbot(2)**2-msbot(1)**2)*2.*(msbot(2)**2*log(msbot(2)**2
     &    /(e*mz**2))-msbot(1)**2*log(msbot(1)**2/(e*mz**2))))

      mh(3)=sqqrt(ma_sign*mh(3)**2-delta/sin(2.*atan(tbeta)))

      mqq=sqrt(abs(mstop(1)*mstop(2)))

      epsilon=3.*alphaw/(4.*pi*sw2*(1.-sw2)*mz**2)*(mq(5)**4
     &    /sin(atan(tbeta))**2)*log(mqq**4/mq(5)**4)
      IF (epsilon.LT.0.) epsilon=0.

      delta=sqrt((mh(3)**2+mz**2+epsilon)**2-4.*mh(3)**2*mz**2*cos(2.*
     &    atan(tbeta))**2-4.*epsilon*mh(3)**2*sin(atan(tbeta))**2
     &    -4.*epsilon*mz**2*cos(atan(tbeta))**2)

      mh(1)=sqrt(0.5*(mh(3)**2+mz**2+epsilon-delta))
      mh(2)=sqrt(0.5*(mh(3)**2+mz**2+epsilon+delta))

      s2alpha=-(mh(3)**2+mz**2)*sin(2.*atan(tbeta))/delta
      halpha=0.5*asin(s2alpha)

      delta= -3.*alphaw/(8.*pi*sw2)*(mq(5)**2/sin(atan(tbeta))**2)*
     &    log(mqq**2/mq(5)**2)
      mh(4)=sqrt(mh(3)**2+mw**2+delta)

      RETURN
      END
