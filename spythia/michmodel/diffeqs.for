	subroutine diffeqs(lnq,vars,eqs)
C Merged with JTL's Alpha routines and CK's NEW_DIFFEQS.f Basically
C the differences from CK are comments out if statements.
	implicit none
	include 'D0$SPYTHIA$INC:VARS.INC'
        include 'D0$SPYTHIA$INC:DIFFEQ.INC'
	real*8 lnq,vars(numeqs),eqs(numeqs)
	real*8 gtop,gbot,gtau,Atop,Abot,Atau,mu
	real*8 mH1sq,mH2sq,m1sq,m2sq,wsq(12),wsl(12),wgf(3)
	real*8 ratio,alphar(3),scale,bigb
	real*8 t1,t2,t3,tt,tta,tb,b1,b2,b3,bb,bt,bta
	real*8 ta1,ta2,ta3,tata,tab,tat
        logical malog,holog,susylog,toplog
	integer i,nloops
	real*8 Qstep,Lstep,Gstep,sqqrt
	external Qstep,Lstep,Gstep,sqqrt
	equivalence (b2,t2,ta2),(b3,t3)

	nloops=2
c	nprint=0
c	write(*,*) 'scale=',log10(exp(lnq))
c	Assign traditional labels to variables
c
	scale=exp(lnq)/200.
	ratio=Mhalf/alphax/200.
	do i=1,3
	    alphar(i)=vars(i)
	enddo
	if (nprint.eq.1) write(12,*) log10(scale*200.),sngl(alphar(3))
	    gtop=vars(4)
        if (ABS(gtop).gt.gtop_limit) then
            gtop_too_big = .TRUE.
            return
        Endif
	    gbot=vars(5)
        if (ABS(gbot).gt.gtop_limit) then
            gtop_too_big = .TRUE.
            return
        Endif
	    gtau=vars(6)
        if (ABS(gtau).gt.gtop_limit) then
            gtop_too_big = .TRUE.
            return
        Endif
	    Atop=vars(7)
	    Abot=vars(8)
	    Atau=vars(9)
	    BigB=vars(10)
	    mH1sq=vars(11)
	    mH2sq=vars(12)
	    mu=vars(13)
	    wsq(5)=sqqrt(vars(14))
	    wsq(6)=wsq(5)
	    wsq(11)=sqqrt(vars(15))
	    wsq(12)=sqqrt(vars(16))
	    wsl(6)=sqqrt(vars(17))
	    wsl(5)=wsl(6)
	    wsl(12)=sqqrt(vars(18))
	    wsl(2)=sqqrt(vars(19))
	    wsl(1)=wsl(2)
	    wsl(3)=wsl(2)
	    wsl(4)=wsl(2)
	    wsl(8)=sqqrt(vars(20))
	    wsl(10)=wsl(8)
	    wsq(1)=sqqrt(vars(21))
	    wsq(2)=wsq(1)
	    wsq(3)=wsq(1)
	    wsq(4)=wsq(1)
	    wsq(7)=sqqrt(vars(22))
	    wsq(9)=wsq(7)
	    wsq(8)=sqqrt(vars(23))
	    wsq(10)=wsq(8)
	    wgf(1)=vars(24)
	    wgf(2)=vars(25)
	    wgf(3)=vars(26)
	    m1sq=mH1sq+mu**2
	    m2sq=mH2sq+mu**2

c	    if (neqs.ge.7) write(12,*) log10(scale*200.),
c     &		BigB,mu,BigB/mu

c	Now set logical arrays for decoupling
c
	if (ndecouple.eq.1) goto 70

	if (scale*200..lt.mq(5)) then
	    toplog=.false.
	else
	    toplog=.true.
	endif

	do i=1,3
		if (scale.lt.wgf(i)) then
		    gflog(i)=.false.
		else
		    gflog(i)=.true.
		endif
	enddo

	do i=1,12
	    if (scale.lt.wsq(i)) then
		sqlog(i)=.false.
	    else
		sqlog(i)=.true.
	    endif

	    if (scale.lt.wsl(i)) then
		sllog(i)=.false.
	    else
		sllog(i)=.true.
	    endif
	enddo

	if (scale**2.lt.abs(m1sq+m2sq)) then
	    malog=.false.
	else
	    malog=.true.
	endif

	if (scale.lt.abs(mu)) then
	    holog=.false.
	else
	    holog=.true.
	endif

c	Now set the beta-functions for gauge and yukawa couplings (SM values)
c
	b(1)=3.*(4./3.) + 1.*(1./10.) -17./30.
	b(2)=3.*(4./3.) + 1.*(1./6.) -22./3. -1.
	b(3)=3.*(4./3.) -11. -2./3.

c	Now set the yukawa coeffs (SUSY values)
c
	tt=6.
	t1=-13./15.
	t2=-3.
	t3=-16./3.
	tta=0.
	tb=1.
	b1=-7./15.
	bb=6.
	bt=1.
	bta=1.
	ta1=-9./5.
	ta3=0.
	tata=4.
	tab=3.
	tat=0.

c	SUSY 2-loop coefficients
c
	if (((ndecouple.eq.0).and.(scale*200..gt.Mhalf)).or.
     &	  ((ndecouple.eq.2).and.(scale*200..gt.Msusy))) then
	    bp(1,1)=199./25.
	    bp(1,2)=27./5.
	    bp(1,3)=88./5.
	    bp(2,1)=9./5.
	    bp(2,2)=25.
	    bp(2,3)=24.
	    bp(3,1)=11./5.
	    bp(3,2)=9.
	    bp(3,3)=14.
	else
	    bp(1,1)=199./50.
	    bp(1,2)=27./10.
	    bp(1,3)=44./5.
	    bp(2,1)=9./10.
	    bp(2,2)=35./6.
	    bp(2,3)=12.
	    bp(3,1)=11./10.
	    bp(3,2)=9./2.
	    bp(3,3)=-26.
	endif

	ap(1,1)=26./5.
	ap(1,2)=14./5.
	ap(1,3)=18./5.
	ap(2,1)=6.
	ap(2,2)=6.
	ap(2,3)=2.
	ap(3,1)=4.
	ap(3,2)=4.
	ap(3,3)=0.

c	Now change coefficients to account for all coupled particles.
c
	b(2)=b(2) + (4./3.)*Gstep(2)
	b(3)=b(3) + 2.*Gstep(3)

	if (toplog) then
	    b(1)=b(1)+17./30.
	    b(2)=b(2)+1.
	    b(3)=b(3)+2./3.
	endif

	do i=1,12
	    if (i.le.6) then
		b(1)=b(1) + (1./60.)*Qstep(i) + (1./20.)*Lstep(i)
	    elseif (mod(i,2).eq.1) then
		b(1)=b(1) + (4./15.)*Qstep(i)
	    else
		b(1)=b(1) + (1./15.)*Qstep(i) + (1./5.)*Lstep(i)
	    endif
	    if ((i.le.6).and.(mod(i,2).eq.0)) then
		b(2)=b(2) + (1./2.)*Qstep(i)*Qstep(i-1) + (1./6.)
     &		  *Lstep(i)*Lstep(i-1)
	    endif
	    b(3)=b(3) + (1./6.)*Qstep(i)
	enddo

	if (holog) then
	    b(1)=b(1) + 2./5.
	    b(2)=b(2) + 2./3.
	endif

	if (malog) then
	    b(1)=b(1) + 1./10.
	    b(2)=b(2) + 1./6.
	endif

	if (ndecouple.eq.2) then
	    if (scale*200..gt.Msusy) then
		b(1)=6.6
		b(2)=1.
		b(3)=-3.
	    else
		b(1)=4.1
		b(2)=-19./6.
		b(3)=-7.
	    endif
	endif


	if (nprint.eq.1) write(12,*) '   ',sngl(b(1)),
     &	  sngl(b(2)),sngl(b(3))
	susylog=((abs(b(1)-6.6).lt.0.001).and.(abs(b(2)-1.)
     &	  .lt.0.001).and.(abs(b(3)+3.).lt.0.001))
	if ((Log10(200.*scale).gt.6.).and.(.not.susylog)) then
	   write(*,*) 'SUSY threshold never reached.',
     &		' Bs=',b(1),b(2),b(3)
     	   jerror=-3
	endif

c	if (.not.sqlog(1)) then
c	    t3=-8.
c	    t2=-9./4.
c	    t1=-17./20.
c	    tt=9./2.
c	    tb=1./2.
c	endif

 70	continue

c	Initialize all diff eqs to 0
c
	do i=1,numeqs
	    eqs(i)=0.
	enddo

c	Now for the actual diff eqs.
c
	if (nloops.eq.1) then
	    do i=1,3
		eqs(i)=b(i)/(2.*pi)*alphar(i)**2
	    enddo

	    if (neqs.ge.4) then
 		eqs(4)= gtop/(4.*pi)*((tt*gtop**2+tb*gbot**2
     &		  +tta*gtau**2)
     &		  /(4.*pi) + t3*alphar(3)+ t2*alphar(2)
     &		  + t1*alphar(1))

 		eqs(5)=gbot/(4.*pi)*((bt*gtop**2+bb*gbot**2+
     &		  bta*gtau**2)/(4.*pi) + b3*alphar(3)+ b2*alphar(2) +
     &		  b1*alphar(1))

     		eqs(6)=gtau/(4.*pi)*((tata*gtau**2+tab*gbot**2+
     &		  tat*gtop**2)/(4.*pi)  + ta2*alphar(2) + ta1*
     &		  alphar(1))
	    endif

	else

    	    do i=1,3
		eqs(i)=alphar(i)/(8.*pi**2)*(4.*pi*b(i)*alphar(i)
     &		  +alphar(i)*(bp(i,1)*alphar(1)+bp(i,2)*alphar(2)
     &		  +bp(i,3)*
     &		  alphar(3))  - 1./(4.*pi)*alphar(i)*(ap(i,1)*gtop**2
     &		  +ap(i,2)*gbot**2+ap(i,3)*gtau**2))
	    enddo

	    if (neqs.ge.4) then
	     eqs(4)=gtop/(4.*pi)*(t1*alphar(1) + t2*alphar(2)
     &	      +t3*alphar(3) +(tt*gtop**2 +tb*gbot**2 +tta*gtau**2)
     &	      /(4.*pi)
     &	      +1./(4.*pi)*((t1*b(1)+t1**2/2.)*alphar(1)**2
     &	      +(t2*b(2)+t2**2/2.)*alphar(2)**2 +(t3*b(3)+t3**2/2.)
     &	      *alphar(3)**2
     &	      +alphar(1)*alphar(2) +136./45.*alphar(1)*alphar(3)
     &	      +8.*alphar(2)*alphar(3) +(gtop**2*(6./5.*alphar(1)+6.
     &	      *alphar(2)
     &	      +16.*alphar(3)) +2./5.*gbot**2*alphar(1))/(4.*pi)
     &	      -(22.*gtop**4
     &	      +5.*gtop**2*gbot**2 +5.*gbot**4 +gbot**2*gtau**2)
     &        /(4.*pi)**2))

	     eqs(5)=gbot/(4.*pi)*(b1*alphar(1) + b2*alphar(2)
     &	      +b3*alphar(3) +(bt*gtop**2 +bb*gbot**2 +bta*gtau**2)
     &	      /(4.*pi)
     &	      +1./(4.*pi)*((b1*b(1)+b1**2/2.)*alphar(1)**2
     &	      +(b2*b(2)+b2**2/2.)*alphar(2)**2 +(b3*b(3)+b3**2/2.)
     &	      *alphar(3)**2
     &	      +alphar(1)*alphar(2) +8./9.*alphar(1)*alphar(3)
     &	      +8.*alphar(2)*alphar(3) +(gbot**2*(2./5.*alphar(1)
     &	      +6.*alphar(2)
     &	      +16.*alphar(3)) +6./5.*gtau**2*alphar(1)
     &	      +4./5.*gtop**2*alphar(1))/(4.*pi) -(22.*gbot**4
     &	      +5.*gtop**2*gbot**2 +5.*gtop**4 +3.*gbot**2*gtau**2
     &	      +3.*gtau**4)/(4.*pi)**2))

	     eqs(6)=gtau/(4.*pi)*(ta1*alphar(1) +ta2*alphar(2)
     &	      +ta3*alphar(3) +(tat*gtop**2 +tab*gbot**2 +tata*gtau**2)
     &       /(4.*pi)
     &	      +1./(4.*pi)*((ta1*b(1)+ta1**2/2.)*alphar(1)**2
     &	      +(ta2*b(2)+ta2**2/2.)*alphar(2)**2
     &	      +(ta3*b(3)+ta3**2/2.)*alphar(3)**2 +(9./5.)*alphar(1)
     &       *alphar(2)
     &	      +(gbot**2*(-2./5.*alphar(1) +16.*alphar(3))
     &	      +gtau**2*(6./5.*alphar(1) +6.*alphar(2)))/(4.*pi)
     &       -(9.*gbot**4 +3.*gtop**2*gbot**2 +9.*gbot**2*gtau**2
     &	      +10.*gtau**4)/(4.*pi)**2))
	    endif

	endif	    ! Use the rest of the equations at all scales.

	if (neqs.lt.7) goto 100

	eqs(7)= (6.*gtop**2*Atop+gbot**2*Abot)/(8.*pi**2)
     &	      +1./pi*(8./3.*wgf(3)*alphar(3)+3./2.*wgf(2)*alphar(2)
     &	      +(3./5.)*13./18.*wgf(1)*alphar(1))

	eqs(8)= (gtop**2*Atop+6.*gbot**2*Abot+gtau**2*Atau)/(8.*pi**2)
     &	      +1./pi*(8./3.*wgf(3)*alphar(3)+3./2.*wgf(2)*alphar(2)
     &	      +(3./5.)*7./18.*wgf(1)*alphar(1))

	eqs(9)= (3.*gbot**2*Abot+4.*gtau**2*Atau)/(8.*pi**2)
     &	      +1./pi*(3./2.*wgf(2)*alphar(2)
     &	      +(3./5.)*3./2.*wgf(1)*alphar(1))

	eqs(10)=(3.*alphar(2)*wgf(2)+(3./5.)*alphar(1)*wgf(1))
     &	      /(2.*pi) + (3.*gtop**2*Atop+3.*gbot**2*Abot
     &        +gtau**2*Atau)/(8.*pi**2)


	eqs(11)= 1./(2.*pi)*(3.*gbot**2/(4.*pi)
     &	      *(wsq(6)**2+Abot**2+wsq(12)**2
     &	      +mH1sq) + gtau**2/(4.*pi)*(wsl(6)**2+Atau**2
     &	      +wsl(12)**2+mH1sq) - 3.*alphar(2)
     &	      *wgf(2)**2-(3./5.)*alphar(1)*wgf(1)**2)

	eqs(12)= 1./(8.*pi**2)*(3.*gtop**2
     &	      *(mH2sq+wsq(11)**2
     &	      +wsq(5)**2+Atop**2))-1./(2.*pi**2)*(3.*pi
     &	      *wgf(2)**2*alphar(2) + pi*wgf(1)**2
     &	      *(3./5.)*alphar(1))

	eqs(13)= mu/(4.*pi)*((3.*gtop**2+3.*gbot**2+gtau**2)/(4.*pi)
     &	      -(3./5.)*alphar(1)-3.*alphar(2))

	if (sqlog(5)) then
	    eqs(14)= (1./(8.*pi**2)*(gtop**2*(mH2sq+wsq(11)**2
     &	       +wsq(5)**2+Atop**2)+gbot**2*(mH1sq+wsq(6)**2
     &	       +wsq(12)**2+Abot**2))
     &	       - (2./pi)*(4./3.*wgf(3)**2*alphar(3)+3./4.*
     &	       wgf(2)**2*alphar(2)+(3./5.)*wgf(1)**2
     &	       *alphar(1)/36.))
	endif

	if (sqlog(11)) then
	    eqs(15)= (1./(4.*pi**2)*gtop**2*(mH2sq+wsq(11)**2
     &	       +wsq(5)**2+Atop**2)
     &	       - (2./pi)*(4./3.*wgf(3)**2*alphar(3) +
     &	       (3./5.)*4./9.*wgf(1)**2*alphar(1)))
	endif

	if (sqlog(12)) then
	    eqs(16)= (1./(4.*pi**2)*gbot**2*(mH1sq+wsq(12)**2
     &	       +wsq(6)**2+Abot**2)
     &	       -(2./pi)*(4./3.*wgf(3)**2*alphar(3) +
     &	       (3./5.)*1./9.*wgf(1)**2*alphar(1)))
	endif

	if (sllog(6)) then
	    eqs(17)= -1./(2.*pi)*(3.*alphar(2)*wgf(2)**2 +
     &	       (3./5.)*alphar(1)*wgf(1)**2 - gtau**2/(4.*pi)
     &	       *(wsl(6)**2+wsl(12)**2+mH1sq+Atau**2))
	endif

	if (sllog(12)) then
	    eqs(18)= -1./(2.*pi)*(4.*(3./5.)*alphar(1)*wgf(1)**2
     &	       -2.*gtau**2/(4.*pi)*(wsl(12)**2+mH1sq+wsl(6)**2
     &         +Atau**2))
	endif

	if (sllog(2)) then
	    eqs(19)= -1./(4.*pi)*(6.*alphar(2)*wgf(2)**2 +
     &	       2.*(3./5.)*alphar(1)*wgf(1)**2)
	endif

	if (sllog(8)) then
	    eqs(20)= -1./(4.*pi)*(8.*(3./5.)*alphar(1)*wgf(1)**2)
	endif

	if (sqlog(1)) then
	    eqs(21)=- 1./(4.*pi)*(32./3.*alphar(3)
     &	       *wgf(3)**2+6.*alphar(2)*wgf(2)**2 +
     &	       (2./9.)*(3./5.)*alphar(1)*wgf(1)**2)
	endif

	if (sqlog(7)) then
	    eqs(22)= -1./(4.*pi)*(32./3.*alphar(3)*wgf(3)**2 +
     &	       (32./9.)*(3./5.)*alphar(1)*wgf(1)**2)
	endif

	if (sqlog(8)) then
	    eqs(23)= -1./(4.*pi)*(32./3.*alphar(3)*wgf(3)**2 +
     &	       (8./9.)*(3./5.)*alphar(1)*wgf(1)**2)
	endif

	do i=1,3
	    if (gflog(i)) then
		eqs(23+i)=ratio*eqs(i)
	    endif
	enddo

 100	return
	end
