
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

	subroutine diffeqs_mtau(lnq,vars,eqs)

	implicit none
	real*8 alph,alph3,mtau,pi,nu,nd,nl,Q
	real*8 lnq,vars(3),eqs(3),scale

        scale=exp(lnq)
	alph=vars(1)
	alph3=vars(2)
	mtau=vars(3)
	nu=2.
        if (scale.gt.4.89) then
           nd=3.
        else
           nd=2.
        endif
	nl=3.
	Q=-1.
	pi=3.1415927

	eqs(1)=1./(2.*pi)*((16./9.*nu+4./9.*nd+4./3.*nl)*alph**2
     &	  +(64./27.*nu+4./27.*nd+4.*nl)*alph**3/(4.*pi)
     &	  +(64./9.*nu+16./9.*nd)*alph**2*alph3/(4.*pi))

	eqs(2)=1./(2.*pi)*((2./3.*(nu+nd)-11.)*alph3**2
     &	  +(38./3.*(nu+nd)-102.)*alph3**3/(4.*pi)
     &	  +(8./9.*nu+2./9.*nd)*alph3**2*alph/(4.*pi)
     &	  +(5033./18.*(nu+nd)-325./54.*(nu+nd)**2-2857./2.)*alph3**4
     &	  /(4.*pi)**2)

	eqs(3)=mtau*(-6.*Q**2*alph/(4.*pi)
     &	  +(-3.*Q**4+(80./9.*nu+20./9.*nd+20./3.*nl)*Q**2)*alph**2
     &	  /(4.*pi)**2)


	return
	end
