        integer numeqs
        parameter (numeqs=26)
	logical sllog(12),sqlog(12),gflog(3)
	common/mass_flags/sqlog,sllog,gflog
        integer neqs,nprint,ndecouple
	common/diffeqs_control/neqs,nprint,ndecouple
        real*8 msusy
        common/diffeq_r8/msusy
        real*8 b(3),bp(3,3),ap(3,3)
	common/bis/b,bp,ap
	integer jerror
	common/jim/jerror
