	SUBROUTINE IDATE(M,D,Y)
C
C	This routine emulates the VAX IDATE Fortran call.
C	Argument M,D,Y are the month (1-12), day (1-31), and 
C       year (last 2 digits)
C
C	Drew Baden    May 1992
C
	IMPLICIT NONE
C
	INTEGER M,D,Y
C
	INTEGER ID,IT
C
	CALL DATIME(ID,IT)
	Y = ID/10000
	M = (ID - 10000*Y)/100
	D = ID - 100*(ID/100) 
C
	RETURN
	END
