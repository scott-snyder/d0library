	SUBROUTINE TIME(ARG)
C
C	This routine emulates the VAX TIME Fortran call.
C	Argument ARG is the time as a CHARACTER*8 of the
C	form hh:mm:ss
C
	IMPLICIT NONE
C
	CHARACTER*8 ARG
C
	CHARACTER*8 ND,NT
C
	CALL DATIMH(ND,NT)
	ARG = NT
	ARG(3:3) = ':'
	ARG(6:6) = ':'
C
	RETURN
	END
