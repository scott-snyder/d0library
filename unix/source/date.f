	SUBROUTINE DATE(ARG)
C
C	This routine emulates the VAX DATE Fortran call.
C	Argument ARG is the date as a CHARACTER*9 in the form
C       of dd-mmm-yy
C
C       Drew Baden    May 1992
C
	IMPLICIT NONE
C
	CHARACTER*9 ARG
C
        CHARACTER*3 MONTH(12)
	CHARACTER*8 NT,ND
	INTEGER IM
C
	DATA MONTH/'JAN','FEB','MAR','APR','MAY','JUN','JUL',
     &             'AUG','SEP','OCT','NOV','DEC'/
C
	CALL DATIMH(ND,NT)
	ARG(1:2) = ND(1:2)
	ARG(3:3) = '-'
        READ(ND(4:5),'(I2)') IM
	ARG(4:6) = MONTH(IM)
	ARG(7:7) = '-'
	ARG(8:9) = ND(7:8)
C
	RETURN
	END
