/*
C  VaX byte inversion. 
C
C  Purpose:  Duplicate VXINVB in vax version of CERNLIB for use on
C     unix systems.  VXINVB provides byte inversion from VAX ordering to
C     native ordering of integer*4 types.
C
C  Fortran call:  CALL VXINVB(IARRAY,LENGTH)
C
C  Author:  John D Hobbs
C  Date:    18 FEB, 1994
C
*/
void vxinvb_(iar,nptr)
unsigned int *iar;
int      *nptr;
{
  int i,j,n;
  unsigned char *car,ctemp;

  n = *nptr;
  car = (unsigned char *)iar;
  for( i=0 ; i<n ; i++ ) {
    j=4*i;

    ctemp=car[j];
    car[j]=car[j+3];
    car[j+3]=ctemp;

    ctemp=car[j+2];
    car[j+2]=car[j+1];
    car[j+1]=ctemp;
  }
}
