C------------------------------------------------------------------------------
      SUBROUTINE SAMCEN_CHSEL(nxfine,xfine,layer,card,port,
     1  nxfine2,xfine2)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-
C-   Inputs  : nxfine   number of fine centroids
C-             xfine    list of fine centroids
C-             layer    samcen card layer
C-             card     samcen card card
C-             port     samcen card port
C-   Outputs : nxfine2  chsel(ected) number of fine centroids
C-             xfine2   list of chsel(ected) fine centroids
C-
C-   Controls:  note bits 10-13 of chsel lookup is the kj samcen address
C-              this number goes from 1-13 for a1,...,c4
C-
C-   Created  29-DEC-1993   Coordinate
C-
C-   1-12-1994 - L. Markosky.  Changed table read-out to .RCP format.
C		SAMCEN tables are now in MU_L15_TABLES_1B.RCP
C----------------------------------------------------------------------
      IMPLICIT NONE
      integer i,j,k
      integer nxfine,xfine(32)
      integer nxfine2,xfine2(32)
      integer layer,card,port
      integer sadd,addr
      integer address(6,5)
      data (address(1,i),i=1,5)/1,2,3,4,0/
      data (address(2,i),i=1,5)/5,6,7,8,9/
      data (address(3,i),i=1,5)/10,11,12,13,0/
      data (address(4,i),i=1,5)/1,2,3,4,0/
      data (address(5,i),i=1,5)/5,6,7,8,9/
      data (address(6,i),i=1,5)/10,11,12,13,0/
      integer nunit,sunit,lunit
      integer idata
      integer ifirst
	INTEGER LUN_SCN, LUN_SCS

      data ifirst/1/
C----------------------------------------------------------------------
c
c       find kj samcen address
c
        addr = 0
        sadd = address(layer,card)
        call mvbits(sadd,0,4,addr,10)
c
c       select table to read, bit to check
c
        nxfine2 = 0
        if (layer .ge. 1 .and. layer .le. 3) lunit = nunit
        if (layer .ge. 4 .and. layer .le. 6) lunit = sunit
        port = port - 1
c
c       now loop over centroids and only send out those that
c       have chsel = 0 in the appropriate port (bit)
c
        do j = 1,nxfine
          call mvbits(xfine(j),0,7,addr,0)
          call mvbits(xfine(j),11,3,addr,7)
c
c       do chsel lookup
c
          read (lunit'addr) idata
          if (.not.(btest(idata,port))) then     ! a 0 means send the centroid
            nxfine2 = nxfine2 + 1
            xfine2(nxfine2) = xfine(j)
          end if
        end do

C      	go to 999
C900    	write (lunit,*) 'error opening nunit'
C        stop
C902    	write (lunit,*) 'error opening sunit'
C	stop

  999 RETURN
C-------------------------------------------------------------------------------
	ENTRY SAMCEN_CHSEL_LUN_SET(LUN_SCN,LUN_SCS)
	  SUNIT = LUN_SCS
	  NUNIT = LUN_SCN
	RETURN
      END
