C------------------------------------------------------------------------
      SUBROUTINE SAMCEN_CONSEC(nxfine,xfine,layer,card,port,
     1  nxfine2,xfine2)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-
C-   Inputs  : nxfine   number of chsel fine centroids
C-             xfine    list of chsel centroids
C-             layer    samcen card layer
C-             card     samcen card card
C-             port     samcen card port
C-   Outputs : nxfine2  number of chsel centroids after adjacent alg.
C-             xfine2   list of chsel centroids after adjacent alg.
C-
C-   Controls:  note bits 10-13 of chsel lookup is the kj samcen address
C-              this number goes from 1-13 for a1,...,c4
C-
C-   Created  29-DEC-1993   Coordinate
C-
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
      integer consec_type(13)
      data consec_type/13*1/
      integer nunit,sunit,lunit, LUN_CONSEC1, LUN_CONSEC2
      integer idata, IER 
      integer consec(32),isent
      integer ifirst
C	INTEGER GT_USER
C        PARAMETER (GT_USER = 1) ! Default User Unit Number
C      data ifirst/1/
C----------------------------------------------------------------------
c
c       find kj samcen address
c       see what consec type this corresponds to
c
        sadd = address(layer,card)
        if (consec_type(sadd) .eq. 1) lunit = nunit
        if (consec_type(sadd) .eq. 2) lunit = sunit
c
c       if there's only one centroid send it out
c
        nxfine2 = 0
        if (nxfine .eq. 1) then
          nxfine2 = nxfine
          xfine2(nxfine2) = xfine(nxfine)
          return
        end if
c
c       now look at pairs of centroids
c       do the consec lookup for all pairs.
c
        do j = 1,32
          consec(j) = 0
        end do
        isent = 0
c
        do j = 1,nxfine-1
          call mvbits(xfine(j),0,7,addr,0)
          call mvbits(xfine(j),11,2,addr,7)
          call mvbits(xfine(j+1),0,7,addr,9)
          call mvbits(xfine(j+1),11,2,addr,16)

          read (lunit'addr) idata
          if (idata .eq. 0) consec(j) = 1         
c
c         this means the second centroid is consecutive with the first
c
        end do
c
c       now loop through the pairs again and 
c       decide which ones to send out

        do j = 1,nxfine-1
          if (consec(j) .eq. 0 .and. isent .eq. 0) then         ! not consec
            isent = 0
            nxfine2 = nxfine2 + 1
            xfine2(nxfine2) = xfine(j)
          end if
          if (consec(j) .eq. 0 .and. isent .ne. 0) then         ! not consec
            isent = 0
          end if
          if (j .eq. (nxfine-1) .and. consec(j) .eq. 0) then    ! last one
            nxfine2 = nxfine2 + 1
            xfine2(nxfine2) = xfine(j+1)
            go to 20
          end if
          if (consec(j) .eq. 1 .and. isent .eq. 0) then         ! first consec
            isent = 2
            nxfine2 = nxfine2 + 1
            xfine2(nxfine2) = xfine(j+1)                        ! send 2nd one
            go to 20
          end if
          if (consec(j) .eq. 1 .and. isent .eq. 2) then         ! 2nd consec
            isent = 3
             go to 20
          end if
          if (consec(j) .eq. 1 .and. isent .eq. 3) then         ! 3rd consec
            isent = 4
            go to 20
          end if
          if (consec(j) .eq. 1 .and. isent .eq. 4) then         ! 4th consec
            isent = 0
            if (j .eq. (nxfine-1)) then                         ! last one
              nxfine2 = nxfine2 + 1
              xfine2(nxfine2) = xfine(j+1)
            end if
            go to 20
          end if
 20     continue
        end do
c
  999 RETURN
C-------------------------------------------------------------------------------
        ENTRY SAMCEN_CONSEC_LUN_SET(LUN_CONSEC1,LUN_CONSEC2)
	SUNIT = LUN_CONSEC1
        NUNIT = LUN_CONSEC2
	RETURN

      END
