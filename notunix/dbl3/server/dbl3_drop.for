      SUBROUTINE DBL3_DROP(det,path,lpi,ier)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Drop banks under LDATA. 
C-
C-   Inputs  : det = Detector type, One of the following ONLY
C-                 = DBM, CAL, VTX, TRD, CDC or FDC. 
C-             path = path name
C=             lpi = Input link address
C-
C-   Outputs : lpi = Modified Zebra structure (Maybe zero on return)
C-             Ier = Error status = 0 if all goes well
C-             Ier = +2, All banks have been dropped. This error code is chosen
C-             to keep compatible with the dbl3_compress error code.
C
C-   Controls: none.
C-
C-   Created   15-NOV-1993   Srini Rajagopalan/Lars Rasmussen
C-   Modified  20-DEC-1993   S. abachi   dbcomp.inc --> dbl3_comp.inc
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'd0$inc:zebstp.inc'
      INCLUDE 'd0$inc:dbl3_comp.inc'
C
      INTEGER i,j,ilen,trulen,ier
      INTEGER idrop,idtyp
      INTEGER lp,lpi,lf,lzfidh
      INTEGER icls,ndv
C
      LOGICAL fdv
C
      CHARACTER*3   det
      CHARACTER*4   bank
      CHARACTER*12  c12h
      CHARACTER*(*) path
C
C common for dbmon link area...
C
      INTEGER ldrop(max_drop)
      COMMON /DBDROP/ ldrop
C
C----------------------------------------------------------------------
C
      ier = 0
C
      IF (det.eq.'DBM') then                ! ---- Dbmon stuff
c
c- find class
c
        call uHtoC( ic(lpi+5),4,c12h,12 )
        icls = 0
        do i = 1,ncls
          if ( clas(i) .eq. c12h ) then
            icls = i
            goto 10
          end if
        end do
        call mzdrop(ixstp,lpi,' ')
        lpi = 0
        ier = 2
        goto 999
C
c- check class for useful devices
c
   10   lp = lc(lpi-1)
        ndv = 0
        idrop = 0
        do while ( lp .gt. 0 )
          call uHtoC( ic(lp+ic(lp+2)-2),4,c12h,12 )
          fdv = .false.
          do i = 1,ndev(icls)
            if ( c12h .eq. devi( i,icls ) .or.
     &      index( devi( i,icls), 'everything') .gt. 0 .or.
     &      index( devi( i,icls), 'EVERYTHING') .gt. 0) fdv = .true.
          end do
          if ( .not. fdv .and. idrop .lt. max_drop ) then
            idrop = idrop + 1
            ldrop(idrop) = lp
          else
            ndv = ndv + 1
          end if
          lp = lc( lp )
        end do
c
c- drop it
c
        do i = 1,idrop
          call mzdrop(ixstp,ldrop(i),' ')
        end do
c
      ELSE                  ! ---- calib stuff
c
c get correct detector type
c
        do i = 1,num_dtyp
          if (dtyp_calib(i).eq.det) then
            idtyp = i
            do j = 1,npath(idtyp)
              ilen = trulen(calib_path(j,idtyp))
              if (calib_path(j,idtyp)(1:ilen).eq.path(1:trulen(path)))
     &                                        go to 20
            enddo
          endif
        enddo
C
C nothing is needed.
C
        call mzdrop(ixstp,lpi,' ')
        lpi = 0
        ier = 2
        go to 999
C
   20   continue
        do idrop = 1,ndrop(idtyp)
          call uctoh(bank_drop(idrop,idtyp),bank,4,4)
  200     LF = lzfidh(idvstp,bank,0)
          if (lf.gt.0) then
            call mzdrop(ixstp,lf,'L')
            go to 200
          endif
        enddo
      ENDIF
C
      call mzgarb(idvstp,0)
C
  999 RETURN
      END
