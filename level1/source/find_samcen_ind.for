C-----------------------------------------------------------------------------
      SUBROUTINE FIND_SAMCEN_IND(otc,layer,card,port)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-
C-   Inputs  : otc number
C-   Outputs : samcen layer, samcen port
C-             samcen layer = 1,2,3 for north
C-             samcen layer = 4,5,6 for south
C-             samcen card = 1,2,3,4,5
C-             samcen port = 1,2 for both
C-   Controls: 
C-
C-   Created  29-DEC-1993   Coordinate
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      integer otc
      integer layer(3),card(3),port(3)
      integer lunit,usunit
c
      lunit = usunit()
c
      if (otc .eq. 600) then
        layer(1) = 3
        card(1) = 3
        port(1) = 1
        layer(2) = 1
        card(2) = 4
        port(2) = 1
        layer(3) = 1
        card(3) = 3
        port(3) = 2
      end if
c
      if (otc .eq. 602) then
        layer(1) = 3
        card(1) = 3
        port(1) = 2
        layer(2) = 1
        card(2) = 4
        port(2) = 1
        layer(3) = 1
        card(3) = 3
        port(3) = 1
      end if
c
      if (otc .eq. 604) then
        layer(1) = 3
        card(1) = 4
        port(1) = 1
        layer(2) = 1
        card(2) = 4
        port(2) = 2
        layer(3) = 1
        card(3) = 3
        port(3) = 1
      end if
c
      if (otc .eq. 606) then
        layer(1) = 3
        card(1) = 4
        port(1) = 2
        layer(2) = 1
        card(2) = 4
        port(2) = 2
        layer(3) = 1
        card(3) = 3
        port(3) = 2
      end if
c
      if (otc .eq. 610) then
        layer(1) = 2
        card(1) = 5
        port(1) = 1
        layer(2) = 2
        card(2) = 4
        port(2) = 1
        layer(3) = 2
        card(3) = 3
        port(3) = 1
      end if
c
      if (otc .eq. 612) then
        layer(1) = 2
        card(1) = 5
        port(1) = 2
        layer(2) = 2
        card(2) = 4
        port(2) = 2
        layer(3) = 2
        card(3) = 3
        port(3) = 2
      end if
c
      if (otc .eq. 614) then
        layer(1) = 2
        card(1) = 5
        port(1) = 1
        layer(2) = 2
        card(2) = 4
        port(2) = 2
        layer(3) = 2
        card(3) = 1
        port(3) = 1
      end if
c
      if (otc .eq. 616) then
        layer(1) = 2
        card(1) = 5
        port(1) = 2
        layer(2) = 2
        card(2) = 4
        port(2) = 1
        layer(3) = 2
        card(3) = 1
        port(3) = 2
      end if
c
      if (otc .eq. 660) then
        layer(1) = 1
        card(1) = 1
        port(1) = 2
        layer(2) = 2
        card(2) = 1
        port(2) = 2
        layer(3) = 3
        card(3) = 1
        port(3) = 2
      end if
c
      if (otc .eq. 662) then
        layer(1) = 1
        card(1) = 1
        port(1) = 1
        layer(2) = 2
        card(2) = 1
        port(2) = 1
        layer(3) = 3
        card(3) = 1
        port(3) = 1
      end if
c
      if (otc .eq. 664) then
        layer(1) = 1
        card(1) = 2
        port(1) = 2
        layer(2) = 2
        card(2) = 2
        port(2) = 2
        layer(3) = 3
        card(3) = 2
        port(3) = 2
      end if
c
      if (otc .eq. 666) then
        layer(1) = 1
        card(1) = 2
        port(1) = 1
        layer(2) = 2
        card(2) = 2
        port(2) = 1
        layer(3) = 3
        card(3) = 2
        port(3) = 1
      end if
c
      if (otc .eq. 630) then
        layer(1) = 6
        card(1) = 3
        port(1) = 1
        layer(2) = 4
        card(2) = 4
        port(2) = 1
        layer(3) = 4
        card(3) = 3
        port(3) = 2
      end if
c
      if (otc .eq. 632) then
        layer(1) = 6
        card(1) = 3
        port(1) = 2
        layer(2) = 4
        card(2) = 4
        port(2) = 1
        layer(3) = 4
        card(3) = 3
        port(3) = 1
      end if
c
      if (otc .eq. 634) then
        layer(1) = 6
        card(1) = 4
        port(1) = 1
        layer(2) = 4
        card(2) = 4
        port(2) = 2
        layer(3) = 4
        card(3) = 3
        port(3) = 1
      end if
c
      if (otc .eq. 636) then
        layer(1) = 6
        card(1) = 4
        port(1) = 2
        layer(2) = 4
        card(2) = 4
        port(2) = 2
        layer(3) = 4
        card(3) = 3
        port(3) = 2
      end if
c
      if (otc .eq. 640) then
        layer(1) = 5
        card(1) = 5
        port(1) = 1
        layer(2) = 5
        card(2) = 4
        port(2) = 1
        layer(3) = 5
        card(3) = 3
        port(3) = 1
      end if
c
      if (otc .eq. 642) then
        layer(1) = 5
        card(1) = 5
        port(1) = 2
        layer(2) = 5
        card(2) = 4
        port(2) = 2
        layer(3) = 5
        card(3) = 3
        port(3) = 2
      end if
c
      if (otc .eq. 644) then
        layer(1) = 5
        card(1) = 5
        port(1) = 1
        layer(2) = 5
        card(2) = 4
        port(2) = 2
        layer(3) = 5
        card(3) = 1
        port(3) = 1
      end if
c
      if (otc .eq. 646) then
        layer(1) = 5
        card(1) = 5
        port(1) = 2
        layer(2) = 5
        card(2) = 4
        port(2) = 1
        layer(3) = 5
        card(3) = 1
        port(3) = 2
      end if
c
      if (otc .eq. 670) then
        layer(1) = 4
        card(1) = 1
        port(1) = 2
        layer(2) = 5
        card(2) = 1
        port(2) = 2
        layer(3) = 6
        card(3) = 1
        port(3) = 2
      end if
c
      if (otc .eq. 672) then
        layer(1) = 4
        card(1) = 1
        port(1) = 1
        layer(2) = 5
        card(2) = 1
        port(2) = 1
        layer(3) = 6
        card(3) = 1
        port(3) = 1
      end if
c
      if (otc .eq. 674) then
        layer(1) = 4
        card(1) = 2
        port(1) = 2
        layer(2) = 5
        card(2) = 2
        port(2) = 2
        layer(3) = 6
        card(3) = 2
        port(3) = 2
      end if
c
      if (otc .eq. 676) then
        layer(1) = 4
        card(1) = 2
        port(1) = 1
        layer(2) = 5
        card(2) = 2
        port(2) = 1
        layer(3) = 6
        card(3) = 2
        port(3) = 1
      end if
c
  999 RETURN
      END
