      SUBROUTINE VERTTRK_MATCH(VERT_PAR,TRK_PAR,DISTANCE,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : compute distance between a given vertex and the
C-                         CD track
C-
C-   Returned value  : DISTANCE : distance between vertex and CD track
C-   Inputs  :         VERT_PAR : vertex x,y,z
C-                     TRK_PAR  : track phi, theta, xcog, ycog, zcog
C-   Outputs : 
C-   Controls: 
C-
C-   Created   5-JUL-1995   Peter Tamburello
C-
C----------------------------------------------------------------------
      implicit none
      real vert_par(3)
      real trk_par(5)
      real distance
      integer ier
      
      real a,b,c
      real s
      real xtrk, ytrk, ztrk, phi, theta
C----------------------------------------------------------------------

      phi = trk_par(1)
      theta = trk_par(2)
      xtrk = trk_par(3)
      ytrk = trk_par(4)
      ztrk = trk_par(5)

      a = cos(phi)*sin(theta)
      b = sin(phi)*sin(theta)
      c = cos(theta)

      s = ( a*(vert_par(1) - xtrk)
     &    + b*(vert_par(2) - ytrk)
     &    + c*(vert_par(3) - ztrk) 
     &    ) / (a**2 + b**2 + c**2)

      distance = ( vert_par(1) - (xtrk+s*a) )**2
     &          +( vert_par(2) - (ytrk+s*b) )**2
     &          +( vert_par(3) - (ztrk+s*c) )**2

      if (distance.gt.0.0) then
        ier = 0
        distance = sqrt(distance)
      else 
        ier = -1
      end if

      return
      end
