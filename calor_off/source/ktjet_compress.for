C VAX/DEC CMS REPLACEMENT HISTORY, Element KTJET_COMPRESS.FOR
C *1     3-FEB-1994 15:20:28 FRAME "catani jet algorithm"
C VAX/DEC CMS REPLACEMENT HISTORY, Element KTJET_COMPRESS.FOR
      SUBROUTINE ktjet_compress( kill_beam )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Take all the merged entries out of the KVEC bank
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  17-JAN-1993   Richard V. Astur
C-
C-   Update   5-Dec-1995    Brad Abbott.  Added in ability to calculate
C-                          Emfrac,Chfrac etc.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:KTJET.INC'
      INCLUDE 'D0$INC:KTJET_LINK.INC'
      INTEGER ilast, i, next, nchange, ipoint_beam
      LOGICAL kill_beam, kill_it
      INCLUDE 'D0$INC:KTJET_UTIL.DEF'
c----------------------------------------------------------------------
C
C: Calculate how much energy the beam jets have gotten
C

      ilast = 0
      DO i = 1, iq( lkvec + 3 )
        IF ( iq( point(i) ) .GT. 0 ) THEN     ! This is a cluster
          kill_it = .false.                   ! Assume this is NOT a beam jet
          IF ( kill_beam ) THEN               ! Kill the beam jets
C            NEXT = IQ( POINT(I) )
C  110       CONTINUE
C            IF ( IQ(LINK_MAP( NEXT )) .LE. 0 ) THEN
C              KILL_IT = .TRUE.
C              IPOINT_BEAM = NEXT
C            ENDIF
C            NEXT = IQ(NEXT_MAP( NEXT ))
C            IF ( NEXT .NE. IQ( POINT(I) ) ) GOTO 110
            IF ( (q(ktet(i)) + .001 ) .LT. .03*abs(q(pz(i))) ) kill_it
     &        =.true.
          ENDIF
C
C: Subtract beam jets from our global Px,Py and Pz,E and ET
C
          IF ( kill_it ) THEN
            q( lkmap + 5 ) = q( lkmap + 5 ) - q( px( i ) )
            q( lkmap + 6 ) = q( lkmap + 6 ) - q( py( i ) )
            q( lkmap + 7 ) = q( lkmap + 7 ) - q( pz( i ) )
            q( lkmap + 8 ) = q( lkmap + 8 ) - q( p0( i ) )
            q( lkmap + 9 ) = abs( q( lkmap + 9 ) - q(ktet(i)))
          ENDIF
C
          IF ( .NOT. kill_it ) ilast = ilast + 1
          IF ( .NOT. kill_it  .AND. i .NE. ilast ) THEN
            iq( point( ilast ) ) = iq( point( i ) )
            q( px( ilast ))     =  q( px( i ))
            q( py( ilast ))     =  q( py( i ))
            q( pz( ilast ))     =  q( pz( i ))
            q( p0( ilast ))     =  q( p0( i ))
            q( ktet(ilast))     =  q( ktet(i))
            q( kteta(ilast))     =  q( kteta(i))
            q( ktphi(ilast))     =  q( ktphi(i))
            q( kt_tot_cells(ilast)) = q(kt_tot_cells(i))
            q( kt_em_frac(ilast)) = q(kt_em_frac(i))
            q( kt_icd_frac(ilast)) = q(kt_icd_frac(i))
            q( kt_fh_frac(ilast)) = q(kt_fh_frac(i))
            q( kt_cell_em(ilast)) = q(kt_cell_em(i))
            q( kt_cell_icd(ilast)) = q(kt_cell_icd(i))
            q( kt_cell_fh(ilast)) = q(kt_cell_fh(i))
          ENDIF
        ENDIF
      ENDDO
C
C: Shrink bank to new size
C
      IF ( ilast .NE. iq(lkvec+3) ) then
        nchange = ( ilast - iq( lkvec+3 ) )*iq( lkvec + 4 )
        CALL mzpush( ixcom, lkvec, 0, nchange , 'I' )
        iq( lkvec + 3 ) = ilast
      ENDIF
c      IF(IQ(LKVEC+3).GE.80)THEN
c        PRINT*,'INPUT_TYPE =',INPUT_TYPE, '  # OF THINGS IS',
c     &    IQ(LKVEC+3)
c        ENDIF
  999 RETURN
      END
