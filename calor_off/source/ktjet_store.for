C VAX/DEC CMS REPLACEMENT HISTORY, Element KTJET_STORE.FOR
C *1     3-FEB-1994 14:40:36 FRAME "catani jet algorithm"
C VAX/DEC CMS REPLACEMENT HISTORY, Element KTJET_STORE.FOR
      SUBROUTINE ktjet_store
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Store the current results for the KT algorithm
C-
C-
C-   Inputs  : [R] RGIAN : RGIAN used for this run of the algorithm
C-   Outputs :
C-   Controls:
C-
C-   Created  17-JAN-1993   Richard V. Astur
c-   Updated  07-Nov-1995   Changed output bank structure to bring inline
c-                          with rest of d0.
c-                          Use errmsg instead of type *.
c-                          Gordon Watts
C-   Updated  5-Dec-1995    Added in ability to calculate Emfrac,Chfrac,etc
C-                          Brad Abbott
C-
C-   Updated  8-Dec-1995    Bug fix -> Fixed declaration of e1(4) to e1(5)
C-                          Brad Abbott
c-   Updated 16-dec-1995    Fixed up ibm compile problems (tab characters,
c-                          format statements).
c-                          Gordon Watts (Brown)
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:KTJET.INC'
      INCLUDE 'D0$INC:KTJET_LINK.INC'
      INCLUDE 'D0$INC:L2LINK.INC'               ! zebra link area
      INCLUDE 'D0$PARAMS:CAPH.DEF'
      include 'd0$inc:pi.def'
      LOGICAL LEAD_JET
      REAL template(20)
      INTEGER lcaph, i, ier, inext, nlinks, ljpts, j
      INTEGER ifirst, ilink, iycut
      REAL eta1, phi1, em1, e1(5), et
      REAL estart, etstart, etastart, phistart
      EQUIVALENCE( lcaph, l2link(5) )
      EQUIVALENCE( ljpts, l2link(6) )
      CHARACTER*4 path1

      character*80 line

      character*6 jet_alg_name
      integer gzcaph

      logical store_as_subjets

      INCLUDE 'D0$INC:KTJET_UTIL.DEF'
c----------------------------------------------------------------------

c
c  Save the envrionment
c

      CALL pathgt(path1)
      CALL pathst('RECO')
      CALL MKPATH

c: Activate zebra link area

      CALL mzlint(ixcom,'/L2LINK/',dum,l2link(nlnk),dum)

C
C: Try an find a caph bank for storing these jets.  If we can't
c  find one, then we will create it.
C

      store_as_subjets = DO_SUBJETS .and. ktcut(iktcut).ne.0.0

      template(1)=3.
      template(2)=6.
      template(3)= D_SEP
      template(4)=7.
      template(5)= INPUT_TYPE
      template(6)=8.
      template(7)= F_CUT
      IF ( store_as_subjets) THEN
        TEMPLATE(1) = 4.
        template(8)=9.
        template(9)= KTCUT( IKTCUT )
c       template(10)=12.
c       template(11)= FLOAT( ISUBJET )
      ENDIF

      IF ( store_as_subjets ) THEN
         jet_alg_name = 'KT_JET'
      ELSE
         jet_alg_name = 'SC_JET'
      ENDIF
      call set_caph (jet_alg_name, template, ier)

c
c  Great.  Now, if we couldn't find it, then we had better create the
c  damm thing.  First thing to do in case of error is reset_caph, as
c  the bomb above will have put something on the caph stack!
c

      if (ier .ne. 0) then

         call reset_caph ()
         
         CALL bkcaph( lcaph )
         iq( lcaph + 2 ) = 0    ! # OF CACL banks used
         iq( lcaph + 3 ) = 0    ! # OF JETS banks used
         iq( lcaph + 4 ) = A_SC_JET ! Algorithm number (6=SCJETS)
         IF ( store_as_subjets ) IQ( LCAPH + 4 ) = A_KT_JET
         iq( lcaph + 5 ) = 1    ! Algorithm version (=1)
         q( lcaph + 6 )  = D_SEP ! Max angular separation for merging
         q( lcaph + 7 ) = input_type ! What we used to get the jets
         q( lcaph + 8 ) = F_CUT ! Fraction of leading jet cut
         IF ( store_as_subjets ) Q( LCAPH + 9 ) = KTCUT( IKTCUT ) ! Subjet y cut
         IF ( store_as_subjets ) Q( LCAPH + 12) = ISUBJET ! Master Jet id
         q( lcaph + 10) = IFSCALE ! What scale definition we are using
         Q( LCAPH + 11) = FSCALE ! Scale found in this event

c
c  Make sure it went a-ok.  Currently, if we get an error in
c  set_caph we still call reset_caph (goto 1000).  Looking at set_caph
c  I think this is the right thing to do, but I've not tested the code!
c

         call set_caph (jet_alg_name, template, ier)
         if (ier .ne. 0) then
	    call errmsg ('set_caph', 'ktjet_store',
     $           'Failed to find just created CAPH bank', 'w')
	    goto 1000
         endif
      endif

c
c  Make sure to get the lcaph pointer we are now pointing at...
c

      lcaph = gzcaph ()
      if (lcaph .eq. 0) then
         call errmsg ('gzcaph', 'ktjet_store',
     $        'Failed to get caph pointer!', 'w')
         goto 1000
      endif

c
C  We can now loop over all the active jets in the lkvec and lkmap arrays
c  and book a jets banks for each
C
      j=0
      DO i = 1, iq( lkvec + 3 )
         IF ( iq( point(i)) .GT. 0 ) THEN
            IF ( input_type .EQ. 6 ) THEN
               j = j + 1
               xgian( 1 + (j-1)*gian_block_size ) = q( kteta( i ) )
               xgian( 2 + (j-1)*gian_block_size ) = q( ktphi( i ) )
               xgian( 3 + (j-1)*gian_block_size ) = q( ktet( i ) )
               ngian = j
            ENDIF
            CALL bkjets( ljets )
            iq( lcaph + 3 ) = iq( lcaph + 3 ) + 1 ! CAPH # of jets
            if ( store_as_subjets ) then
               iq( ljets - 5 ) = isubjet ! JETS bank number
            else
               iq( ljets - 5 ) = iq(lcaph+3)
            endif
            q( ljets + 2 ) = q( px( i ) )
            q( ljets + 3 ) = q( py( i ) )
            q( ljets + 4 ) = q( pz( i ) )
            q( ljets + 5 ) = q( p0( i ) )
            q( ljets + 6 ) = q( ktet( i ))
            q( ljets + 9 ) = q( kteta( i ))
            q( ljets + 8 ) = q( ktphi( i ))
            q( ljets + 7 ) = 2.*ATAN(EXP(-q(ljets+9) ))
            q( ljets + 22) = q( kt_cell_em(i))
            q( ljets + 23) = q( kt_tot_cells(i))
            q( ljets + 24) = q( kt_cell_fh(i))
            q( ljets + 25) = q( kt_cell_icd(i))
            q( ljets + 14) = q( kt_em_frac(i))
            q( ljets + 17) = q( kt_icd_frac(i))
            q( ljets + 18) = q( kt_fh_frac(i))
            
c          IF ( q( ktet(i))/abs(q(ljets+5)) .lt. .01 ) then
c            q(ljets+8) = 0.
c            IF ( q( ljets+4) .gt. 0.) then
c              q(ljets+9) = 10.
c              q(ljets+7) = .00159
c            ELSE
c              q(ljets+9) = -10.
c              q(ljets+7) = 3.14
c            ENDIF
c          ELSE
c            CALL etoeta( q( ljets + 2 ), q(ljets+8), q(ljets+7),
c     &          q(ljets +9))
C               ETA        !  P(4)           PHI         THETA
c          ENDIF
            q( ljets + 10) = 0.0 ! SIG**2 EX
            q( ljets + 11) = 0.0 ! SIG**2 EY
            q( ljets + 12) = 0.0 ! RMS ETA
            q( ljets + 13) = 0.0 ! RMS PHI
c     q( ljets + 14) = 0.0       ! EMF
C
C: Loop over pointers. Fill JPTS bank with all the channels that made up this
C: jet. Also calculate the RMS eta,phi and the emfraction of this jet.
C
C: Count number of 'cells'. We are done when we get back to the beginning.
C
c  We can only do this for input_types of 5 or less. :(
C

            if (input_type .lt. 5
     $           .and. (input_type .ne. 1
     $           .and. input_type .ne. 4)) then
               nlinks = 0
               ifirst = iq( point(i) )
               inext  = ifirst
 120           nlinks = nlinks + 1
               inext  = iq( next_map( inext ) )
               IF ( inext .NE. ifirst ) GOTO 120
C: book JPTS
 125           CALL bkjpts( ljets, nlinks, ljpts )
C: loop again. This time fill JPTS and some JETS quantities
               estart = 0.0     ! energy
               etstart= 0.0     ! et
               etastart= q(kteta(i)) ! eta
               phistart= q(ktphi(I)) ! phi
c     q(ljets+14) = 0.0  ! em fraction
               q(ljets+12) = 0.0 ! eta width
               q(ljets+13) = 0.0 ! phi width
c     lcaeh  = gzcaeh()
               nlinks = 0
               inext  = iq( point(i) )
               ifirst = inext
 140           nlinks = nlinks + 1
               ilink  = iq( link_map( inext ) )
               iq( ljpts + ( 2 + nlinks ) ) = ilink
c          if ( input_type .eq. 3 .and. lcaeh .gt. 0 ) then
c            estart = estart + q(lcaeh + 17*(ilink-1) + 7)
c          endif
               IF (  ilink .GT. 0 ) THEN
                  CALL KTJET_GET_LINK_INFO( ETA1, PHI1, EM1, E1, ILINK,
     &                 INPUT_TYPE )
                  estart            = estart + e1(4)
                  etstart           = etstart + e1(5)
c     q( ljets + 14 )   = q( ljets + 14 ) + em1
                  q( ljets + 12 )   = q( ljets + 12 ) + e1(5)*
     &                 (etastart-eta1)**2
                  q( ljets + 13 )   = q( ljets + 13 ) + e1(5)*
     &                 min(abs(phistart-phi1),
     $                 sngl(twopi)-abs(phistart-phi1) )**2
               ENDIF
               inext  = iq( next_map( inext ) )
               IF ( inext .NE. ifirst )  GOTO 140
c
c: Check consitency and normalize quantities
C
               if ( abs(estart-q(p0(i))) .gt. .01 ) then
                  call errmsg('Inconsistent cells-0','ktjet_store',
     &                 'Sum of cells do not match jet','W')
                  write (line, '(a,f6.2,f6.2,i2)')
     $                 'cell mismatch: estart, q, input_type:',
     $                 estart, q(p0(i)), input_type
                  call errmsg('Inconsistent cells-0','ktjet_store',
     $                 line, 'w')
             endif
             if ( etstart .gt. .1 ) then
c     q(ljets+14) = q(ljets+14)/etstart
                q(ljets+12) = q(ljets+12)/etstart
                q(ljets+13) = q(ljets+13)/etstart
             else
                q(ljets+12) = 0.0
                q(ljets+13) = 0.0
                q(ljets+14) = 0.0
             endif
c     type *, estart, q(p0(i))
             iq(ljpts+2) = nlinks
c     145         CONTINUE
          endif
       ENDIF
      ENDDO
c
c  Done with this set of jets.  Better go!


1000	continue

	call reset_caph ()
	call pathst (path1)

	return
      END
