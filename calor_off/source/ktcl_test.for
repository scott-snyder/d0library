      SUBROUTINE KTCL_TEST
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  21-FEB-1995   Richard V. Astur
c-   Updated  30-oct-1995   Replace references to lhead with GZKTCL!
c-                          Gordon Watts
c-   Updated  07-Nov-1995   use errmsg instead of type *
c-                          Gordon Watts
c-   Updated  13-Nov-1995   Fixed up format statements for IBM compiler
c-                          Gordon Watts
c-   Updated  15-Dec-1995   Fixed up more format statements!  Grrr!
c-                          Gordon Watts
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:KTJET.INC'
      INCLUDE 'D0$INC:KTJET_LINK.INC'
      INTEGER LKTCL, I, NKTCL, IWORD, iword2
      REAL DE, DETA, DPHI, ETA, PHI, ENERGY, EPREC
      real em_frac, icd_frac, fh_frac
      real cell_em, cell_icd, cell_fh, cell_ch
      real d_em_frac, d_icd_frac, d_fh_frac
      real d_cell_em, d_cell_icd, d_cell_fh, d_cell_ch
      real actual_cell_ch

      character*80 line

      integer gzktcl

      INCLUDE 'D0$INC:KTJET_UTIL.DEF'
C----------------------------------------------------------------------
      lktcl = GZKTCL ()
      IF ( LKTCL .LE. 0 ) THEN
        CALL ERRMSG('NO KTCL','ktcl_test','No KTCL bank to test','W')
        RETURN
      ENDIF

c
c  Great -- no go ahead and unpack and test everything.
c

      NKTCL = 0
      iword2 = 0
      DO I = 1, IQ( LKVEC + 3 )
        IF ( IQ( POINT(I) ) .GT. 0 ) THEN
          NKTCL = NKTCL + 1
          IWORD = IQ( LKTCL + ktcl_header_size +
     $         (NKTCL-1)*ktcl_block_size + 1)
          iword2 = iq (lktcl + ktcl_header_size
     $         + (nktcl-1)*ktcl_block_size + 2)
          CALL UNPKTCL( IWORD, iword2, ETA, PHI, ENERGY,
     $         em_frac, icd_frac, fh_frac,
     $         cell_em, cell_icd, cell_fh, cell_ch)

          DETA  = ETA - Q( KTETA(I) )
          DPHI  = PHI - Q( KTPHI(I) )
          DE    = ENERGY - Q( P0(I) )
          d_em_frac = em_frac - q( kt_em_frac(i))
          d_icd_frac = icd_frac - q(kt_icd_frac(i))
          d_fh_frac = fh_frac - q(kt_fh_frac(i))
          d_cell_em = cell_em - q(kt_cell_em(i))
          d_cell_icd = cell_icd - q(kt_cell_icd(i))
          d_cell_fh = cell_fh - q(kt_cell_fh(i))
          actual_cell_ch = q(kt_tot_cells(i)) -
     $           (q(kt_cell_em(i)) + q(kt_cell_icd(i)) +
     $             q(kt_cell_fh(i)))
          d_cell_ch = cell_ch - actual_cell_ch

          EPREC = .01
          IF ( ENERGY .GT. 80. ) EPREC = .11
          IF ( ABS(DETA) .GT. .02 .OR. ABS(DPHI) .GT. .02 .OR. ABS(DE)
     &      .GT. EPREC ) then
             write (line, '(a, f6.4, f6.4, f7.4)') 'Bad DETA/DPHI/DE: ',
     $            DETA,
     $            DPHI, DE
             call errmsg ('bad-packing-0', 'ktcl_test', line, 'w')
             write (line, '(a, f5.3, f5.2, f7.3)') 'original: ',
     $            q(kteta(i)),
     $            q(ktphi(i)), Q( P0(I) )
             call errmsg ('bad-packing-1', 'ktcl_test', line, 'w')
             write (line, '(a, f6.4, f6.4, f8.4)') 'unpacked: ', eta,
     $            phi,
     $            energy
             call errmsg ('bad-packing-2', 'ktcl_test', line, 'w')
          endif

          if ((abs(d_em_frac) .gt. 0.02
     $         .or. abs(d_icd_frac) .gt. 0.02
     $         .or. abs(d_fh_frac) .gt. 0.02)
     $         .and. (q(kt_em_frac(i)) .gt. 0
     $         .and. q(kt_icd_frac(i)) .gt. 0
     $         .and. q(kt_fh_frac(i)) .gt. 0)
     $         .and. (abs(q(kt_em_frac(i))) .lt. 1.0
     $         .and. abs(q(kt_icd_frac(i))) .lt. 1.0
     $         .and. abs(q(kt_fh_frac(i))) .lt. 1.0)) then
             write (line, '(a, f4.2, f4.2, f4.2)') 'Bad fracs: ',
     $            d_em_frac,
     $            d_icd_frac, d_fh_frac
             call errmsg ('bad-packing-frac', 'ktcl_test', line, 'w')
          endif
          if ((abs(d_cell_em) .gt. 1.04
     $         .or. abs(d_cell_icd) .gt. 1.143
     $         .or. abs(d_cell_fh) .gt. 1.1
     $         .or. abs(d_cell_ch) .gt. 2.0)
     $         .and. (q(kt_cell_em(i)) .lt. 64
     $         .and. q(kt_cell_icd(i)) .lt. 16
     $         .and. q(kt_cell_fh(i)) .lt. 32
     $         .and. actual_cell_ch .lt. 8)) then
             write (line, '(a, f5.2, f5.2, f5.2, f5.2)') 'Bad Cells: ',
     $            d_cell_em,
     $            d_cell_icd,
     $            d_cell_fh, d_cell_ch
             call errmsg ('bad-packing-cell-0', 'ktcl_test', line, 'w')
             write (line, '(a,f5.2,f5.2,f5.2,f5.2)') ' unpacked: ',
     $            cell_em,
     $            cell_icd, cell_fh,
     $            cell_ch
             call errmsg ('bad-packing-cell-1', 'ktcl_test', line, 'w')
             write (line, '(a,f5.2,f5.2,f5.2,f5.2)') ' original: ',
     $            q(kt_cell_em(i)),
     $            q(kt_cell_icd(i)), q(kt_cell_fh(i)),
     $            actual_cell_ch
             call errmsg ('bad-packing-cell-2', 'ktcl_test', line, 'w')
          endif
        ENDIF
      ENDDO

c
c  Done.
c

  999 RETURN
      END
