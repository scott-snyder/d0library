      SUBROUTINE GETCRATE(bank,maxcrate,ncrates,icrates,ichans)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : for raw data bank, will fetch the number
C-   of crates and the address offset for each crate to be used by
C-   subsequent routines (xcrate, etc.)
C-
C-   much of this is stolen from GZFIND_CRATE
C-
C-   Inputs  : C*4 bank, ncrates, integer icrates(*)
C-   Outputs : none
C-   Controls:
C-
C-   Created   4-OCT-1992   Drew Baden
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      include 'D0$XFRAME$SOURCE:d0map.inc'
c
      character*4 bank
      integer maxcrate,ncrates,icrates(*),ichans(*)
c
      character*80 msg
      integer lbank,length_bank,end_bank,end_last_crate,end_this_crate
      integer this_crate_id,length_this_crate,begin_this_crate
      integer header_pointer,header_length,data_pointer,data_length
      INTEGER   DATA_CABLE_TRAILER_LENGTH
      PARAMETER ( DATA_CABLE_TRAILER_LENGTH = 16 )
      INTEGER   CRATE_ID_OFFSET, TOTAL_WORD_OFFSET
      PARAMETER ( CRATE_ID_OFFSET = - 2 )
      PARAMETER ( TOTAL_WORD_OFFSET = - 3 )
      INTEGER   LOWER_16_BITS
      PARAMETER ( LOWER_16_BITS = 255 * 256 + 255 ) !  Hex(FFFF)
c
      call blocat(dbdiv(0),bank,lbank)
      if (lbank.lt.1) then
        msg = 'Status: Bank not found'//char(0)
        call rawstat(%ref(msg))
        return
      endif
c
      length_bank = iq( lbank - 1 )
      end_bank = lbank + length_bank
      end_last_crate = end_bank - data_cable_trailer_length
      end_this_crate = end_last_crate
      ncrates = 0
      do while (end_this_crate.gt.lbank)
        this_crate_id = iq( end_this_crate + crate_id_offset )
        this_crate_id = iand( this_crate_id, lower_16_bits )
        length_this_crate = iq( end_this_crate + total_word_offset )
        begin_this_crate = end_this_crate - length_this_crate + 1
        ncrates = ncrates + 1
        icrates(ncrates) = this_crate_id
        header_pointer = begin_this_crate
        header_length = iq(header_pointer) + 1  !include length word
        data_pointer = header_pointer + header_length
        data_length = length_this_crate - header_length - 4
        ichans(ncrates) = data_length
        end_this_crate = begin_this_crate - 1
      enddo
c
      return
      end
