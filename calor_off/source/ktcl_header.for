c
c  ktcl_header.for
c
c
c  Pack and unpack routines to help with getting the header for the
c  ktcl bank up and running.
c
c     Creation    1-oct-1995     Gordon Watts
c     Updated     9-oct-1995     Some comments added
c                                Gordon Watts
c     Updated     3-Oct-1995     Incorrect bit positions for header words fixed
c                                Gordon Watts
c     Updated     7-nov-1995     Increased number of bits for min et to
c                                precluster (increase the range too).  Also
c                                increase resolution on delta et.
c                                Use third header word for both these guys.
c                                Gordon Watts
c     Updated    13-nov-1995     Fixed constant setting (11 in an array of 10)
c                                Gordon Watts
c     Updated    20-nov-1995     Increased bit field for the min precluster
c                                et.
c                                Gordon Watts
c     
c

      subroutine ktcl_header

      implicit none

      integer bit_position, i
      integer word_one, word_two, word_three

c
c  Define, for word one, the various fields...
c
c  version (4 bits), # preclusters (13), # neg cells removed (5),
c  et of neg cells (5)
c

      integer word_one_size
      parameter (word_one_size = 4)
      real word_one_value (word_one_size)
      real word_one_min(word_one_size)
      real word_one_max(word_one_size)
      integer word_one_prec(word_one_size)

      integer version, n_preclusters, n_neg_cells_removed
      real et_removed_neg_cells

c
c  Define, for word two, the various fields...
c
c  Min et for precluster (5 0-800MeV)
c  Search Area Size (4), input_type (4), precluster width (5 0-0.8),
c  eta/phi def(2), phy eta/detector eta (1), et ordered/eta ordered (1),
c  neg cell removal done (1), preclustering done (1)
c

      integer word_two_size
      parameter (word_two_size = 10)
      real word_two_value (word_two_size)
      real word_two_min(word_two_size)
      real word_two_max(word_two_size)
      integer word_two_prec(word_two_size)

      real preclu_min_et
      integer search_size, input_type
      real preclu_width
      integer eta_def, et_def
      logical physics_eta, eta_ordered, neg_cell_removal_done
      logical preclu_done

c
c  Define for word two, the various fields...
c
c    the et below which we should precluster.
c
c    percluster et (8 0-100 GeV), delta et due 
c    to prclustering (9 bits, 0-4 GeV).
c

      integer word_three_size
      parameter (word_three_size = 2)
      real word_three_value (word_three_size)
      real word_three_min(word_three_size)
      real word_three_max(word_three_size)
      integer word_three_prec(word_three_size)

      real precluster_et, preclu_delta_et
c
c  Setup
c

      data word_one_prec /4, 13, 5, 5/
      data word_one_min /0.0, 0.0, 0.0, 0.0/
      data word_one_max /15.0, 8191.0, 31.0, 64.0/

      data word_two_prec /8, 5, 4, 5, 1, 1, 1, 1, 1, 1/
      data word_two_min  /0.0, 0.0, 0.0, 0.0, 1.0, 1.0, 0.0,
     $     0.0, 0.0, 0.0/
      data word_two_max  /100.0, 15.0, 15.0, 0.8, 2.0, 2.0,
     $     1.0, 1.0, 1.0, 1.0/

      data word_three_prec /8, 9/
      data word_three_min /0.0, 0.0/
      data word_three_max /100.0, 5.0/

c
c  Never come in this entry point
c

      stop 'ktcl_header should never be called.'

c
c  Pack the #1 word in the header
c

      entry ktcl_header_pack_one (word_one,
     $     version, n_preclusters, n_neg_cells_removed,
     $     et_removed_neg_cells)

c
c  Set the stuff up
c

      word_one_value(1) = version
      word_one_value(2) = n_preclusters
      word_one_value(3) = n_neg_cells_removed
      word_one_value(4) = et_removed_neg_cells

c
c  Great -- now pack it in!
c

      word_one = 0
      bit_position = 1
      do i = 1, word_one_size
         call kt_pack_a_word (bit_position, word_one, word_one_value(i),
     $        word_one_prec(i), word_one_min(i), word_one_max(i))
         bit_position = bit_position + word_one_prec(i)
      enddo

      return

c
c  Unpack word one
c

      entry ktcl_header_unpack_one (word_one,
     $     version, n_preclusters, n_neg_cells_removed,
     $     et_removed_neg_cells)

c
c  Great -- now unpack it!
c

      bit_position = 1
      do i = 1, word_one_size
         call kt_unpack_a_word (bit_position, word_one,
     $        word_one_value(i),
     $        word_one_prec(i), word_one_min(i), word_one_max(i))
         bit_position = bit_position + word_one_prec(i)
      enddo

c
c  Set the stuff up
c

      version = word_one_value(1)
      n_preclusters = word_one_value(2)
      n_neg_cells_removed = word_one_value(3)
      et_removed_neg_cells = word_one_value(4)

      return

c
c  Header word 2 -- packing...
c

      entry ktcl_header_pack_two (word_two,
     $     preclu_min_et, search_size,
     $     input_type, preclu_width, et_def, eta_def, physics_eta,
     $     eta_ordered, neg_cell_removal_done, preclu_done)

c
c  Setup the words up
c

      word_two_value(1) = preclu_min_et
      word_two_value(2) = search_size
      word_two_value(3) = input_type
      word_two_value(4) = preclu_width
      word_two_value(5) = et_def
      word_two_value(6) = eta_def

      word_two_value(7) = 0.0
      if (physics_eta) word_two_value(7) = 1.0

      word_two_value(8) = 0.0
      if (eta_ordered) word_two_value(8) = 1.0

      word_two_value(9) = 0.0
      if (neg_cell_removal_done) word_two_value(9) = 1.0

      word_two_value(10) = 0.0
      if (preclu_done) word_two_value(10) = 1.0

c
c  Great, no pack it in.
c

      word_two = 0
      bit_position = 1
      do i = 1, word_two_size
         call kt_pack_a_word (bit_position, word_two, word_two_value(i),
     $        word_two_prec(i), word_two_min(i), word_two_max(i))
         bit_position = bit_position + word_two_prec(i)
      enddo

      return

c
c  Unpack the damm thing
c

      entry ktcl_header_unpack_two (word_two,
     $     preclu_min_et, search_size,
     $     input_type, preclu_width, et_def, eta_def, physics_eta,
     $     eta_ordered, neg_cell_removal_done, preclu_done)
      
c
c  First, unpack it...
c

      bit_position = 1
      do i = 1, word_two_size
         call kt_unpack_a_word (bit_position, word_two,
     $        word_two_value(i),
     $        word_two_prec(i), word_two_min(i), word_two_max(i))
         bit_position = bit_position + word_two_prec(i)
      enddo

c
c  Now, restore the values to the caller
c

      preclu_min_et = word_two_value(1)
      search_size = word_two_value(2)
      input_type = word_two_value(3)
      preclu_width = word_two_value(4)
      et_def = word_two_value(5)
      eta_def = word_two_value(6)

      physics_eta = .false.
      if (word_two_value(7) .eq. 1.0) physics_eta = .true.

      eta_ordered = .false.
      if (word_two_value(8) .eq. 1.0) eta_ordered = .true.

      neg_cell_removal_done = .false.
      if (word_two_value(9) .eq. 1.0)
     $     neg_cell_removal_done = .true.

      preclu_done = .false.
      if (word_two_value(10) .eq. 1.0)
     $     preclu_done = .true.

      return

c
c  Pack word three
c


      entry ktcl_header_pack_three (word_three, precluster_et,
     $     preclu_delta_et)

c
c  Set the stuff up
c

      word_three_value(1) = precluster_et
      word_three_value(2) = preclu_delta_et

c
c  Great -- now pack it in!
c

      word_three = 0
      bit_position = 1
      do i = 1, word_three_size
         call kt_pack_a_word (bit_position, word_three,
     $        word_three_value(i),
     $        word_three_prec(i), word_three_min(i), word_three_max(i))
         bit_position = bit_position + word_three_prec(i)
      enddo

      return

c
c  Unpack word three
c

      entry ktcl_header_unpack_three (word_three,
     $     precluster_et, preclu_delta_et)

c
c  Great -- now unpack it!
c

      bit_position = 1
      do i = 1, word_three_size
         call kt_unpack_a_word (bit_position, word_three,
     $        word_three_value(i),
     $        word_three_prec(i), word_three_min(i), word_three_max(i))
         bit_position = bit_position + word_three_prec(i)
      enddo

c
c  Set the stuff up
c

      precluster_et = word_three_value(1)
      preclu_delta_et = word_three_value(2)

      return

      end


c****************
c  Utility routines...
c***************
c
c ktcl_get_input_type
c
c  Utility routine to return the input type used to build the ktcl bank
c  pointed to by lktcl.  Utility routine to get around the extra work
c  involved in unpacking the bit fields.
c

      integer function ktcl_get_input_type (lktcl)

      implicit none

      include 'd0$inc:zebcom.inc'

      integer lktcl

      real preclu_min_et
      integer search_size, input_type
      real preclu_width
      integer eta_def, et_def
      logical physics_eta, eta_ordered, neg_cell_removal_done
      logical preclu_done


      call ktcl_header_unpack_two (IQ(LKTCL+2),
     $     preclu_min_et, search_size,
     $     input_type, preclu_width, et_def, eta_def, physics_eta,
     $     eta_ordered, neg_cell_removal_done, preclu_done)

      ktcl_get_input_type = input_type

      return
      end


c***************************
c
c ktcl_get_ncluster
c
c  Return the number of preclusters stored in the ktcl bank pointed to
c  by lktcl.  This is a utility routine to save work caused by the 
c  dense bit-packing.
c

      integer function ktcl_get_ncluster (lktcl)

      implicit none

      include 'd0$inc:zebcom.inc'

      integer lktcl

      integer version, n_preclusters, n_neg_cells_removed
      real et_removed_neg_cells

      call ktcl_header_unpack_one (IQ(LKTCL+1),
     $     version, n_preclusters, n_neg_cells_removed,
     $     et_removed_neg_cells)

      ktcl_get_ncluster = n_preclusters

      return
      end

c**************
c kt_pack_a_word
c
c  Pack a single word... Be nice to rounding too...  Start packing
c  at bit_position (starts at zero), with a width of bit_length.  Min
c  and max are given for scaling.
c

         subroutine kt_pack_a_word (bit_position, word,
     $        value, bit_length, min_value, max_value)

         implicit none

         integer bit_position
         integer word
         real    value
         integer bit_length
         real    min_value
         real    max_value

         real    packed_value 
         integer bit_value
         real    rat

c
c  Ok -- first job is to move the value into its min and max.
c

         packed_value = value
         if (packed_value .gt. max_value) packed_value = max_value
         if (packed_value .lt. min_value) packed_value = min_value

c
c  Next, scale it -- figure out what the integer thing is going to
c  be.
c

         rat = (((2**bit_length)-1)
     $        * (packed_value - min_value)
     $        / (max_value - min_value))

         bit_value = NINT(rat)

c
c  Now, pack it
c

         call sbyt (bit_value, word, bit_position, bit_length)

c
c  done!
c

         return
         end

c
c  Pack a single word... Be nice to rounding too...  Start packing
c  at bit_position (starts at zero), with a width of bit_length.  Min
c  and max are given for scaling.
c

         subroutine kt_unpack_a_word (bit_position, word,
     $        value, bit_length, min_value, max_value)

         implicit none

         integer bit_position
         integer word
         real    value
         integer bit_length
         real    min_value
         real    max_value

         real    packed_value 
         integer bit_value
         real    step

         integer jbyt
c
c  First -- extract the bits
c

         bit_value = jbyt (word, bit_position, bit_length)

c
c  now, calculate the size per bit
c

         step = (max_value - min_value) / float (2**bit_length-1)

c
c  And, now the real value unpacked..
c

         value = min_value + step * bit_value

c
c  done!
c

         return
         end
