      subroutine file_options ( code )

c     Craig Young               3-AUG-87

c     This subroutine is for calling extensions to the filer.

      include    'd0$inc:swing.inc'
      include    '($smgdef)'

      character  choice*(pd_max_choice_len)
      integer    code, ii
      logical    do_bar

      if ( code .eq. 0 ) then                   !Use pulldown menu
	 ii = 4
         call pd_list_choice( board_id, keyboard, width, ii,
     .                        %val(pull_choices.ptr(4)),
     .                        choice, .false., code, do_bar )
      end if

      if ( code .eq. 141 )
     .   call execute_dcl

      return
      end
