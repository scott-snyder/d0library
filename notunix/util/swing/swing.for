*=======================================================================
*
*  Title:        SWING
*
*  Version:      1-001
*
*  Abstract:     SWING is a VMS utility for displaying and manipulating
*                VMS directory trees.
*
*  Environment:  VMS
*
*  Author:       Eric Andresen of General Research Corporation
*
*  Date:         24-SEP-1986
*
*-----------------------------------------------------------------------
*
*  Modified and
*  Expanded by:  Craig Young of Hughes Aircraft Company
*
*  Additions:    The main addition was the FILER and all the subroutines
*                which support it.  The DCL Command option was added to
*                the SWING command menu.  Changes were made to subroutine
*                Load_Nodes to support '<' and '>' as directory indica-
*                tors, to allow the Master file directory as the root
*                directory and to allow the START qualifier.
*
*  Date:         3-AUG-1987
*
*-----------------------------------------------------------------------
*
*  Modified by:	 Frank J Nagy of Fermilab
*
*  Changes:	 Modified main routine to pick up command line as foreign
*		 command, add the SWING verb and parse it with DCL so that
*		 the /START qualifier can be checked for.
*
*  Date:         16-AUG-1987
*
*-----------------------------------------------------------------------
*
*  Modified by:  Lin A Winterowd of Fermilab
*
*  Changes:      Modified  DRAW_SCREEN  to correct initial screen layout
*                positioning error; and FIND_NODE to  limit  search  for
*                directory  created  outside  of  SWING  and  not  SAVED
*                (causing arithmetic trap on DELETE operation).

      program swing

      include 'd0$inc:swing.inc'
      include '($smgdef)'

      integer linest
      common /length/ linest

      integer    ii, jj, istat
      integer    ikey, old_level, old_line, isave, code, code_type
      integer    smg$create_virtual_display
      logical    crt, finished
      character  key, choice*(PD_MAX_CHOICE_LEN)

	External  Swing_Tables
	Integer*4 Lib$Get_Foreign, Cli$Dcl_Parse, Lib$Get_Input, sts
	Character*255 CmdLine
	Integer*2 CL_Len

C
C Get the Foreign Command line, tack the verb onto the front and
C invoke the DCL command processor on the result.
C
	sts = Lib$Get_Foreign( CmdLine,, CL_Len)
	IF (.NOT. sts) CALL Lib$Signal( %VAL(sts))
	IF (CL_Len .gt. 0) Then
	    sts = Cli$Dcl_Parse( 'SWING '//CmdLine(1:CL_Len),
	1			Swing_Tables, Lib$Get_Input)
	Else
	    sts = Cli$Dcl_Parse( 'SWING ', Swing_Tables, Lib$Get_Input)
	EndIf
	IF (.NOT. sts) CALL Exit

      if ( .not. crt() )
     .   call print_message( 'You must use a DEC CRT terminal', 1 )

      call define_paste_board

c     CREATE THE WINDOWS

      istat = smg$create_virtual_display(  1, 132, window1 )
      istat = smg$create_virtual_display(  MAX_LINES, 132, window2 )
      istat = smg$create_virtual_display(  2, 132, window3 )
      istat = smg$create_virtual_display(  12, 25, file_window )
      call smg$set_display_scroll_region(  file_window )
      istat = smg$create_virtual_display(  15, 70, DCL_window )
      call smg$set_display_scroll_region(  DCL_window )

      call load_nodes
      call define_smg_layout
      call load_display
      call draw_screen

      proc_created = 0

      do while ( .not. finished )

         call smg$read_keystroke( keyboard, ikey )

         call print_message( ' ', 0 )

         old_line = cur_line
         old_level = cur_level
         old_rend = node(node_num).rend

         code_type = 0
         code = 0

         if ( ikey .eq. smg$k_trm_do .or.
     .        ikey .eq. smg$k_trm_ctrlp ) then
            if ( avo ) then
               call pd_get_choice( board_id, keyboard, width,
     .                             pull_choices, choice, code )
               code_type = code / 10
            else
               call print_message( 'Advanced video option required', 0 )
               end if
            end if

         if ( ikey .eq. smg$k_trm_ctrlz .or.
     .        ikey .eq. smg$k_trm_f10         .or.
     .        ikey .eq. smg$k_trm_lowercase_x .or.
     .        ikey .eq. smg$k_trm_uppercase_x .or.
     .        ikey .eq. smg$k_trm_lowercase_e .or.
     .        ikey .eq. smg$k_trm_uppercase_e .or.
     .        ikey .eq. smg$k_trm_lowercase_q .or.
     .        ikey .eq. smg$k_trm_uppercase_q .or.
     .        ikey .eq. smg$k_trm_enter       .or.
     .        ikey .eq. smg$k_trm_cr          .or.
     .        code .eq. 91 ) then
            finished = .true.

            else if ( ikey .eq. smg$k_trm_up ) then
            ii = cur_level
            jj = cur_line - 1
            do while( jj .ge. 1 .and. node_pointer(ii,jj) .eq. 0 )
               jj = jj - 1
            end do
            if ( jj .ge. 1 ) cur_line = jj
            call update_screen( old_line, old_level )

            else if ( ikey .eq. smg$k_trm_down ) then
            ii = cur_level
            jj = cur_line + 1
            do while( node_pointer(ii,jj) .eq. 0 .and.jj .le. num_lines)
               jj = jj + 1
            end do
            if ( jj .le. num_lines ) cur_line = jj
            call update_screen( old_line, old_level )

            else if ( ikey .eq. smg$k_trm_right ) then
            ii = cur_level + 1
            jj = cur_line
            do while( node_pointer(ii,jj) .eq. 0 .and.ii.le. MAX_LEVELS)
               ii = ii + 1
            end do
            if ( ii .le. MAX_LEVELS ) cur_level = ii
            call update_screen( old_line, old_level )

            else if ( ikey .eq. smg$k_trm_left .and.
     .                cur_level .ge. 1 ) then
            ii = cur_level - 1
            jj = cur_line
            do while( node_pointer(ii,jj) .eq. 0 .and. jj .ge. 1 )
               jj = jj - 1
            end do
            if ( jj .ge. 1 ) then
               cur_level = ii
               cur_line = jj
               end if
            call update_screen( old_line, old_level )

            else if ( ikey .eq. smg$k_trm_lowercase_b .or.
     .                ikey .eq. smg$k_trm_uppercase_b ) then
            ii = MAX_LEVELS
            cur_line = num_lines
            do while( node_pointer(ii,cur_line) .eq. 0 .and. ii .ge. 1 )
               ii = ii - 1
            end do
            cur_level = ii
            call update_screen( old_line, old_level )

            else if ( ikey .eq. smg$k_trm_lowercase_t .or.
     .                ikey .eq. smg$k_trm_uppercase_t ) then
            cur_line = 1
            cur_level = 0
            call update_screen( old_line, old_level )

            else if ( code_type .eq. 1 .or.
     .                ikey .eq. smg$k_trm_lowercase_c .or.
     .                ikey .eq. smg$k_trm_uppercase_c ) then
            call create_directory( code )

            else if ( code_type .eq. 2 .or.
     .                ikey .eq. smg$k_trm_lowercase_r .or.
     .                ikey .eq. smg$k_trm_uppercase_r ) then
            call rename_directory( 20 )

            else if ( code_type .eq. 3 .or.
     .                ikey .eq. smg$k_trm_lowercase_m .or.
     .                ikey .eq. smg$k_trm_uppercase_m ) then
            call rename_directory( 30 )

            else if ( code_type .eq. 4 .or.
     .                ikey .eq. smg$k_trm_lowercase_d .or.
     .                ikey .eq. smg$k_trm_uppercase_d ) then
            call delete_directory( code )

            else if ( code_type .eq. 5 .or.
     .                ikey .eq. smg$k_trm_lowercase_p .or.
     .                ikey .eq. smg$k_trm_uppercase_p ) then
            call hardcopy( code )

            else if ( code_type .eq. 6 .or.
     .                ikey .eq. smg$k_trm_lowercase_s .or.
     .                ikey .eq. smg$k_trm_uppercase_s ) then
            call record_structure( .true. )

            else if ( code_type .eq. 7 .or.
     .                ikey .eq. smg$k_trm_lowercase_o .or.
     .                ikey .eq. smg$k_trm_uppercase_o ) then
            call change_options( code )

            else if ( code_type .eq. 8 .or.
     .                ikey .eq. smg$k_trm_pf2 .or.
     .                ikey .eq. smg$k_trm_help .or.
     .                ikey .eq. smg$k_trm_lowercase_h .or.
     .                ikey .eq. smg$k_trm_uppercase_h ) then
            call help( code )
            end if

         call smg$set_cursor_abs( window2, cur_line, cur_level*17+1 )

      end do

      call exit_swing
      end
