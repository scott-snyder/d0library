*=======================================================================
*
*  Title:        PULLDOWN PACKAGE
*
*  Version:      1-001
*
*  Abstract:     This is a package of routines to implement a pulldown
*                menu system on a VT100 type terminal with SMG routines.
*                It is used by SWING
*
*  Environment:  VMS
*
*  Author:       Eric Andresen of General Research Corporation
*
*  Date:         24-SEP-1986
*
*-----------------------------------------------------------------------

      subroutine pd_get_choice( board_id, keyboard, width,
     .                          pd_choices, choice, code )

      implicit none
*     PD_GET_CHOICE( BOARD_ID, KEYBOARD, WIDTH, PD_CHOICES, CHOICE, CODE )
*
*     BOARD_ID       INTEGER*4
*     KEYBOARD       INTEGER*4
*     WIDTH          INTEGER*4
*     PD_CHOICES     RECORD /PD_CHOICE_TYPE/  (PULLDOWN.INC)
*     CHOICE         CHARACTER*(PD_MAX_CHOICE_LEN)
*     CODE           INTEGER*4
*
      include 'd0$inc:pulldown.inc'

      integer   num_choice, save_choice, code, keyboard, width
      integer   board_id
      logical   do_bar
      character choice*(PD_MAX_CHOICE_LEN)
      record /pd_choice_type/ pd_choices

      do_bar = .true.
      num_choice = 1

C     LOOP UNTIL A VALID EXIT OCCURS
      do while ( do_bar )

C        GET A CHOICE FROM THE BAR
         call pd_bar_choice( keyboard, num_choice, pd_choices )

         save_choice = 0
         do_bar = .false.

C        AS LONG AS THE USER IS CHOOSING LISTS FROM THE BAR
         do while ( save_choice .ne. num_choice .and.
     .              pd_choices.ptr(num_choice) .ne. 0 )
            save_choice = num_choice
            call pd_list_choice( board_id, keyboard, width, num_choice,
     .                           %val(pd_choices.ptr(num_choice)),
     .                           choice, .true., code, do_bar )
         end do

C        IF A CHOICE HAS BEEN MADE
         if ( .not. do_bar ) then

C           IF ITS ONLY A CHOICE FROM THE BAR BECAUSE THERE WAS NO
C           ASSOCIATED LIST
            if ( save_choice .eq. 0 .and. num_choice .ne. 0 ) then
               choice = pd_choices.choice(num_choice)
               code = pd_choices.code(num_choice)

C              IF NO CHOICE WAS MADE
               else if ( save_choice .eq.0 .and. num_choice .eq.0 ) then
               choice = ' '
               code = -1
               end if

C              OTHERWISE A CHOICE WAS MADE FROM THE CALL TO
C              pd_list_choice

            end if

      end do

      return
      end
