      SUBROUTINE pd_load_bar( width, pd_choices )

*     PD_LOAD_BAR( WIDTH, PD_CHOICES )
*
*     WIDTH          INTEGER*4
*     PD_CHOICES     RECORD /PD_CHOICE_TYPE/  (PULLDOWN.INC)
*
      IMPLICIT NONE
      INCLUDE '($smgdef)'
      INCLUDE 'd0$inc:pulldown.inc'

      INTEGER smg$create_virtual_display, smg$change_virtual_display
      INTEGER max_cell, ii, jj, kk, lens(PD_MAX_CHOICES)
      INTEGER start_pos, off_set, width, istat
      RECORD /pd_choice_type/ pd_choices

C     FIND OUT HOW MANY CHOICES THERE ARE AND THE MAXIMUM LENGTH
      ii = 1
      max_cell = 0
      DO WHILE ( ii .LE. pd_choices.number )
        CALL str$trim( pd_choices.choice(ii), pd_choices.choice(ii),
     &                  lens(ii) )
        max_cell = max( max_cell, lens(ii) )
        ii = ii + 1
      END DO
      ii = ii - 1

C     CREATE THE VIRTUAL DISPLAY FOR THE BAR
      IF ( pd_bar_id .EQ. 0 ) THEN
        istat = smg$create_virtual_display( 1, width, pd_bar_id,,
     &                                       smg$m_reverse )
      ELSE
        CALL smg$erase_display( pd_bar_id )
        istat = smg$change_virtual_display( pd_bar_id, 1, width,
     &    ,smg$m_reverse,)
C? ****  Arguments have changed.
C?     &                                       pd_bar_id,, smg$m_reverse )
      END IF

C     FIGURE OUT THE LENGTH OF EACH CELL

C     IF THERE IS ROOM ENOUGH FOR ALL OF THE CHOICES AS IS
      IF ( (ii*max_cell) .le. width ) then
        pd_cell_size = min( 16, width / ii )

C        MAKE IT 16 OR LESS
      ELSE
        pd_cell_size = min( 16, width / max_cell )
      END IF

C     PUT THE CHOICES IN THE MENU
      DO jj = 1, ii
        start_pos = 1 + (pd_cell_size*(jj-1))
        off_set = max( 1, pd_cell_size-lens(jj)) / 2
        CALL smg$put_chars( pd_bar_id,
     &                       pd_choices.choice(jj)(1:lens(jj)),,
     &                       start_pos + off_set )
      END DO

      pd_num_choices = ii

      RETURN
      END
