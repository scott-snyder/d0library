      subroutine redefine_smg_layout

c     Craig Young               3-AUG-87

c     This subroutine redefines the options for the command bar.

      include    'd0$inc:swing.inc'
      include    '($smgdef)'

      record /pd_choice_type/ sub_choices(8)

      pull_choices.number = 8
      pull_choices.choice(1) = 'Delete'
      pull_choices.code(1) = 110
      pull_choices.ptr(1) = 0
      pull_choices.choice(2) = 'Edit'
      pull_choices.code(2) = 120
      pull_choices.ptr(2) = 0
      pull_choices.choice(3) = 'Move'
      pull_choices.code(3) = 130
      pull_choices.ptr(3) = 0
      pull_choices.choice(4) = 'Options'
      pull_choices.code(4) = 140
      pull_choices.ptr(4) = %loc( sub_choices(4) )
      pull_choices.choice(5) = 'Print'
      pull_choices.code(5) = 150
      pull_choices.ptr(5) = 0
      pull_choices.choice(6) = 'Rename'
      pull_choices.code(6) = 160
      pull_choices.ptr(6) = 0
      pull_choices.choice(7) = 'Help'
      pull_choices.code(7) = 170
      pull_choices.ptr(7) = 0
      pull_choices.choice(8) = 'Quit'
      pull_choices.code(8) = 180
      pull_choices.ptr(8) = %loc( sub_choices(8) )

      sub_choices(4).number = 1
      sub_choices(4).choice(1) = 'DCL Command'
      sub_choices(4).code(1) = 141
      sub_choices(4).key(1) = 68
      sub_choices(8).number = 2
      sub_choices(8).choice(1) = 'Okay, quit filer'
      sub_choices(8).code(1) = 181
      sub_choices(8).key(1) = 79
      sub_choices(8).choice(2) = 'Cancel'
      sub_choices(8).code(2) = 182
      sub_choices(8).key(2) = 67

      call pd_load_bar( width, pull_choices )

      return
      end
