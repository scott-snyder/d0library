      subroutine define_smg_layout

      include    'd0$inc:swing.inc'
      include    '($smgdef)'

      record /pd_choice_type/ sub_choices(9)

      pull_choices.number = 9
      pull_choices.choice(1) = 'Create'
      pull_choices.code(1) = 10
      pull_choices.ptr(1) = 0
      pull_choices.choice(2) = 'Rename'
      pull_choices.code(2) = 20
      pull_choices.ptr(2) = 0
      pull_choices.choice(3) = 'Move'
      pull_choices.code(3) = 30
      pull_choices.ptr(3) = 0
      pull_choices.choice(4) = 'Delete'
      pull_choices.code(4) = 40
      pull_choices.ptr(4) = 0
      pull_choices.choice(5) = 'Print'
      pull_choices.code(5) = 50
      pull_choices.ptr(5) = 0
      pull_choices.choice(6) = 'Save'
      pull_choices.code(6) = 60
      pull_choices.ptr(6) = 0
      pull_choices.choice(7) = 'Options'
      pull_choices.code(7) = 70
      pull_choices.ptr(7) = %loc( sub_choices(7) )
      pull_choices.choice(8) = 'Help'
      pull_choices.code(8) = 80
      pull_choices.ptr(8) = 0
      pull_choices.choice(9) = 'Exit'
      pull_choices.code(9) = 90
      pull_choices.ptr(9) = %loc( sub_choices(9) )

      sub_choices(1).number = 0
      sub_choices(2).number = 0
      sub_choices(3).number = 0
      sub_choices(4).number = 0

      sub_choices(5).number = 0

      sub_choices(6).number = 0

      sub_choices(7).number = 3
      sub_choices(7).choice(1) = 'DCL Command'
      sub_choices(7).code(1) = 71
      sub_choices(7).key(1) = 68
      sub_choices(7).choice(2) = 'Filer'
      sub_choices(7).code(2) = 73
      sub_choices(7).key(2) = 70
      sub_choices(7).choice(3) = 'Show Directory Name'
      sub_choices(7).code(3) = 72
      sub_choices(7).key(3) = 83

      sub_choices(8).number = 0

      sub_choices(9).number = 2
      sub_choices(9).choice(1) = 'ok exit'
      sub_choices(9).code(1) = 91
      sub_choices(9).key(1) = 79
      sub_choices(9).choice(2) = 'cancel'
      sub_choices(9).code(2) = 92
      sub_choices(9).key(2) = 67

      call pd_load_bar( width, pull_choices)

      use_window1 = .false.

      return
      end
