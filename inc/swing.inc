*=======================================================================
*     COMMONS FOR SWING.FOR
*=======================================================================
     
      implicit  none
     
      include '/d0library/scratch/inc/pulldown.inc'
     
      parameter MAX_LINES = 495  
      parameter MAX_LEVELS = 7
      parameter MAX_NODES = 3500
      parameter MAX_FILES = 500
     
      integer*4 line
      integer   last_level, last_line(0:MAX_LEVELS)
      integer   last_node(0:MAX_LEVELS)
      integer   node_pointer( 0:MAX_LEVELS, MAX_LINES ), lowest_level
      integer   node_num, num_nodes, num_lines, file_num, num_files
     
      structure /node_type/
                character*255 spec
                character*12  name
                integer*4     length
                integer*4     level
                integer*4     line
                integer*4     rend
                integer*4     child
                integer*4     sister
      end structure
     
      record /node_type/ node(0:MAX_NODES)
     
      structure /fnode_type/
                character*255 spec
                character*25  name
                integer*4     length
                integer*4     rend
      end structure
     
      record /fnode_type/ fnode(MAX_FILES)
     
      common /node_info/ line,
     .                   last_level,
     .                   lowest_level,
     .                   last_line,
     .                   last_node,
     .                   num_lines,
     .                   node_pointer,
     .                   node,
     .                   node_num,
     .                   num_nodes,
     .                   fnode,
     .                   file_num,
     .                   num_files
     
      integer   window1, window2, window3, board_id, set_term_buf(3)
      integer   width, cur_level, cur_line, bottom_line, top_line
      integer   old_rend, len_disk, len_main, len_root, keyboard
      integer   file_window, top_file_line, bottom_file_line
      integer   DCL_window, proc_created
      integer*2 inbox_channel, outbox_channel
      logical   avo, using_screen, found, update, delete_problem
      logical   use_window1, do_save, swing_file_exists
      character this_terminal*31, disk*31, root*255, main*50
     
      record /pd_choice_type/ pull_choices
     
      common /smg_info/  board_id,
     .                   keyboard,
     .                   window1,
     .                   window2,
     .                   window3,
     .                   file_window,
     .                   DCL_window,
     .                   pull_choices,
     .                   this_terminal,
     .                   set_term_buf,
     .                   width,
     .                   avo,
     .                   old_rend,
     .                   disk,
     .                   len_disk,
     .                   root,
     .                   len_root,
     .                   main,
     .                   len_main,
     .                   cur_level,
     .                   cur_line,
     .                   top_line,
     .                   bottom_line,
     .                   top_file_line,
     .                   bottom_file_line,
     .                   using_screen,
     .                   use_window1,
     .                   update,
     .                   found,
     .                   delete_problem,
     .                   do_save,
     .                   swing_file_exists,
     .                   inbox_channel,
     .                   outbox_channel,
     .                   proc_created
