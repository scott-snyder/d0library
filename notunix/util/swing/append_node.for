      subroutine append_node( level, spec, search )

      include    'd0$inc:swing.inc'
      include    '($smgdef)'

      integer    level, len_node, free_node, istat
      character  spec*255, search*255

      node_num = free_node()

      if ( level .gt. lowest_level ) lowest_level = level
      if ( level .le. last_level ) then
         line = line + 1
         num_lines = line

         node(last_node(level)).sister = node_num
         else
         node(node_num-1).child = node_num
         end if

      if ( level .ne. 0 ) then
         call file_to_dir( spec,
     .                     node(node_num).spec,
     .                     node(node_num).length,
     .                     node(node_num).name )

      else
         call str$trim( spec, spec, len_node )
         node(node_num).spec = spec
         node(node_num).length = len_node
         if ( len_node .le. 10 ) then
            node(node_num).name = spec
            else
            node(node_num).name = spec(:11)//'*'
            end if
         end if

      node(node_num).line = line
      node(node_num).level = level
      node(node_num).rend = smg$m_reverse

      node_pointer(level,line) = node_num

      search = node(node_num).spec(1:node(node_num).length)//'*.dir;1'

      last_level = level
      last_node(level) = node_num

      return
      end
