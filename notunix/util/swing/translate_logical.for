        function        translate_logical (logical_name,translation)

c       Craig Young             3-AUG-87

        implicit        none
        include         '($lnmdef)'
        include         '($ssdef)'


        integer*4       translate_logical
        character*(*)   translation
        character*(*)   logical_name

        integer*4       sys$trnlnm
        integer*4       status
        integer*4       attribute
        integer*4       ret_buf_len
        integer*4       n

        structure /item_list/
          integer*2     item_len
          integer*2     item_code /lnm$_string/
          integer*4     buffer_addr
          integer*4     ret_buffer_addr /0/
          integer*4     end_items /0/
        end structure
        record /item_list/ items

        translation = ' '
        attribute = lnm$m_case_blind
        items.item_len = LEN(logical_name)
        items.buffer_addr = %loc(translation)
        call str$trim(logical_name,logical_name,n)

        status = sys$trnlnm ( attribute, 'LNM$PROCESS',
     -          logical_name(1:n),,items)
        if ( status .ne. ss$_normal ) then
          status = sys$trnlnm ( attribute, 'LNM$JOB',
     -          logical_name(1:n),,items)
          if ( status .ne. ss$_normal )
     -      status = sys$trnlnm ( attribute, 'LNM$SYSTEM',
     -          logical_name(1:n),,items)
        end if

        translate_logical = status
        return
        end
