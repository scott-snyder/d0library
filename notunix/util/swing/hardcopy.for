      SUBROUTINE hardcopy( code )

      INCLUDE    'd0$inc:swing.inc'
      INCLUDE    '($smgdef)'

      INTEGER    column, num, ii, jj, level, ikey, start, end, len
      INTEGER    code, ll, kk
      CHARACTER  hard_node*12, dashes*12, out_line(MAX_LINES)*132
      CHARACTER  one_line*200

      DATA dashes / '------------' /

      OPEN( unit=1, name='swing.lis', carriagecontrol='list', 
     &  status='new', err=99 )

      CALL print_message( 'Creating hardcopy listing in SWING.LIS', 0 )

      last_level = 1
      line = 0
      DO ii = 0, MAX_LEVELS
        last_line(ii) = 1
      END DO
      DO ii = 1, num_lines
        out_line(ii) = ' '
      END DO

      DO jj = 1, num_lines
        DO level = 0, MAX_LEVELS
          IF ( node_pointer(level,jj) .ne. 0 ) then
            num = node_pointer(level,jj)

            column = level * 17 + 1
            line = node(num).line

            CALL str$trim( hard_node, node(num).name, len )
            IF ( level .LT. 7 ) THEN
              IF (( node_pointer(level+1,jj) .ne. 0 ) .and. 
     &          (len .lt. 12))
     &               hard_node = hard_node(1:len)//dashes(len+1:12)
            END IF

            out_line(line)(column:column+11) = hard_node
            IF ( level .GT. 0 ) THEN
              out_line(line)(column-3:column-1) = '---'

              IF ( level .LE. last_level ) THEN
                out_line(line)(column-3:column-3) = '+'
                IF ( out_line(line-1)(column-3:column-3) .eq. '+' )
     &                  out_line(line-1)(column-3:column-3) = '|'

              ELSE IF ( level .EQ. last_level + 1 ) THEN
                out_line(line)(column-5:column-2) = '----'
              END IF

              IF ( level .LT. last_level ) THEN
                IF ( out_line(last_line(level))(column-3:column-3)
     &                    .EQ. '+' ) THEN
                  ll = last_line(level)
                ELSE
                  ll = last_line(level) + 1
                END IF
                DO kk = ll, line-1
                  out_line(kk)(column-3:column-3) = '|'
                END DO
              END IF
            END IF

            last_level = level
            last_line(level) = line
          END IF
        END DO
      END DO

      DO ii = 1, num_lines
        CALL str$trim( out_line(ii), out_line(ii), len )
        WRITE( 1, 100 ) out_line(ii)(1:len)
  100   FORMAT( a )
      END DO

      CALL print_message( 'Finished creating SWING.LIS',0 )

      CLOSE( unit=1 )

      RETURN

   99 CALL print_message( 'Unable to open file for hardcopy', 0 )
      RETURN
      END
