C----------------------------------------------------------------------
C
C                   DDDDDDDD           0000   0
C                   D       D         0    0 0
C                   D        D       0      0
C                   D         D     0      0 0
C                   D         D     0     0  0
C                   D         D     0    0   0
C                   D         D     0   0    0
C                   D         D     0  0     0
C                   D        D       00     0
C                   D       D        00    0
C                   DDDDDDDD        0  0000
C
C
C----------------------------------------------------------------------
C
      PROGRAM D0sources
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Print with index and page numbering specified
C-              files. Tuned for FORTRAN and Talaris printers
C-
C-
C-   Original  has been developped at CERN for ALEPH by Christian Arnault.
C-   Updated   6-MAY-1988   Olivier Callot
C-   Updated  12-AUG-1988   Olivier Callot  Less machine dependant, Internal
C-                                          processing of the options
C-   Updated   5-DEC-1988   Kaushik De      Add choice of Talaris setup
C-
C----------------------------------------------------------------------
      IMPLICIT INTEGER (a-z)

      EXTERNAL sources_commands
      CHARACTER*80 file_name, str
      CHARACTER*3 exten

      PARAMETER (max_input = 1000)
      DIMENSION name_len(max_input)
      CHARACTER*80 input_list(max_input), per_page_string
      LOGICAL*1 wild_flag

      COMMON /com1/  per_page, per_line, the_unit, read_unit

      CHARACTER*23 time
      COMMON /comdat/ time

      CHARACTER*40  Source_file

      COMMON /errcom/ ierr

      INTEGER exit_block(4)

      EXTERNAL sources_exit, lib$get_input, cli$_present, cli$_absent
      INTEGER  cli$present
      CHARACTER*255 command_line
      CHARACTER*80 the_command, queue_name
      INTEGER      lon_command, queue_lon
C + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + +
      LOGICAL default_banner_page
      INTEGER qms_font_number, qms_font_lpi, qms_font_cpi
      CHARACTER*4 qms_font_orientation
      COMMON /local_options/ default_banner_page, qms_font_number,
     &                       qms_font_lpi, qms_font_cpi,
     &                       qms_font_orientation
C=======================================================================

      CALL enable_ctrlc

      exit_block(1) = 0
      exit_block(2) = %loc(sources_exit)
      exit_block(3) = 0
      exit_block(4) = %loc(status)

      status = sys$dclexh (exit_block)
      IF (.NOT. status) CALL exit(status)

      status = sys$asctim (,time,,)
C + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + +
C   Read the command line. Parse it
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      status = lib$get_foreign( command_line,,comm_lon )
      status = cli$dcl_parse( 'SOURCES '//command_line(1:comm_lon),
     &                        sources_commands, lib$get_input )
C + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + +
C   Default values depends on the qualifier
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      IF ( cli$present( 'QMS' ) .EQ. %loc(cli$_present) ) THEN
        per_page            = 80
        per_line            = 100
        default_banner_page = .true.
        qms_font_number     = 10
        qms_font_lpi        = 8
        qms_font_cpi        = 14
        qms_font_orientation = '^IOP'
      ELSEIF ( cli$present( 'TALARIS' ) .EQ. %loc(cli$_present) ) THEN
        per_page            = 80
        per_line            = 100
        default_banner_page = .true.
        qms_font_number     = 1217
        qms_font_lpi        = 8
        qms_font_cpi        = 14
        qms_font_orientation = '^IOP'
      ELSE
        per_page            = 64
        per_line            = 132
        default_banner_page = .false.
      ENDIF


      IF ( cli$present('setup') ) THEN
        status = cli$get_value('setup',queue_name, queue_lon)
c
c   font handling for FNAL like setup
c
        IF (queue_name(1:queue_lon) .EQ. 'S610P') THEN
          per_page            = 60
          per_line            = 70
          qms_font_number     = 1200
          qms_font_lpi        = 6
          qms_font_cpi        = 10
          qms_font_orientation = '^IOP'
        ELSEIF (queue_name(1:queue_lon) .EQ. 'R610P') THEN
          per_page            = 60
          per_line            = 70
          qms_font_number     = 1100
          qms_font_lpi        = 6
          qms_font_cpi        = 10
          qms_font_orientation = '^IOP'
        ELSEIF (queue_name(1:queue_lon) .EQ. 'T610P') THEN
          per_page            = 60
          per_line            = 70
          qms_font_number     = 1000
          qms_font_lpi        = 6
          qms_font_cpi        = 10
          qms_font_orientation = '^IOP'
        ELSEIF (queue_name(1:queue_lon) .EQ. 'S610L') THEN
          per_page            = 45
          per_line            = 95
          qms_font_number     = 1200
          qms_font_lpi        = 6
          qms_font_cpi        = 10
          qms_font_orientation = '^IOL'
        ELSEIF (queue_name(1:queue_lon) .EQ. 'R610L') THEN
          per_page            = 45
          per_line            = 95
          qms_font_number     = 1100
          qms_font_lpi        = 6
          qms_font_cpi        = 10
          qms_font_orientation = '^IOL'
        ELSEIF (queue_name(1:queue_lon) .EQ. 'T610L') THEN
          per_page            = 45
          per_line            = 95
          qms_font_number     = 1000
          qms_font_lpi        = 6
          qms_font_cpi        = 10
          qms_font_orientation = '^IOL'
c
        ELSEIF (queue_name(1:queue_lon) .EQ. 'S612P') THEN
          per_page            = 60
          per_line            = 84
          qms_font_number     = 1204
          qms_font_lpi        = 6
          qms_font_cpi        = 12
          qms_font_orientation = '^IOP'
        ELSEIF (queue_name(1:queue_lon) .EQ. 'R612P') THEN
          per_page            = 60
          per_line            = 84
          qms_font_number     = 1103
          qms_font_lpi        = 6
          qms_font_cpi        = 12
          qms_font_orientation = '^IOP'
        ELSEIF (queue_name(1:queue_lon) .EQ. 'T612P') THEN
          per_page            = 60
          per_line            = 84
          qms_font_number     = 1003
          qms_font_lpi        = 6
          qms_font_cpi        = 12
          qms_font_orientation = '^IOP'
        ELSEIF (queue_name(1:queue_lon) .EQ. 'S612L') THEN
          per_page            = 45
          per_line            = 114
          qms_font_number     = 1204
          qms_font_lpi        = 6
          qms_font_cpi        = 12
          qms_font_orientation = '^IOL'
        ELSEIF (queue_name(1:queue_lon) .EQ. 'R612L') THEN
          per_page            = 45
          per_line            = 114
          qms_font_number     = 1103
          qms_font_lpi        = 6
          qms_font_cpi        = 12
          qms_font_orientation = '^IOL'
        ELSEIF (queue_name(1:queue_lon) .EQ. 'T612L') THEN
          per_page            = 45
          per_line            = 114
          qms_font_number     = 1003
          qms_font_lpi        = 6
          qms_font_cpi        = 12
          qms_font_orientation = '^IOL'
c
        ELSEIF (queue_name(1:queue_lon) .EQ. 'S812P') THEN
          per_page            = 80
          per_line            = 84
          qms_font_number     = 1212
          qms_font_lpi        = 8
          qms_font_cpi        = 12
          qms_font_orientation = '^IOP'
        ELSEIF (queue_name(1:queue_lon) .EQ. 'R812P') THEN
          per_page            = 80
          per_line            = 84
          qms_font_number     = 1109
          qms_font_lpi        = 8
          qms_font_cpi        = 12
          qms_font_orientation = '^IOP'
        ELSEIF (queue_name(1:queue_lon) .EQ. 'T812P') THEN
          per_page            = 80
          per_line            = 84
          qms_font_number     = 1009
          qms_font_lpi        = 8
          qms_font_cpi        = 12
          qms_font_orientation = '^IOP'
        ELSEIF (queue_name(1:queue_lon) .EQ. 'S812L') THEN
          per_page            = 60
          per_line            = 114
          qms_font_number     = 1212
          qms_font_lpi        = 8
          qms_font_cpi        = 12
          qms_font_orientation = '^IOL'
        ELSEIF (queue_name(1:queue_lon) .EQ. 'R812L') THEN
          per_page            = 60
          per_line            = 114
          qms_font_number     = 1109
          qms_font_lpi        = 8
          qms_font_cpi        = 12
          qms_font_orientation = '^IOL'
        ELSEIF (queue_name(1:queue_lon) .EQ. 'T812L') THEN
          per_page            = 60
          per_line            = 114
          qms_font_number     = 1009
          qms_font_lpi        = 8
          qms_font_cpi        = 12
          qms_font_orientation = '^IOL'
c
        ELSEIF (queue_name(1:queue_lon) .EQ. 'S814P') THEN
          per_page            = 80
          per_line            = 98
          qms_font_number     = 1217
          qms_font_lpi        = 8
          qms_font_cpi        = 14
          qms_font_orientation = '^IOP'
        ELSEIF (queue_name(1:queue_lon) .EQ. 'R814P') THEN
          per_page            = 80
          per_line            = 98
          qms_font_number     = 1117
          qms_font_lpi        = 8
          qms_font_cpi        = 14
          qms_font_orientation = '^IOP'
        ELSEIF (queue_name(1:queue_lon) .EQ. 'S814L') THEN
          per_page            = 60
          per_line            = 133
          qms_font_number     = 1217
          qms_font_lpi        = 8
          qms_font_cpi        = 14
          qms_font_orientation = '^IOL'
        ELSEIF (queue_name(1:queue_lon) .EQ. 'R814L') THEN
          per_page            = 60
          per_line            = 133
          qms_font_number     = 1117
          qms_font_lpi        = 8
          qms_font_cpi        = 14
          qms_font_orientation = '^IOL'
        ENDIF
      ENDIF
      IF ( cli$present('font') ) THEN
        status = cli$get_value('font', queue_name, queue_lon)
        READ(queue_name(1:queue_lon),*) qms_font_number
      ENDIF

      IF ( cli$present('char_per_line') ) THEN
        status = cli$get_value('char_per_line', queue_name, queue_lon)
        READ(queue_name(1:queue_lon),*) per_line
      ENDIF
      IF ( cli$present('line_per_page') ) THEN
        status = cli$get_value('line_per_page',queue_name, queue_lon)
        READ(queue_name(1:queue_lon),*) per_page
      ENDIF
C
C  *** The count is for active lines. The page numbering gives 3 lines.
C
      per_page = per_page - 3

      the_unit  = 2
      read_unit = 3

      ifile = 0
      DO WHILE (cli$get_value('file_name',file_name,ilen))
        ifile = ifile + 1
        input_list (ifile) = file_name (1:ilen)
        name_len (ifile) = ilen
      ENDDO

      IF (ifile .LE. 0) GOTO 9000

c ................ opening spooled file

      ierr = 2
      source_file = 'SOURCES.LIS'
      OPEN ( unit=the_unit, TYPE='new', file=source_file, err= 10 )
      GOTO 20
   10 CONTINUE
C
C ****  Try on SYS$LOGIN device, where we should have write access...
C
      source_file = 'SYS$LOGIN:SOURCES.LIS'
      OPEN ( unit=the_unit, TYPE='new', file=source_file, err = 9000)
   20 CONTINUE
      CALL index_header( '1' )
      page = 1

c.............. building index

      DO icomm = 1,ifile
        file_name = input_list (icomm)
        ilen = name_len (icomm)
        CALL write_command( page, file_name(1:ilen) )
        DO WHILE (lib$find_file(file_name(1:ilen),str,context,,,,2))
          CALL str$trim (str, str, i)
C
C ****  suppress non_printable files from extension string
C
          jendir  = index( str(1:i), ']' )+1
          jbegext = index( str(jendir:i), '.' )+jendir
          jendext = index( str(1:i), ';' )-1
          exten = str( jbegext:jendext )
          IF( exten .EQ. 'DIR' ) GOTO 100
          IF( exten .EQ. 'EXE' ) GOTO 100
          IF( exten .EQ. 'OBJ' ) GOTO 100
          IF( exten .EQ. 'OLB' ) GOTO 100
C
C ****  Test if file can be read
C
          OPEN( unit=read_unit, TYPE='OLD', file=str(1:i),
     &          err=200, readonly)
          ierr = add_file (str(1:i))
          IF (ierr .NE. 0) GOTO 9000
  200     CLOSE( unit=read_unit )
  100     CONTINUE
        ENDDO
      ENDDO
C
C ****  WRITE the complete listing now
C
      CALL sources( source_file )

      CLOSE (unit=the_unit)
C + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + +
C   Possible non-printing of the file with /KEEP
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      istat = cli$present('keep')
      IF ( istat .EQ. %loc(cli$_present) ) GOTO 9000
C
      the_command = '$ PRINT/DELETE'
      lon_command = 14
C
C ****  If banner, then no FLAG page on output
C
      istat = cli$present( 'banner' )
      IF( default_banner_page ) THEN
        IF( istat .EQ. %loc(cli$_negated) ) go to 400
      ELSE
        IF( istat .EQ. %loc(cli$_absent)  ) GOTO 400
      ENDIF
      the_command = the_command(1:lon_command) // '/NOFLAG'
      lon_command = lon_command + 7
  400 CONTINUE
C
C ****  If FORM was requested, process it
C
      IF ( cli$present( 'FORM' ) ) THEN
        status = cli$get_value( 'FORM', queue_name, queue_lon )
        the_command = the_command(1:lon_command) // '/FORM=' //
     &                queue_name( 1:queue_lon )
        lon_command = lon_command + 6 + queue_lon
      ENDIF
C
C ****  If QUEUE was given, add it to the command
C
      IF ( cli$present('queue') ) THEN
        status = cli$get_value('queue',queue_name, queue_lon)
        the_command = the_command(1:lon_command) //
     &                '/QUEUE=' //
     &                queue_name(1:queue_lon)
        lon_command = lon_command + 7 + queue_lon
      ENDIF
      the_command = the_command(1:lon_command+1)// source_file
C
      TYPE *,'----- Now we PRINT it : '//the_command
      status = lib$do_command ( the_command )
 9000 CONTINUE
      END

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      INTEGER FUNCTION add_file (file_name)

      IMPLICIT INTEGER (a-z)
      PARAMETER (max_files=1000)

      COMMON /com1/  per_page, per_line, the_unit, read_unit

      CHARACTER*(*) end_marker
      CHARACTER form_feed, carriage_return

      PARAMETER (end_marker = '********   END of listing   ********')

      PARAMETER (form_feed = char(12))
      PARAMETER( carriage_return = char(13) )

      PARAMETER (header_size = 3)

      CHARACTER*80 file_list(max_files), the_file, old_dir, cur_dir
      DIMENSION name_len(max_files)
      CHARACTER*(*) file_name
      CHARACTER*256 buf
      DIMENSION npages(max_files)

      PARAMETER (max_modules = 1000)
      CHARACTER*80 module_name
      COMMON /com2/ first_page,
     &              module_name (max_modules),
     &              module_len  (max_modules),
     &              module_page (max_modules),
     &              n_modules

      DATA ifile /1/
      DATA n_file /1/
      DATA page /1/
      DATA old_dir /' '/

      add_file = 0
      ilen = len(file_name)

c...nombre total de records (il faut ajouter les 3 lignes du header).

      first_page = page
      irec = count_rec (file_name) + header_size
      if (irec .ge. 0) then

c...nombre de pages pour ce fichier.

        i = irec / per_page
        if (i * per_page .lt. irec) i = i+1
        npages(ifile) = i
C
C ****  extract directory, compare it
C
        i = index( file_name( 1:ilen ), ']' )
        jmin = 1
        IF ( i .NE. 0 ) THEN
          cur_dir = file_name( 1:i )
          IF( cur_dir .NE. old_dir ) THEN
            old_dir = cur_dir
            CALL write_dir( page, cur_dir(1:i) )
          ENDIF
          jmin = i+1
        ENDIF

        CALL write_entry (page,file_name(jmin:ilen))
        DO i = 1,n_modules
          j = module_len (i)
          CALL write_module (module_page(i),module_name(i)(1:j))
        ENDDO

c...nombre total de pages.

        page = page + npages(ifile)
        file_list(ifile) = file_name(1:ilen)
        name_len(ifile) = ilen
        ifile = ifile + 1
      ELSEIF (irec .EQ. -1) THEN
        add_file = 4
      ELSEIF (irec .EQ. -2) THEN
        add_file = 5
      ENDIF


      RETURN

      ENTRY sources( file_name )

      n_files = ifile - 1

      page = 1-page
      CALL write_entry (page,end_marker)

      IF (n_files .LT. 1) RETURN

      TYPE *,'----- Now creating the spooled file '//file_name

      DO ifile = 1, n_files
        ilen = name_len (ifile)
        the_file = file_list (ifile)(1:ilen)
        OPEN (unit = read_unit, TYPE = 'OLD', Readonly,
     &        file = the_file (1:ilen), err = 100)
        CALL write_name (the_file (1:ilen))
C
C ****  extract file name+ext
C
        ifir = index( the_file(1:ilen), ']' )+1
        i = index( the_file(ifir:ilen), ';' )
        ilas = ilen
        IF( i.ge.2 ) ilas = ifir+i-2

        first = header_size + 1
        DO ipage = 1, npages(ifile)
          DO iline = first,per_page
            READ (read_unit,2000,end=200) buflen,buf
            if( buflen .gt. 256 ) buflen = 256
            IF (buflen .EQ. 0) THEN
              buflen = 1
              buf(1:1) = ' '
            ENDIF
C
C ****  If CR+LF, suppress them
C
            i = index( buf(1:buflen), carriage_return)
            IF ( i .GT. 0 ) THEN
              buflen = i-1
              IF ( buflen .lt.1 ) THEN
                buf(1:1) = ' '
                buflen = 1
              ENDIF
            ENDIF
C
C ****  If Form_feed, just ignore it... Page numbering done by SOURCES itself
C
  140       i = index (buf(1:buflen), form_feed)
            IF (i .GT. 0) THEN
              buf (i:i) = '$'
              GOTO 140
            ENDIF
            WRITE (the_unit,2100) buf(1:buflen)
          ENDDO
          CALL write_page (page, the_file(ifir:ilas))
  150     CONTINUE
          first = 1
        ENDDO

        GOTO 100

  200   IF( ifile .EQ. n_files ) THEN
          page = -page                  ! tag to avoid last page empty
        ENDIF
        CALL fill_page (page, iline, the_file(ifir:ilas))

  100   CLOSE (unit=read_unit)

      ENDDO

 2000 FORMAT(q,a256)
 2100 FORMAT(1x,a)

      RETURN


      END

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      SUBROUTINE write_page (page, subtitle)

      IMPLICIT INTEGER (a-z)
      CHARACTER*(*) subtitle

      COMMON /com1/  per_page, per_line, the_unit, read_unit

      INTEGER rel_page
      CHARACTER*80 get_line

      ltit = len( subtitle )
      WRITE (the_unit, 1000)
      IF ( page .EQ. 0 ) THEN
        WRITE (the_unit, 200)
  200   FORMAT(<per_line-5>x,'Index')
      ELSE
        WRITE (the_unit,100),subtitle(1:ltit),abs(page)
  100   FORMAT (<per_line-11-ltit>x,a,'  Page ',I4)
      ENDIF
      IF( page .LT. 0 ) RETURN
      WRITE (the_unit, '(a)') '1'
      IF( page .NE. 0 ) page = page + 1
 1000 FORMAT(1x)
      RETURN
      END

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      SUBROUTINE write_name (name)

      IMPLICIT INTEGER (a-z)
      CHARACTER*(*) name

      CHARACTER*23 time
      COMMON /comdat/ time
      DATA time_len /17/

      COMMON /com1/  per_page, per_line, the_unit, read_unit


      ilen = len (name)
      CALL file_date (name(1:ilen), time)

      ilen2 = per_line - ilen - time_len - 2
      IF (ilen2 .LT. 1) THEN
        ilen2 = 1
        ilen = per_line - ilen2 - time_len - 2
      ENDIF

      WRITE (the_unit,300) time(1:time_len),name(1:ilen)
      WRITE (the_unit,1000)
      WRITE (the_unit,1000)

  300 FORMAT(1x,a,'>',<ilen2>('-'),a)
 1000 FORMAT(1x)

      RETURN
      END


ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      INTEGER FUNCTION count_rec (file_name)

      IMPLICIT INTEGER (a-z)


      PARAMETER (header_size = 3)

      CHARACTER*(*) file_name
      CHARACTER*256 buf

      COMMON /com1/  per_page, per_line, the_unit, read_unit

      PARAMETER (fortran = 1)
      PARAMETER (macro = 2)
      PARAMETER (pascal = 3)
      PARAMETER (vaxtpu = 4 )

      PARAMETER (max_modules = 1000)
      CHARACTER*80 module_name
      COMMON /com2/ first_page,
     &              module_name (max_modules),
     &              module_len  (max_modules),
     &              module_page (max_modules),
     &              n_modules

      LOGICAL get_fortran_module
      LOGICAL get_macro_module
      LOGICAL get_pascal_module
      LOGICAL get_vaxtpu_module

      n_modules = 0
      ilen = len (file_name)
      CALL str$upcase (buf,file_name(1:ilen))
      OPEN (unit=read_unit, TYPE='OLD', readonly,
     &   file=file_name(1:ilen), err=9999)

      IF (index (buf(1:ilen),'.F') .GT. 0) THEN
        language = fortran
      ELSEIF (index (buf(1:ilen),'.MAR') .GT. 0) THEN
        language = macro
      ELSEIF (index (buf(1:ilen),'.PAS') .GT. 0) THEN
        language = pascal
      ELSEIF (index (buf(1:ilen),'.TPU') .GT. 0) THEN
        language = vaxtpu
      ELSE
        language = 0
      ENDIF

      irec = 0
      iline = header_size + 1
      the_page = first_page

    1 READ (read_unit,200,end=1000,err=9998) buf_len,buf
  200 FORMAT (q,a)
      if( buf_len .gt. 256 ) buf_len = 256
      irec = irec+1

      IF (language .EQ. fortran) THEN
        n = n_modules + 1
        IF ( get_fortran_module(buf(1:buf_len),
     &     module_name(n), module_len(n)) ) THEN
          n_modules = n
          module_page (n) = the_page
        ENDIF
      ELSEIF (language .EQ. macro) THEN
        n = n_modules + 1
        IF ( get_macro_module(buf(1:buf_len),
     &     module_name(n), module_len(n)) ) THEN
          n_modules = n
          module_page (n) = the_page
        ENDIF
      ELSEIF (language .EQ. pascal) THEN
        n = n_modules + 1
        IF ( get_pascal_module(buf(1:buf_len), module_name(n),
     &       module_len(n)) ) THEN
          n_modules = n
          module_page (n) = the_page
        ENDIF
      ELSEIF (language .EQ. vaxtpu) THEN
        n = n_modules + 1
        IF ( get_vaxtpu_module(buf(1:buf_len), module_name(n),
     &       module_len(n)) ) THEN
          n_modules = n
          module_page (n) = the_page
        ENDIF
      ELSE
        GOTO 1
      ENDIF

      iline = iline + 1
      IF (iline .GT. per_page) THEN
        iline = 1
        the_page = the_page + 1
      ENDIF

      GOTO 1
 1000 count_rec = irec
      CLOSE (unit=read_unit)
      RETURN
 9998 count_rec=-1
      CLOSE (unit=read_unit)
      RETURN
 9999 count_rec=-2
      RETURN
      END

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      LOGICAL FUNCTION get_fortran_module (line,name,name_len)
      IMPLICIT INTEGER (a-z)
      CHARACTER*(*) line,name
      CHARACTER*256 up_line
      LOGICAL test_blank
      LOGICAL flag

      flag = .false.
      CALL str$upcase (up_line,line)
      i = index (up_line,'PROGRAM ')
      IF (i .GT. 0) GOTO 1
      i = index (up_line,'SUBROUTINE ')
      IF (i .GT. 0) GOTO 1
      i = index (up_line,'ENTRY ')
      IF (i .GT. 0) GOTO 1
      i = index (up_line,'FUNCTION ')
      IF (i .GT. 0) THEN
        flag = test_blank(line(1:i-1))
        IF (flag) GOTO 2
        j = i
        i = index (up_line,'INTEGER ')
        IF (i .GT. 0 .AND. i .LT. j) GOTO 1
        i = index (up_line,'REAL ')
        IF (i .GT. 0 .AND. i .LT. j) GOTO 1
        i = index (up_line,'BYTE ')
        IF (i .GT. 0 .AND. i .LT. j) GOTO 1
        i = index (up_line,'LOGICAL ')
        IF (i .GT. 0 .AND. i .LT. j) GOTO 1
        i = index (up_line,'CHARACTER')
        IF (i .GT. 0 .AND. i .LT. j) GOTO 1
        GOTO 1
      ENDIF
      GOTO 2
    1 flag = test_blank(line(1:i-1))
    2 get_fortran_module = flag
      IF (flag) THEN
        j = len(line)
        name = line(i:j)
        name_len = j-i+1
      ENDIF
      RETURN
      END
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      LOGICAL FUNCTION get_macro_module (line,name,name_len)
      IMPLICIT INTEGER (a-z)
      CHARACTER*(*) line,name
      CHARACTER*256 up_line
      LOGICAL test_blank
      LOGICAL flag

      flag = .false.
      CALL str$upcase (up_line,line)
      i = index (up_line,'.TITLE')
      IF (i .GT. 0) GOTO 1
      i = index (up_line,'.ENTRY')
      IF (i .GT. 0) GOTO 1
      GOTO 2
    1 flag = test_blank(line(1:i-1))
    2 get_macro_module = flag
      IF (flag) THEN
        j = len(line)
        name = line(i:j)
        name_len = j-i+1
      ENDIF
      RETURN
      END
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      LOGICAL FUNCTION get_pascal_module (line,name,name_len)
      IMPLICIT INTEGER (a-z)
      CHARACTER*(*) line,name
      CHARACTER*256 up_line
      LOGICAL test_blank
      LOGICAL flag

      flag = .false.
      CALL str$upcase (up_line,line)
      i = index (up_line,'PROGRAM ')
      IF (i .GT. 0) GOTO 1
      i = index (up_line,'PROCEDURE ')
      IF (i .GT. 0) GOTO 1
      i = index (up_line,'FUNCTION ')
      IF (i .GT. 0) GOTO 1
      GOTO 2
    1 flag = test_blank(line(1:i-1))
    2 get_pascal_module = flag
      IF (flag) THEN
        j = len(line)
        name = line(i:j)
        name_len = j-i+1
      ENDIF
      RETURN
      END
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      LOGICAL FUNCTION get_vaxtpu_module (line,name,name_len)
      IMPLICIT INTEGER (a-z)
      CHARACTER*(*) line,name
      CHARACTER*256 up_line
      LOGICAL test_blank
      LOGICAL flag

      flag = .false.
      CALL str$upcase (up_line,line)
      i = index (up_line,'PROCEDURE ')
      IF (i .LE. 0) GOTO 2
      IF (i .GT. 1) THEN
        flag = test_blank(line(1:i-1))
      ELSE
        flag = .true.
      ENDIF
    2 get_vaxtpu_module = flag
      IF (flag) THEN
        j = len(line)
        name = line(i:j)
        name_len = j-i+1
      ENDIF
      RETURN
      END
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      LOGICAL FUNCTION test_blank (line)
      CHARACTER*(*) line
      CHARACTER space,tab,ch
      PARAMETER (space=' ')
      PARAMETER (tab=char(9))

      test_blank = .false.
      line_len = len(line)
      DO i=1,line_len
        ch = line(i:i)
        IF (ch .NE. space .AND. ch .NE. tab) RETURN
      ENDDO
      test_blank = .true.
      RETURN
      END
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      SUBROUTINE index_header( page_char )
      IMPLICIT INTEGER (a-z)
      CHARACTER*1 page_char

      EXTERNAL lib$date_time, sys$trnlog,lib$find_file,
     &  lib$find_file_end
      EXTERNAL cli$_present, cli$_negated, cli$_absent
      INTEGER cli$present
      LOGICAL cli$get_value
C
      CHARACTER*1 trayspec, uname(30)
      CHARACTER*30 datestr, username, clus_name
      CHARACTER*100 comment
      CHARACTER*255 logical_name
      CHARACTER*15 prname
      CHARACTER*7  mach_type
      EQUIVALENCE (uname, username)
      INTEGER*2 name_len, name_code
      COMMON /item_list/ name_len, name_code, name_adr, ret_adr,
     &                   end_list
      COMMON /com1/  per_page, per_line, the_unit, read_unit
      LOGICAL default_banner_page
      INTEGER qms_font_number, qms_font_lpi, qms_font_cpi
      CHARACTER*4 qms_font_orientation
      COMMON /local_options/ default_banner_page, qms_font_number,
     &                       qms_font_lpi, qms_font_cpi,
     &                       qms_font_orientation
      DATA nb_call / 0 /
C=======================================================================
C
C ****  Nice banner on Talaris printer
C
      IF( nb_call .NE. 0 ) GOTO 200
      nb_call = 1
      istat = cli$present( 'banner' )
      IF( default_banner_page ) THEN
        IF( istat .EQ. %loc(cli$_negated) ) go to 200
      ELSE
        IF( istat .EQ. %loc(cli$_absent)  ) GOTO 200
      ENDIF
      istatus = lib$date_time(datestr)
C
C   get username string and locate '[' and ']' that bracket the name
C
      istatus = sys$trnlog('SYS$LOGIN',namelen,username,,,,)
      i = 1
      DO WHILE (uname(i) .NE. '[')
        i = i + 1
      END DO
      ileftbracket = i + 1
      DO WHILE (uname(i) .NE. ']')
        i = i + 1
      END DO
      irightbracket = i - 1
      prname = ' '
      lusnam = irightbracket-ileftbracket+1
      IFirst = 9 - lusnam/2
      prname(ifirst:ifirst+lusnam-1) =
     &                       username(ileftbracket:irightbracket)
      IF ( cli$present('comment') .EQ. %loc(cli$_present) ) THEN
        IF ( .NOT. cli$get_value('comment',comment, lcomm) ) THEN
          comment = ' '
          lcomm   = 1
        ENDIF
      ELSE
        TYPE 1000
 1000   FORMAT(' Enter comment for banner page : ',$)
        ACCEPT 1010,lcomm,comment
 1010   FORMAT(q,a)
      ENDIF
C
C ****  Printer print '_' as '^' so convert to '-', which is closer
C
      IF( lcomm .GT. 30 ) lcomm = 30
      DO i =  1, lcomm
        IF( comment(i:i) .EQ. '_' ) comment(i:i) = '-'
      ENDDO
C
C   turn QUIC on
C
      WRITE(the_unit,2000) '^PY^-'
C
C   turn printer simulation off, free format on
      WRITE(the_unit,2000) '^IWE^F^DAOC'
C
C   write legend on first page
C
      WRITE(the_unit,2000)'^IOP'
      status = sys$trnlog('SYS$CLUSTER_NODE', namelen, clus_name,,,,)
      Mach_type = 'Cluster'
      IF ( clus_name(1:namelen) .EQ. 'SYS$CLUSTER_NODE' ) THEN
        status = sys$trnlog( 'SYS$NODE', namelen, clus_name,,,,)
        Mach_type = 'System '
      ENDIF
      clus_name = clus_name(1:namelen-2)
      IF( clus_name(1:1) .EQ. '_' ) clus_name = clus_name(2:namelen-2)
      WRITE(the_unit,2000)'^IT00200^IJ00000^R'
      WRITE(the_unit,2000)'^M06003000500^KL06   '//clus_name(1:6)//
     &                    '   VAX   '//mach_type//'  ^R'
      WRITE(the_unit,2000)'^IT01000^O'
      WRITE(the_unit,2000)'^M08004001500'//PRNAME
      WRITE(the_unit,2000)'^M04002000600      '//DATESTR(1:20)
      WRITE(the_unit,2020)'^M06002501500^DL',(' ',i=1,(29-lcomm)/2),
     &                                  comment(1:lcomm)
      WRITE(the_unit,2000)'^IJ09000^IT00200'
      WRITE(the_unit,2000)'^M06003500000^KL06^R^DL'//
     &                    '  D0  SOURCES  Utility   '
      WRITE(THE_UNIT,2040)'^F^IC',qms_font_cpi,'00',
     &                      '^IL',qms_font_lpi,'00',
     &                      '^IS',qms_font_number,'^O'
      WRITE(THE_UNIT,2000)qms_font_orientation
      IF ( qms_font_orientation .eq. '^IOP' ) THEN
        WRITE(the_unit,2000)'^IB10800'       !10.8 inches bottom margin
      ENDIF
      WRITE(the_unit,2000)'^IJ00500'       !  .5 inches top margin
      WRITE(the_unit,2000)'^IT01000'       ! 1.0 inches left margin
      WRITE(the_unit,2000)'^,'             ! Make a page eject...
      WRITE(the_unit,2000)'^-^PN-'
 2000 FORMAT(1x,a)
 2020 FORMAT(1x,20a)
 2040 FORMAT(1X,A,I2.2,A,A,I2.2,A,A,I5.5,A)

  200 CONTINUE

      TYPE 10011
10011 FORMAT ('       ENTRY points ',40x,'File names     page'/
     &        1x,79('='))

      WRITE (the_unit,10010)
10010 FORMAT ('       ENTRY points ',<per_line-38>x,
     &        'File names    page')

      WRITE (the_unit,10020)
10020 FORMAT (1x,<per_line-1>('='))

      RETURN
      END

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      SUBROUTINE write_entry (page,name)

      IMPLICIT INTEGER(a-z)

      PARAMETER (first_entry = 3)
      CHARACTER*(*) name

      CHARACTER tab
      PARAMETER (tab = char(9))
      CHARACTER*132 line, fill_line, blank_line
      CHARACTER*70 line_screen
      DATA fill_line /
     &  '     ..........................................................
     &..................................................................
     &...'/
      DATA blank_line / ' ' /

      LOGICAL*1 end_flag

      COMMON /com1/  per_page, per_line, the_unit, read_unit

      DATA iline /first_entry/
      DATA end_flag /.false./

      IF (page .LT. 0) THEN
        page = -page
        end_flag = .true.
      ENDIF

      ilen = len (name)
      line = blank_line( 1:per_line-10-ilen )//name( 1:ilen )
      minc = per_line-79
      IF( minc .LE. 0 ) minc = 1
      line_screen = line(minc:per_line-10)

   10 CONTINUE
      i = index (line,tab)
      IF (i .GT. 0) THEN
        line(i:i) = ' '
        GOTO 10
      ENDIF
   11 CONTINUE
      i = index (line_screen,tab)
      IF (i .GT. 0) THEN
        line_screen(i:i) = ' '
        GOTO 11
      ENDIF

      TYPE 200, line_screen, page
  200 FORMAT( 1x, a,i8 )
      WRITE (the_unit,100), line(1:per_line - 9), page
  100 FORMAT( 1x, a, i8)

      iline = iline + 1
      IF (end_flag) THEN
        CALL fill_page( 0,iline,'Index')
        page = 1
      ELSE
        IF (iline .GT. per_page) THEN
          CALL write_page (0,'Index')
          CALL index_header( ' ' )
          iline = first_entry
        ENDIF
      ENDIF
      RETURN

      ENTRY write_module (page, name)
      ilen = len (name)
      line = '      '//name(1:ilen)//fill_line
      line_screen = line
      GOTO 10

      ENTRY write_dir( page, name )

      ilen = len( name )
      IF( iline+3 .GT. per_page ) THEN
        CALL write_page(0,'Index')
        CALL index_header(' ')
        iline = first_entry
      ENDIF
      TYPE 400, name(1:ilen)
      WRITE( the_unit, 400 ) name(1:ilen)
  400 FORMAT(/1x,a,' is now the current directory'/)
      iline = iline + 3
      RETURN

      ENTRY write_command( page, name )
      ilen = len( name )
      IF( iline+3 .GT. per_page ) THEN
        CALL write_page( 0,'Index')
        CALL index_header(' ')
        iline = first_entry
      ENDIF
      TYPE 300, name(1:ilen)
      WRITE( the_unit, 300 ) name(1:ilen)
  300 FORMAT(/20x,'Processing file specification --> ',a/)
      iline = iline + 3

      END

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      SUBROUTINE fill_page (page,iline, subtitle)
      IMPLICIT INTEGER(a-z)
      CHARACTER*(*)  subtitle
      COMMON /com1/  per_page, per_line, the_unit, read_unit

      IF (iline .LT. per_page) THEN
        DO i = iline,per_page
          WRITE (the_unit,1000)
 1000     FORMAT(1x)
        ENDDO
      ENDIF
      CALL write_page (page,subtitle)

      RETURN
      END

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      SUBROUTINE enable_ctrlc
      IMPLICIT INTEGER(a-z)

      INCLUDE '($iodef)'

      STRUCTURE /iostat_block/
        INTEGER*2 iostat
        BYTE      transmit,
     &          receive,
     &          crfill,
     &          lffill,
     &          parity,
     &          zero
      END STRUCTURE

      RECORD /iostat_block/ iosb

      EXTERNAL ctrlc_ast
      EXTERNAL ctrly_ast

      status = sys$assign ('sys$input',input_chan,,)
      IF (.NOT. status) RETURN

      status = sys$qiow (,
     &                   %val(input_chan),
     &                   %val(io$_setmode .OR. io$m_ctrlcast),
     &                   iosb,
     &                   ,,
     &                   ctrlc_ast,
     &                   ,,,,)

      IF (.NOT. status) RETURN
      status = iosb.iostat
      IF (.NOT. status) RETURN

      status = sys$qiow (,
     &                   %val(input_chan),
     &                   %val(io$_setmode .OR. io$m_ctrlyast),
     &                   iosb,
     &                   ,,
     &                   ctrly_ast,
     &                   ,,,,)

      IF (.NOT. status) RETURN
      status = iosb.iostat
      IF (.NOT. status) RETURN

      RETURN
      END
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      SUBROUTINE ctrlc_ast
      COMMON /errcom/ ierr
      ierr = 0
      END

      SUBROUTINE ctrly_ast
      COMMON /errcom/ ierr
      ierr = 0
      END

      SUBROUTINE sources_exit
      IMPLICIT INTEGER(a-z)
      COMMON /com1/  per_page, per_line, the_unit, read_unit
      COMMON /errcom/ ierr

      PARAMETER (max_error=6)
      CHARACTER*80 error_mess(max_error)
      DATA error_mess /
     *   ' input for sources procedure aborted ',
     *   ' error in creating spooled file ',
     *   ' error in opening indirect input file ',
     *   ' error in reading indirect input file ',
     *   ' impossible to open input file ',
     *   ' error in reading input file '/

      IF ( ierr .GT. 0 .AND. ierr .LE. max_error ) THEN
        error_len = len (error_mess(ierr))
        WRITE (6,'(1x,a)') error_mess(ierr)(1:error_len)

        CLOSE (unit=the_unit, err=200, dispose='delete')

  200   CONTINUE
      ENDIF
 1000 CONTINUE
      END
      LOGICAL*1 FUNCTION file_date (file_name, date)
      IMPLICIT NONE
      INTEGER file_name(2)
      CHARACTER*(*) date

      INCLUDE '($fabdef)'
      INCLUDE '($rabdef)'
      INCLUDE '($xabdef)'
      INCLUDE '($xabdatdef)'
      RECORD /fabdef/ fab
      RECORD /rabdef/ rab
      RECORD /xabdef/ xab

      INTEGER length
      INTEGER status
      INTEGER sys$open
      INTEGER sys$close
      INTEGER sys$asctim

      file_date = .false.
      date      = ' '

      fab.fab$b_bid = fab$c_bid
      fab.fab$b_bln = fab$c_bln

      fab.fab$l_fna = file_name(2)
      fab.fab$b_fns = file_name(1) .AND. 255

      xab.xab$b_cod = xab$c_dat
      xab.xab$b_bln = xab$c_datlen
      fab.fab$l_xab = %loc (xab)

      status = sys$open (fab)
      IF (status) THEN
        CALL sys$asctim (length, date, xab.xab$q_rdt,)
        status = sys$close (fab)
        file_date = .true.
      ENDIF
      RETURN
      END
