      logical function crt

      include    'd0$inc:swing.inc'
      include    '($dvidef)'
      include    '($ttdef)'
      include    '($tt2def)'

      integer*2  b2(14)
      integer*4  b4(7), buf, len_buf, sys$trnlog, sys$getdviw, dev_type,
     -           len_dev_type
      logical*4  for$bjtest, istat

      equivalence ( b4(1), b2(1) )

      b2(1) = 4
      b2(2) = dvi$_devdepend2
      b4(2) = %loc( buf )
      b4(3) = %loc( len_buf )

      b2(7) = 4
      b2(8) = dvi$_devtype
      b4(5) = %loc( dev_type )
      b4(6) = %loc( len_dev_type )

      b4(7) = 0

      istat = sys$getdviw( ,, 'SYS$COMMAND', b4,,,, )

      crt      = ( for$bjtest( buf, tt2$v_deccrt ) .or.
     .                            dev_type .eq. tt$_vt52 )
      avo      = for$bjtest( buf, tt2$v_avo )

      return
      end
