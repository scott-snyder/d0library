c  Tkr_util.inc
      INTEGER maxcmd        
      PARAMETER( maxcmd = 132 )
      INTEGER bufmax
      INTEGER tkr_chan,buflen,bufout,ibuf
      CHARACTER*80 MSG_STRING
      INTEGER ibufmax
      PARAMETER( ibufmax = 16384 )
      COMMON /tkr_util/tkr_chan,msg_string,buflen,bufout,
     &  ibuf(ibufmax)
