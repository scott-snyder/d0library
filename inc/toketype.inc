* Included file TOKETYPE.INC - contains mapping of automata states to
* token types.
*
*   Valid token types:
*
*        0  Not a token (error condition)
*        1  identifier
*        2  token equality operator
*        3  token less than operator
*        4  token greater than operator
*        5  token less than or equal to
*        6  token greater than or equal to
*        7  token not equal to
*        8  token real constant
*        9  token integer constant
*       10  token delimeter found ("&", "|", "~")
*       11  invalid character found
*       12  error "." in middle of an identifier
*       13  end of string
*
*     Author: Edward Gardella (meandering around in 1986-7)
*     Modifications:
*
      INTEGER DELIM, EOS, EQ, GE, GT, IDENT, INTCON, INVCH, INVPER, LE
      INTEGER LT, MAXTOK, NE, NOTTOK, REALCN
                             
* Number of last valid token
      PARAMETER (MAXTOK = 13)

* Token identifiers
      PARAMETER ( NOTTOK = 0)
      PARAMETER ( IDENT =  1)
      PARAMETER ( EQ = 2 )
      PARAMETER ( LT = 3 )
      PARAMETER ( GT = 4 )
      PARAMETER ( LE = 5 )
      PARAMETER ( GE = 6 )
      PARAMETER ( NE = 7 )
      PARAMETER ( REALCN =  8 )
      PARAMETER ( INTCON =  9 )
      PARAMETER ( DELIM  = 10 )
      PARAMETER ( INVCH =  11 )
      PARAMETER ( INVPER = 12 )
      PARAMETER ( EOS = 13 )
*
