*
* Included file: PSTACK.INC - contains definition for parser stack array for the
*                RZ query language.
*
      INTEGER PSTACK(1:110, 1:3)
*
* PSTACK(n, PSTATE) gives the state for position n on the stack
* PSTACK(n, TYPE)  gives the token type of position n on the stack
* PSTACK(n, VALUE) gives the token value of position n on the stack
*
      INTEGER TYPE, VALUE, PSTATE
*
      PARAMETER ( PSTATE = 1)
      PARAMETER ( TYPE = 2)
      PARAMETER ( VALUE = 3)
*
* TOPSTK points to the top of the stack
* CSTATE is the current parser state
*
      INTEGER TOPSTK, CSTATE
*
