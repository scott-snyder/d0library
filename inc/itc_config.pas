  (***************************************************************************
  *									     *
  *	Configuration parameters for the ITC system.			     *
  *									     *
  ***************************************************************************)


  (***************************************************************************
  *	Number of active channels, channels are re-used			     *
  ***************************************************************************)
	  Number_of_Channels	= 40;

  (***************************************************************************
  *	Messages longer than max will be truncated			     *
  ***************************************************************************)
	  Max_Message_Size	= 256;		{ characters or bytes }

  (***************************************************************************
  *	Max Mess Size for target mailbox (must be able to hold an NCB)	     *
  ***************************************************************************)
	  NCB_Size		= 128;		{ characters or bytes }

  (***************************************************************************
  *	Held to 16 in order to fit into optdata area of NCB           	     *
  ***************************************************************************)
	  MaxLen_TargNam	= 16;		{ characters }

  (***************************************************************************
  *	Ring buffer is used to record activity (connect, disconnect, mess)   *
  ***************************************************************************)
	  Ring_Buff_Size	= 1024;

  (***************************************************************************
  *	Time allowed for target to create comm mailboxes		     *
  ***************************************************************************)
	  Conn_Wait_Time	= '0 ::05.00';	{ 5 seconds }

  (***************************************************************************
  *	Event Flag, reserved via LIB$Reserve_EF during initialization	     *
  ***************************************************************************)
	  ITC_EFN	= 37;		{ This is in the local EF cluster }

  (***************************************************************************
  *	Timer ID, used to timeout connects				     *
  ***************************************************************************)
	  ITC_TimerID	= 7;		{ Arbitrary nonzero value }

  (***************************************************************************
  *	Parameter signifies no more channels available			     *
  ***************************************************************************)
	  None	= 0;