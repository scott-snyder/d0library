#include <unistd.h>
sleep_time_(int sec)
{
	uint islept;
	islept = sleep ( sec );
	return(islept);
}
