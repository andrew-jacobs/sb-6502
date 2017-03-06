
// #include <unistd.h>
#include <stdio.h>


int getopt (int argc, char **argv, char *options)
{
	return (-1);
}


void copy (FILE *pFile)
{
	
}



int main (int argc, char **argv)
{
	int		option;
	
	while ((option = getopt (argc, argv, "bntv")) != -1) {
		switch (option) {
		case 'b':
			break;
		case 'n':
			break;
		case 't':
			break;
		case 'v':
			break;
			
		default:
			fprintf (stderr, "Usage: cat [-bntv] [file ...]\n");
			exit (1);
		}
	}
	
	
	
}