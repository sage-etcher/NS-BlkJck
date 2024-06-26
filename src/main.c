/* Basic Standard Libs */
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

/* Heap Allocations Libs */
#include <malloc.h>
#include <string.h>

/* SEI CERT C Libs */
#include <stdint.h>
#include <inttypes.h>
#include <limits.h>
#include <errno.h>

#define EMPTY_STRING ""


    int 
main (int argc, char **argv)
{
    const char *filename = ((argc >= 1 && argv[1]) ? argv[1] : EMPTY_STRING);
    if (0 == strcmp (filename, EMPTY_STRING))
    {   
        (void)fprintf (stderr, "NO FILE\n");
        exit (EXIT_FAILURE);
    }

    

    exit (EXIT_SUCCESS);
}

