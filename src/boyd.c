// Copyright (C) 2021 Robert Coffey
// Released under the GPLv2 license

#include <stdio.h>
#include <stdlib.h>

#include "boyd.h"

boyd_t *boyd_init(void)
{
	boyd_t *boyd = malloc(sizeof boyd);
	if (!boyd) {
		printf("Error: boyd_init: malloc failed\n");
		exit(1);
	}
	return boyd;
}

void boyd_destroy(boyd_t *boyd)
{
	free(boyd);
}
