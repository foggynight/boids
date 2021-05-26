// Copyright (C) 2021 Robert Coffey
// Released under the GPLv2 license

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "boid.h"
#include "setup.h"

#define MAX_LINE_LENGTH	128	// Max length of a line of boid parameters

void setup_boid_arr(FILE *setup_file)
{
	size_t line_count;
	char line[MAX_LINE_LENGTH+1];

	for (line_count = 0; line_count < MAX_BOID_COUNT; ++line_count) {
		if (fscanf(setup_file, "%s", line) == EOF)
			break;

		boid_arr[line_count].x = atof(strtok(line, ","));
		boid_arr[line_count].y = atof(strtok(NULL, ","));
		boid_arr[line_count].angle = atof(strtok(NULL, ","));
		boid_arr[line_count].velocity = atof(strtok(NULL, "\n"));
	}

	boid_count = line_count;
}
