# Copyright (C) 2021 Robert Coffey
# Released under the GPLv2 license

EXEC := boids

SRC_DIR := src
OBJ_DIR := obj

SRCS := $(wildcard $(SRC_DIR)/*.c)
OBJS := $(SRCS:$(SRC_DIR)/%.c=$(OBJ_DIR)/%.o)

CC := gcc
CFLAGS := -Wall -Wextra -Wpedantic

SDL_CONFIG := `sdl2-config --cflags --libs`

all: $(EXEC)

$(EXEC): $(OBJS)
	$(CC) $^ $(SDL_CONFIG) -o $@

$(OBJ_DIR)/%.o: $(SRC_DIR)/%.c | $(OBJ_DIR)
	$(CC) $(CFLAGS) -c $< $(SDL_CONFIG) -o $@

$(OBJ_DIR):
	@mkdir -p $@

.PHONY: clean
clean:
	@rm -rv $(OBJ_DIR) $(EXEC)
