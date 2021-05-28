# Copyright (C) 2021 Robert Coffey
# Released under the GPLv2 license

EXEC := boids

SRC_DIR := src
OBJ_DIR := obj

HEDS := $(wildcard $(SRC_DIR)/*.h)
SRCS := $(wildcard $(SRC_DIR)/*.c)
OBJS := $(SRCS:$(SRC_DIR)/%.c=$(OBJ_DIR)/%.o)

CC := gcc
CFLAGS := -Wall -Wextra -Wpedantic
LIBS := -lm

SDL_CONFIG := `sdl2-config --cflags --libs` -lSDL2_image

all: $(EXEC)

$(EXEC): $(OBJS)
	$(CC) -o $@ $^ $(SDL_CONFIG) $(LIBS)

$(OBJS): $(HEDS)

$(OBJ_DIR)/%.o: $(SRC_DIR)/%.c | $(OBJ_DIR)
	$(CC) -o $@ $(CFLAGS) -c $< $(SDL_CONFIG)

$(OBJ_DIR):
	@mkdir -p $@

.PHONY: clean
clean:
	@rm -frv $(OBJ_DIR)

.PHONY: remove
remove:
	@rm -frv $(OBJ_DIR) $(EXEC)
