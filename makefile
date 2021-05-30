# Copyright (C) 2021 Robert Coffey
# Released under the GPLv2 license

EXEC := boids

SRC_DIR := src
OBJ_DIR := obj

HEDS := $(wildcard $(SRC_DIR)/*.hpp)
SRCS := $(wildcard $(SRC_DIR)/*.cpp)
OBJS := $(SRCS:$(SRC_DIR)/%.cpp=$(OBJ_DIR)/%.o)

CC := g++
CFLAGS := -Wall -Wextra -Wpedantic
LIBS := -lm

SDL_CONFIG := `sdl2-config --cflags --libs` -lSDL2_image

all: $(EXEC)

$(EXEC): $(OBJS)
	$(CC) -o $@ $^ $(LIBS) $(SDL_CONFIG)

$(OBJS): $(HEDS)

$(OBJ_DIR)/%.o: $(SRC_DIR)/%.cpp | $(OBJ_DIR)
	$(CC) -o $@ $(CFLAGS) -c $<

$(OBJ_DIR):
	@mkdir -p $@

.PHONY: clean
clean:
	@rm -frv $(OBJ_DIR)

.PHONY: remove
remove:
	@rm -frv $(OBJ_DIR) $(EXEC)
