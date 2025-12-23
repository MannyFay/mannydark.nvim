# ==============================================================================
# Comprehensive Makefile Sample - Syntax Highlighting Demonstration
# ==============================================================================

# This file demonstrates all major Makefile language features
# for syntax highlighting purposes.

# ==============================================================================
# Comments
# ==============================================================================

# Single line comment

# Multiple
# line
# comments

# ==============================================================================
# Variables
# ==============================================================================

# Simple assignment (recursive)
CC = gcc
CXX = g++
AR = ar

# Immediate assignment
SRCS := $(wildcard src/*.c)
OBJS := $(SRCS:.c=.o)

# Conditional assignment (only if not already set)
PREFIX ?= /usr/local
DESTDIR ?=

# Append assignment
CFLAGS += -Wall -Wextra
LDFLAGS += -L$(PREFIX)/lib

# Shell assignment
DATE != date +%Y-%m-%d
VERSION := $(shell cat VERSION 2>/dev/null || echo "0.0.1")
GIT_HASH := $(shell git rev-parse --short HEAD 2>/dev/null)

# Multi-line variables
define HELP_MESSAGE
Usage: make [target]

Available targets:
  all       - Build everything
  clean     - Remove build artifacts
  install   - Install to PREFIX
  test      - Run tests
  help      - Show this help
endef

define MULTILINE_SCRIPT
	@echo "Step 1"
	@echo "Step 2"
	@echo "Step 3"
endef

# Empty variable
EMPTY :=

# Variable with newline
NEWLINE := $(EMPTY)
NEWLINE += $(EMPTY)

# ==============================================================================
# Automatic Variables
# ==============================================================================

# $@ - Target name
# $< - First prerequisite
# $^ - All prerequisites (deduplicated)
# $+ - All prerequisites (with duplicates)
# $* - Stem (matched by %)
# $? - Prerequisites newer than target
# $| - Order-only prerequisites
# $(@D) - Directory part of target
# $(@F) - File part of target
# $(<D), $(<F), etc.

# ==============================================================================
# Built-in Variables
# ==============================================================================

# Compiler and tools
CC = gcc
CXX = g++
CPP = $(CC) -E
AS = as
AR = ar
LD = ld
MAKE = make
RM = rm -f
MKDIR = mkdir -p
INSTALL = install

# Compiler flags
CFLAGS = -O2 -g -Wall -Wextra -Werror -pedantic
CXXFLAGS = -O2 -g -Wall -Wextra -std=c++17
CPPFLAGS = -I$(INCLUDE_DIR) -DVERSION=\"$(VERSION)\"
LDFLAGS = -L$(LIB_DIR)
LDLIBS = -lm -lpthread
ARFLAGS = rcs

# ==============================================================================
# Directory Variables
# ==============================================================================

SRC_DIR := src
OBJ_DIR := build/obj
BIN_DIR := build/bin
LIB_DIR := build/lib
INCLUDE_DIR := include
TEST_DIR := tests
DOC_DIR := docs

# Install directories
bindir := $(PREFIX)/bin
libdir := $(PREFIX)/lib
includedir := $(PREFIX)/include
datadir := $(PREFIX)/share

# ==============================================================================
# File Lists
# ==============================================================================

# Source files
SOURCES := $(wildcard $(SRC_DIR)/*.c)
SOURCES += $(wildcard $(SRC_DIR)/**/*.c)
HEADERS := $(wildcard $(INCLUDE_DIR)/*.h)

# Object files
OBJECTS := $(SOURCES:$(SRC_DIR)/%.c=$(OBJ_DIR)/%.o)

# Dependency files
DEPS := $(OBJECTS:.o=.d)

# Test files
TEST_SOURCES := $(wildcard $(TEST_DIR)/*.c)
TEST_OBJECTS := $(TEST_SOURCES:$(TEST_DIR)/%.c=$(OBJ_DIR)/test/%.o)

# Target names
TARGET := myprogram
LIBRARY := libmylib.a
SHARED_LIB := libmylib.so
TEST_TARGET := run_tests

# ==============================================================================
# Phony Targets
# ==============================================================================

.PHONY: all clean install uninstall test check dist docs help
.PHONY: debug release sanitize coverage
.PHONY: docker-build docker-run docker-push
.PHONY: lint format tidy

# ==============================================================================
# Default Target
# ==============================================================================

all: $(BIN_DIR)/$(TARGET) $(LIB_DIR)/$(LIBRARY)

# ==============================================================================
# Pattern Rules
# ==============================================================================

# Compile C sources to objects
$(OBJ_DIR)/%.o: $(SRC_DIR)/%.c | $(OBJ_DIR)
	@$(MKDIR) $(dir $@)
	$(CC) $(CFLAGS) $(CPPFLAGS) -MMD -MP -c $< -o $@

# Compile C++ sources to objects
$(OBJ_DIR)/%.o: $(SRC_DIR)/%.cpp | $(OBJ_DIR)
	@$(MKDIR) $(dir $@)
	$(CXX) $(CXXFLAGS) $(CPPFLAGS) -MMD -MP -c $< -o $@

# Compile test sources
$(OBJ_DIR)/test/%.o: $(TEST_DIR)/%.c | $(OBJ_DIR)/test
	$(CC) $(CFLAGS) $(CPPFLAGS) -I$(SRC_DIR) -c $< -o $@

# ==============================================================================
# Build Targets
# ==============================================================================

# Main executable
$(BIN_DIR)/$(TARGET): $(OBJECTS) | $(BIN_DIR)
	$(CC) $(LDFLAGS) $^ $(LDLIBS) -o $@

# Static library
$(LIB_DIR)/$(LIBRARY): $(OBJECTS) | $(LIB_DIR)
	$(AR) $(ARFLAGS) $@ $^

# Shared library
$(LIB_DIR)/$(SHARED_LIB): $(OBJECTS) | $(LIB_DIR)
	$(CC) -shared $(LDFLAGS) $^ $(LDLIBS) -o $@

# Test executable
$(BIN_DIR)/$(TEST_TARGET): $(TEST_OBJECTS) $(filter-out %main.o,$(OBJECTS)) | $(BIN_DIR)
	$(CC) $(LDFLAGS) $^ $(LDLIBS) -o $@

# ==============================================================================
# Directory Creation
# ==============================================================================

$(OBJ_DIR) $(BIN_DIR) $(LIB_DIR) $(OBJ_DIR)/test:
	$(MKDIR) $@

# ==============================================================================
# Build Variants
# ==============================================================================

debug: CFLAGS += -DDEBUG -O0 -g3 -fsanitize=address
debug: LDFLAGS += -fsanitize=address
debug: all

release: CFLAGS += -DNDEBUG -O3 -flto
release: LDFLAGS += -flto
release: all

sanitize: CFLAGS += -fsanitize=address,undefined
sanitize: LDFLAGS += -fsanitize=address,undefined
sanitize: all

coverage: CFLAGS += --coverage -fprofile-arcs -ftest-coverage
coverage: LDFLAGS += --coverage
coverage: all

# ==============================================================================
# Test Target
# ==============================================================================

test: $(BIN_DIR)/$(TEST_TARGET)
	@echo "Running tests..."
	@$(BIN_DIR)/$(TEST_TARGET)

check: test

# ==============================================================================
# Clean Targets
# ==============================================================================

clean:
	$(RM) -r $(OBJ_DIR) $(BIN_DIR) $(LIB_DIR)
	$(RM) *.gcov *.gcda *.gcno
	$(RM) -r coverage_report

distclean: clean
	$(RM) -r autom4te.cache config.log config.status
	$(RM) Makefile config.h

mostlyclean:
	$(RM) $(OBJECTS) $(DEPS)

# ==============================================================================
# Install Targets
# ==============================================================================

install: all
	$(INSTALL) -d $(DESTDIR)$(bindir)
	$(INSTALL) -m 755 $(BIN_DIR)/$(TARGET) $(DESTDIR)$(bindir)/
	$(INSTALL) -d $(DESTDIR)$(libdir)
	$(INSTALL) -m 644 $(LIB_DIR)/$(LIBRARY) $(DESTDIR)$(libdir)/
	$(INSTALL) -d $(DESTDIR)$(includedir)
	$(INSTALL) -m 644 $(HEADERS) $(DESTDIR)$(includedir)/

uninstall:
	$(RM) $(DESTDIR)$(bindir)/$(TARGET)
	$(RM) $(DESTDIR)$(libdir)/$(LIBRARY)
	$(RM) $(addprefix $(DESTDIR)$(includedir)/,$(notdir $(HEADERS)))

install-strip: install
	strip $(DESTDIR)$(bindir)/$(TARGET)

# ==============================================================================
# Distribution Target
# ==============================================================================

DIST_NAME := $(TARGET)-$(VERSION)
DIST_FILES := $(SOURCES) $(HEADERS) Makefile README.md LICENSE

dist: $(DIST_NAME).tar.gz

$(DIST_NAME).tar.gz: $(DIST_FILES)
	$(MKDIR) $(DIST_NAME)
	cp -r $(DIST_FILES) $(DIST_NAME)/
	tar czf $@ $(DIST_NAME)
	$(RM) -r $(DIST_NAME)

# ==============================================================================
# Documentation
# ==============================================================================

docs:
	doxygen Doxyfile

docs-clean:
	$(RM) -r $(DOC_DIR)/html $(DOC_DIR)/latex

# ==============================================================================
# Code Quality
# ==============================================================================

lint:
	cppcheck --enable=all --inconclusive $(SRC_DIR)
	cpplint $(SOURCES) $(HEADERS)

format:
	clang-format -i $(SOURCES) $(HEADERS)

tidy:
	clang-tidy $(SOURCES) -- $(CFLAGS) $(CPPFLAGS)

# ==============================================================================
# Docker Targets
# ==============================================================================

DOCKER_IMAGE := myproject
DOCKER_TAG := $(VERSION)

docker-build:
	docker build -t $(DOCKER_IMAGE):$(DOCKER_TAG) .
	docker tag $(DOCKER_IMAGE):$(DOCKER_TAG) $(DOCKER_IMAGE):latest

docker-run:
	docker run --rm -it $(DOCKER_IMAGE):$(DOCKER_TAG)

docker-push:
	docker push $(DOCKER_IMAGE):$(DOCKER_TAG)
	docker push $(DOCKER_IMAGE):latest

docker-clean:
	docker rmi $(DOCKER_IMAGE):$(DOCKER_TAG) $(DOCKER_IMAGE):latest

# ==============================================================================
# Help Target
# ==============================================================================

help:
	$(info $(HELP_MESSAGE))
	@true

# Or using here-doc style:
help2:
	@echo "Usage: make [target]"
	@echo ""
	@echo "Targets:"
	@echo "  all        - Build everything (default)"
	@echo "  clean      - Remove build artifacts"
	@echo "  install    - Install to PREFIX ($(PREFIX))"
	@echo "  test       - Run unit tests"
	@echo "  help       - Show this help message"

# ==============================================================================
# Conditional Logic
# ==============================================================================

# Check for required tools
ifeq ($(shell which $(CC) 2>/dev/null),)
$(error $(CC) not found. Please install GCC.)
endif

# Platform-specific settings
UNAME_S := $(shell uname -s)

ifeq ($(UNAME_S),Linux)
    LDLIBS += -lrt
    SHARED_LIB := libmylib.so
endif

ifeq ($(UNAME_S),Darwin)
    LDLIBS += -framework CoreFoundation
    SHARED_LIB := libmylib.dylib
endif

ifdef DEBUG
    CFLAGS += -DDEBUG -g
else
    CFLAGS += -DNDEBUG -O2
endif

ifndef VERBOSE
    QUIET := @
endif

ifneq ($(V),1)
.SILENT:
endif

# ==============================================================================
# Functions
# ==============================================================================

# Custom function
define compile-rule
$(2): $(1)
	$$(CC) $$(CFLAGS) -c $$< -o $$@
endef

# Call the function
$(eval $(call compile-rule,src/main.c,build/main.o))

# Built-in functions
UPPERCASE := $(shell echo "hello" | tr a-z A-Z)
LOWERCASE := $(shell echo "HELLO" | tr A-Z a-z)

# String functions
WORDS := one two three
FIRST_WORD := $(firstword $(WORDS))
LAST_WORD := $(lastword $(WORDS))
WORD_COUNT := $(words $(WORDS))
NTH_WORD := $(word 2,$(WORDS))
WORDLIST := $(wordlist 1,2,$(WORDS))

# File functions
BASENAME := $(basename src/file.c)
SUFFIX := $(suffix src/file.c)
DIR := $(dir src/subdir/file.c)
NOTDIR := $(notdir src/subdir/file.c)
ADDPREFIX := $(addprefix build/,$(notdir $(SOURCES)))
ADDSUFFIX := $(addsuffix .bak,$(SOURCES))
JOIN := $(join a b c,1 2 3)
REALPATH := $(realpath .)
ABSPATH := $(abspath ../relative/path)

# Filter functions
C_FILES := $(filter %.c,$(SOURCES))
NOT_MAIN := $(filter-out %main.c,$(SOURCES))
SORTED := $(sort z a m b)
PATSUBST := $(patsubst %.c,%.o,$(SOURCES))
STRIP := $(strip   hello   )
SUBST := $(subst ee,EE,feet on the street)

# Control functions
RESULT := $(if $(DEBUG),debug mode,release mode)
VALUE := $(or $(CUSTOM_VAR),$(DEFAULT_VAR),fallback)
CHECK := $(and $(CC),$(CXX),both compilers found)
FOREACH := $(foreach dir,$(SRC_DIR) $(TEST_DIR),$(wildcard $(dir)/*.c))
ERROR_MSG := $(error This is an error message)
WARNING_MSG := $(warning This is a warning message)
INFO_MSG := $(info This is an info message)

# Eval and value
define generated-rule
$(1).o: $(1).c
	$$(CC) -c $$< -o $$@
endef

$(foreach src,$(basename $(SOURCES)),$(eval $(call generated-rule,$(src))))

# ==============================================================================
# Static Pattern Rules
# ==============================================================================

$(OBJECTS): $(OBJ_DIR)/%.o: $(SRC_DIR)/%.c
	$(MKDIR) $(dir $@)
	$(CC) $(CFLAGS) $(CPPFLAGS) -c $< -o $@

# ==============================================================================
# Secondary Expansion
# ==============================================================================

.SECONDEXPANSION:

$(BIN_DIR)/%: $$(filter $$*/%.o,$$(OBJECTS))
	$(CC) $(LDFLAGS) $^ $(LDLIBS) -o $@

# ==============================================================================
# Special Targets
# ==============================================================================

.DEFAULT_GOAL := all

.SUFFIXES:
.SUFFIXES: .c .cpp .o .h .a .so

.PRECIOUS: $(OBJ_DIR)/%.o

.INTERMEDIATE: generated_file.c

.SECONDARY:

.DELETE_ON_ERROR:

.IGNORE: clean

.SILENT: help

.EXPORT_ALL_VARIABLES:

.NOTPARALLEL: install

.ONESHELL:

# ==============================================================================
# Include Dependencies
# ==============================================================================

-include $(DEPS)

# Include optional local configuration
-include local.mk
-include config.mk

# ==============================================================================
# Recipe Examples
# ==============================================================================

# Multi-line recipe
complex-target:
	@echo "Starting complex build..."
	@$(MKDIR) output
	$(CC) -c src/file1.c -o output/file1.o
	$(CC) -c src/file2.c -o output/file2.o
	$(CC) output/file1.o output/file2.o -o output/program
	@echo "Build complete!"

# Recipe with shell one-liner
check-env:
	@if [ -z "$(API_KEY)" ]; then \
		echo "Error: API_KEY not set"; \
		exit 1; \
	fi

# Recipe with continuation
long-command:
	$(CC) \
		-I$(INCLUDE_DIR) \
		-L$(LIB_DIR) \
		-DVERSION=\"$(VERSION)\" \
		-o $@ $^

# ==============================================================================
# End of Makefile
# ==============================================================================
