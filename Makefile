CROSS_COMPILE ?= mipsel-linux-
PREFIX ?= /usr/local

VERSION_MAJOR = 0
VERSION_MINOR = 1

LIBNAME = liblightrec.so
SONAME = $(LIBNAME).$(VERSION_MAJOR)
LIBLIGHTREC = $(SONAME).$(VERSION_MINOR)

CC = $(CROSS_COMPILE)cc
ANALYZER = clang --analyze
INSTALL ?= install

CFLAGS = -Wall -fPIC
LDLIBS = -llightning -lopcodes

OBJS = blockcache.o disassembler.o emitter.o lightrec.o optimizer.o regcache.o recompiler.o

.PHONY: all analyze clean

all: $(LIBLIGHTREC)

$(LIBLIGHTREC): $(OBJS)
	$(CC) -shared -Wl,-soname,$(SONAME) $(CFLAGS) $(LDFLAGS) $^ $(LDLIBS) -o $@

clean:
	rm -f $(OBJS) $(LIBLIGHTREC)

analyze:
	$(ANALYZER) $(CFLAGS) $(OBJS:%.o=%.c)
