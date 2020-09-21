BINARY_NAME=keybounce-detect

OBJS=keybounce-detect.o

CFLAGS=-g -MMD -Wall -Werror
LDFLAGS=
# Add any libraries used here
LDLIBS=

$(BINARY_NAME): $(OBJS)

clean:
	rm -f $(OBJS) $(OBJS:%.o=%.d) $(BINARY_NAME)

.PHONY: clean

-include *.d
